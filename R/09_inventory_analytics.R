library(tidyverse)
library(lubridate)
library(scales)
set.seed(42)

sales     <- readRDS("data/processed/sales_store.rds")
inventory <- readRDS("data/processed/inventory.rds") |>
  mutate(product_id = as.character(product_id), store_id = as.character(store_id))
products  <- readRDS("data/processed/products.rds") |>
  mutate(product_id = as.character(product_id))
suppliers <- readRDS("data/processed/suppliers.rds")
stores    <- readRDS("data/processed/stores.rds")

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 150)

# ---- Settings (document these in your report) ----
window_days   <- 90     # demand window
service_z     <- 1.65   # 95% service level
review_days   <- 14     # order cycle used for target stock
slow_days     <- 30     # no sale for 30+ days = slow-moving
dead_days     <- 90     # no sale for 90+ days = dead stock
overstock_days <- 90    # stock covers more than 90 days = overstocked
low_days      <- 7      # stock covers under 7 days = low stock

ref_date <- max(sales$invoice_day)
days     <- seq(ref_date - window_days + 1, ref_date, by = "day")

# ---- 1. Average daily demand and variability per product-store ----
daily <- sales |>
  filter(invoice_day %in% days, stock_code %in% inventory$product_id) |>
  group_by(store_id, product_id = stock_code, invoice_day) |>
  summarise(units = sum(quantity), .groups = "drop")

demand <- inventory |> distinct(store_id, product_id) |>
  crossing(invoice_day = days) |>
  left_join(daily, by = c("store_id", "product_id", "invoice_day")) |>
  mutate(units = replace_na(units, 0)) |>
  group_by(store_id, product_id) |>
  summarise(avg_daily_demand = mean(units), sd_daily = sd(units),
            .groups = "drop")

last_sale <- sales |>
  filter(stock_code %in% inventory$product_id) |>
  group_by(store_id, product_id = stock_code) |>
  summarise(last_sale = max(invoice_day), .groups = "drop")

# ---- 2. Join everything; rebuild SIMULATED stock to a realistic scale ----
inv <- inventory |>
  select(store_id, product_id) |>
  left_join(demand, by = c("store_id", "product_id")) |>
  left_join(last_sale, by = c("store_id", "product_id")) |>
  left_join(products, by = "product_id") |>
  left_join(select(suppliers, supplier_id, supplier_name, lead_time),
            by = "supplier_id") |>
  left_join(select(stores, store_id, store_name, district), by = "store_id") |>
  mutate(days_since_sale = as.numeric(ref_date - last_sale),
         days_since_sale = replace_na(days_since_sale, 999),
         stock_quantity  = round(avg_daily_demand * runif(n(), 2, 120)),
         stock_quantity  = if_else(runif(n()) < 0.03, 0, stock_quantity))

# ---- 3. Metrics ----
inv <- inv |>
  mutate(
    inventory_value  = stock_quantity * cost_price,
    days_remaining   = if_else(avg_daily_demand > 0,
                               stock_quantity / avg_daily_demand, Inf),
    stockout_date    = if_else(is.finite(days_remaining),
                               ref_date + ceiling(days_remaining), as.Date(NA)),
    safety_stock     = ceiling(service_z * sd_daily * sqrt(lead_time)),
    reorder_point    = ceiling(avg_daily_demand * lead_time + safety_stock),
    target_stock     = ceiling(avg_daily_demand * (lead_time + review_days) +
                                 safety_stock),
    reorder_qty      = pmax(0, target_stock - stock_quantity),
    annual_turnover  = if_else(stock_quantity > 0,
                               avg_daily_demand * 365 / stock_quantity, NA_real_),
    demand_cv        = if_else(avg_daily_demand > 0, sd_daily / avg_daily_demand, 0))

# ---- 4. Business rules ----
inv <- inv |>
  mutate(
    critical_stockout = stock_quantity == 0 & avg_daily_demand > 0,
    replenish         = stock_quantity < reorder_point & stock_quantity > 0,
    low_stock         = is.finite(days_remaining) & days_remaining < low_days,
    overstocked       = is.finite(days_remaining) & days_remaining > overstock_days,
    dead_stock        = stock_quantity > 0 & days_since_sale >= dead_days,
    slow_moving       = stock_quantity > 0 & days_since_sale >= slow_days &
      days_since_sale < dead_days,
    status = case_when(
      critical_stockout ~ "Critical stockout",
      replenish         ~ "Replenish",
      dead_stock        ~ "Dead stock",
      slow_moving       ~ "Slow-moving",
      overstocked       ~ "Overstocked",
      TRUE              ~ "OK"))

# ---- 5. Stock-risk score (0-100) ----
inv <- inv |>
  mutate(
    cover_ratio = if_else(is.finite(days_remaining), days_remaining / lead_time, 99),
    risk_cover  = pmin(1, pmax(0, (2 - cover_ratio) / 2)),
    risk_lead   = percent_rank(lead_time),
    risk_var    = percent_rank(demand_cv),
    risk_score  = round(100 * (0.6 * risk_cover + 0.2 * risk_lead + 0.2 * risk_var)),
    risk_score  = if_else(critical_stockout, pmax(risk_score, 90), risk_score),
    priority    = case_when(risk_score >= 70 ~ "Critical",
                            risk_score >= 50 ~ "High",
                            risk_score >= 30 ~ "Medium", TRUE ~ "Low"))

# ---- 6. Headline KPIs ----
kpi <- tibble(
  metric = c("Current inventory value (GBP, simulated stock)",
             "Product-store combinations", "Critical stockouts",
             "Replenishment needed", "Low stock (under 7 days)",
             "Overstocked (over 90 days)", "Slow-moving", "Dead stock",
             "Overall inventory turnover (times per year)",
             "Average days of inventory remaining (median)"),
  value = c(round(sum(inv$inventory_value, na.rm = TRUE)), nrow(inv),
            sum(inv$critical_stockout), sum(inv$replenish), sum(inv$low_stock),
            sum(inv$overstocked), sum(inv$slow_moving), sum(inv$dead_stock),
            round(sum(inv$avg_daily_demand * 365 * inv$cost_price, na.rm = TRUE) /
                    sum(inv$inventory_value, na.rm = TRUE), 1),
            round(median(inv$days_remaining[is.finite(inv$days_remaining)]), 1)))
print(kpi)
print(count(inv, status))
write_csv(kpi, "reports/inventory_kpis.csv")

# ---- 7. Outputs ----
reorder <- inv |>
  filter(status %in% c("Critical stockout", "Replenish"), reorder_qty > 0) |>
  mutate(recommendation = paste0(
    "Order ", reorder_qty, " units of ", product_name, " for ", store_name,
    ". Stock lasts ", round(days_remaining, 1), " days; supplier lead time is ",
    lead_time, " days.")) |>
  arrange(desc(risk_score)) |>
  select(priority, risk_score, store_name, district, product_id, product_name,
         status, stock_quantity, avg_daily_demand, days_remaining, lead_time,
         reorder_point, reorder_qty, stockout_date, supplier_name, recommendation)
write_csv(reorder, "reports/reorder_recommendations.csv")

write_csv(inv |> filter(status %in% c("Slow-moving", "Dead stock", "Overstocked")) |>
            arrange(desc(inventory_value)) |>
            select(status, store_name, product_id, product_name, stock_quantity,
                   days_remaining, days_since_sale, inventory_value),
          "reports/slow_dead_overstock.csv")

lead_tbl <- suppliers |>
  left_join(products |> count(supplier_id, name = "products_supplied"),
            by = "supplier_id") |>
  arrange(desc(lead_time))
write_csv(lead_tbl, "reports/supplier_lead_times.csv")

write_csv(inv |> arrange(desc(risk_score)), "reports/inventory_risk_full.csv")
saveRDS(inv, "data/processed/inventory_risk.rds")

# ---- 8. Charts ----
save_plot(count(inv, status) |>
            ggplot(aes(reorder(status, n), n)) + geom_col() + coord_flip() +
            geom_text(aes(label = comma(n)), hjust = -0.1) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
            labs(title = "Inventory status (simulated stock)", x = NULL,
                 y = "Product-store combinations") + theme_minimal(), "27_inventory_status")

save_plot(inv |> slice_max(risk_score, n = 15, with_ties = FALSE) |>
            mutate(label = paste(str_trunc(product_name, 22), "-", store_name)) |>
            ggplot(aes(reorder(label, risk_score), risk_score)) + geom_col() + coord_flip() +
            labs(title = "Top 15 stock-risk scores", x = NULL, y = "Risk score (0-100)") +
            theme_minimal(), "28_top_risk")

save_plot(ggplot(filter(inv, is.finite(days_remaining)), aes(days_remaining)) +
            geom_histogram(bins = 50) + coord_cartesian(xlim = c(0, 150)) +
            labs(title = "Days of inventory remaining", x = "Days", y = "Product-store combinations") +
            theme_minimal(), "29_days_remaining")

save_plot(ggplot(lead_tbl, aes(reorder(supplier_name, lead_time), lead_time)) +
            geom_col() + coord_flip() +
            labs(title = "Supplier lead times (simulated)", x = NULL, y = "Days") +
            theme_minimal(), "30_supplier_lead_times")

print("Phase 6 complete")