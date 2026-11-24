library(tidyverse)
library(lubridate)
library(scales)

sales      <- readRDS("data/processed/sales_store.rds")
returns    <- readRDS("data/processed/returns.rds")
products   <- readRDS("data/processed/products.rds") |>
  mutate(product_id = as.character(product_id))
inventory  <- readRDS("data/processed/inventory.rds") |>
  mutate(product_id = as.character(product_id))
promotions <- readRDS("data/processed/promotions.rds") |>
  mutate(product_id = as.character(product_id),
         start_date = as.Date(start_date), end_date = as.Date(end_date))

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 150)

d0 <- min(sales$invoice_day); d1 <- max(sales$invoice_day)
years_covered <- as.numeric(d1 - d0 + 1) / 365

# ---- Profit uses only products with a (simulated) cost price ----
costed <- sales |> filter(!is.na(cost_price)) |>
  mutate(cogs = quantity * cost_price, gross_profit = revenue - cogs)

# ---- Sales growth: Jan-Nov 2011 vs Jan-Nov 2010 (like for like) ----
yoy <- sales |> filter(month(invoice_day) <= 11, year(invoice_day) %in% 2010:2011) |>
  group_by(year = year(invoice_day)) |> summarise(revenue = sum(revenue))
sales_growth <- yoy$revenue[yoy$year == 2011] / yoy$revenue[yoy$year == 2010] - 1

# ---- Return rate ----
sold_qty     <- sum(sales$quantity)
returned_qty <- sum(abs(returns$quantity))

# ---- Customer retention: bought in 2010 and again in 2011 ----
cust <- sales |> filter(!is.na(customer_id)) |>
  group_by(customer_id) |>
  summarise(y2010 = any(year(invoice_day) == 2010),
            y2011 = any(year(invoice_day) == 2011), .groups = "drop")
eligible  <- sum(cust$y2010)
returning <- sum(cust$y2010 & cust$y2011)

# ---- Inventory turnover (simulated stock snapshot) ----
inv_value <- inventory |> left_join(products, by = "product_id") |>
  summarise(v = sum(stock_quantity * cost_price, na.rm = TRUE)) |> pull(v)
cogs_per_year <- sum(costed$cogs) / years_covered

kpi <- tibble(
  kpi = c("Total revenue (GBP)", "Total orders", "Average order value (GBP)",
          "Average selling price (GBP per unit)",
          "Revenue covered by cost data (%)",
          "Gross profit, covered products (GBP)",
          "Gross profit margin, covered products (%)",
          "Sales growth, Jan-Nov 2011 vs 2010 (%)",
          "Return rate (%)", "Customer retention 2010 to 2011 (%)",
          "Inventory turnover (times per year, simulated stock)"),
  value = c(sum(sales$revenue), n_distinct(sales$invoice),
            sum(sales$revenue) / n_distinct(sales$invoice),
            sum(sales$revenue) / sum(sales$quantity),
            100 * sum(costed$revenue) / sum(sales$revenue),
            sum(costed$gross_profit),
            100 * sum(costed$gross_profit) / sum(costed$revenue),
            100 * sales_growth, 100 * returned_qty / sold_qty,
            100 * returning / eligible, cogs_per_year / inv_value)) |>
  mutate(value = round(value, 2))
print(kpi)
write_csv(kpi, "reports/kpi_phase4.csv")

# ---- Q1 & Q2: product profitability ----
prod_profit <- costed |> group_by(stock_code, description) |>
  summarise(revenue = sum(revenue), gross_profit = sum(gross_profit),
            .groups = "drop") |>
  mutate(margin_pct = 100 * gross_profit / revenue)
write_csv(prod_profit, "reports/product_profitability.csv")

high_margin <- prod_profit |> filter(revenue >= median(revenue)) |>
  slice_max(margin_pct, n = 10)
poor_profit <- prod_profit |>
  filter(revenue >= quantile(revenue, 0.75),
         margin_pct <= quantile(margin_pct, 0.25)) |>
  arrange(desc(revenue))
print(high_margin); print(poor_profit)

save_plot(ggplot(high_margin, aes(reorder(str_trunc(description, 30), margin_pct),
                                  margin_pct)) +
            geom_col() + coord_flip() +
            labs(title = "Highest margin products (simulated costs)", x = NULL,
                 y = "Gross margin (%)") + theme_minimal(), "19_top_margin_products")

save_plot(ggplot(prod_profit, aes(revenue, margin_pct)) + geom_point(alpha = 0.6) +
            scale_x_continuous(labels = label_comma()) +
            labs(title = "Revenue vs margin by product (simulated costs)",
                 x = "Revenue (GBP)", y = "Gross margin (%)") + theme_minimal(),
          "20_revenue_vs_margin")

# ---- Q3: store growth (Jan-Nov 2011 vs 2010) ----
store_growth <- sales |>
  filter(month(invoice_day) <= 11, year(invoice_day) %in% 2010:2011) |>
  group_by(store_name, year = year(invoice_day)) |>
  summarise(revenue = sum(revenue), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = revenue, names_prefix = "y") |>
  mutate(growth_pct = round(100 * (y2011 / y2010 - 1), 1)) |>
  arrange(desc(growth_pct))
print(store_growth)
write_csv(store_growth, "reports/store_growth.csv")

save_plot(ggplot(store_growth, aes(reorder(store_name, growth_pct), growth_pct)) +
            geom_col() + coord_flip() +
            labs(title = "Store revenue growth, 2011 vs 2010 (simulated stores)",
                 x = NULL, y = "Growth (%)") + theme_minimal(), "21_store_growth")



# ---- Q4: do discounts help? (simulated promotions) ----

# Create valid promotion records using actual product IDs
set.seed(123)

promotion_products <- products |>
  filter(product_id %in% unique(sales$stock_code))

promotion_products <- promotion_products |>
  slice_sample(n = min(50, nrow(promotion_products)))
promotions_valid <- tibble(
  product_id = promotion_products$product_id,
  start_date = sample(seq(d0, d1 - 30, by = "day"),
                      nrow(promotion_products), replace = TRUE),
  discount = sample(c(10, 15, 20, 25, 30, 35, 40),
                    nrow(promotion_products), replace = TRUE)
) |>
  mutate(
    end_date = start_date + sample(7:14, n(), replace = TRUE),
    discount = discount / 100
  )

daily <- sales |>
  group_by(stock_code, invoice_day) |>
  summarise(units = sum(quantity), .groups = "drop")

price_tbl <- sales |>
  group_by(stock_code) |>
  summarise(price = median(unit_price, na.rm = TRUE), .groups = "drop")

total_days <- as.numeric(d1 - d0 + 1)

disc <- pmap_dfr(promotions_valid, function(product_id, start_date,
                                            discount, end_date) {
  
  s <- max(start_date, d0)
  e <- min(end_date, d1)
  
  if (s > e) return(NULL)
  
  n_p <- as.numeric(e - s + 1)
  n_normal <- total_days - n_p
  
  if (n_normal <= 0) return(NULL)
  
  dd <- filter(daily, stock_code == product_id)
  
  u_p <- sum(dd$units[dd$invoice_day >= s & dd$invoice_day <= e],
             na.rm = TRUE)
  u_o <- sum(dd$units, na.rm = TRUE) - u_p
  
  pr <- price_tbl$price[price_tbl$stock_code == product_id]
  cp <- products$cost_price[products$product_id == product_id]
  
  if (length(pr) == 0 || length(cp) == 0) return(NULL)
  
  pr <- pr[1]
  cp <- cp[1]
  
  tibble(
    product_id = product_id,
    discount = discount,
    units_per_day_promo = u_p / n_p,
    units_per_day_normal = u_o / n_normal,
    profit_per_day_promo = (u_p / n_p) * (pr * (1 - discount) - cp),
    profit_per_day_normal = (u_o / n_normal) * (pr - cp)
  )
})

disc <- disc |>
  mutate(
    unit_lift_pct = if_else(
      units_per_day_normal > 0,
      100 * (units_per_day_promo / units_per_day_normal - 1),
      NA_real_
    ),
    profit_change_pct = if_else(
      profit_per_day_normal != 0,
      100 * (profit_per_day_promo / profit_per_day_normal - 1),
      NA_real_
    )
  )

disc_summary <- disc |>
  filter(is.finite(unit_lift_pct), is.finite(profit_change_pct)) |>
  summarise(
    promotions_analysed = n(),
    median_unit_lift_pct = round(median(unit_lift_pct), 1),
    median_profit_change_pct = round(median(profit_change_pct), 1),
    share_with_higher_profit_pct =
      round(100 * mean(profit_change_pct > 0), 1)
  )

print(disc_summary)
write_csv(disc, "reports/discount_analysis.csv")


# ---- Q5: product return rates ----
sold_by <- sales |> group_by(stock_code) |>
  summarise(description = first(description), sold = sum(quantity), .groups = "drop")
ret_by <- returns |> group_by(stock_code) |>
  summarise(returned = sum(abs(quantity)), .groups = "drop")
return_rates <- sold_by |> inner_join(ret_by, by = "stock_code") |>
  filter(sold >= 100) |>
  mutate(return_rate_pct = round(100 * returned / sold, 1)) |>
  arrange(desc(return_rate_pct))
write_csv(return_rates, "reports/product_return_rates.csv")
print(head(return_rates, 10))

save_plot(return_rates |> slice_max(return_rate_pct, n = 10) |>
            ggplot(aes(reorder(str_trunc(description, 30), return_rate_pct), return_rate_pct)) +
            geom_col() + coord_flip() +
            labs(title = "Highest return rates (products with 100+ units sold)",
                 x = NULL, y = "Return rate (%)") + theme_minimal(), "22_return_rates")

print("Phase 4 complete: see reports/ and reports/figures")