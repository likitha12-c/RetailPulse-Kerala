# ---- 1. Load packages ----
library(tidyverse)
library(lubridate)
library(scales)

# ---- 2. Create output folders before writing any files ----
dir.create("reports", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("documentation", recursive = TRUE, showWarnings = FALSE)

# ---- 3. Load project data ----
sales    <- readRDS("data/processed/sales_store.rds")
inv      <- readRDS("data/processed/inventory_forecast.rds")
holidays <- readRDS("data/processed/holidays.rds") |>
  mutate(date = as.Date(date))
stores   <- readRDS("data/processed/stores.rds")
churn    <- read_csv("reports/churn_risk_predictions.csv", show_col_types = FALSE)
profit   <- read_csv("reports/product_profitability.csv", show_col_types = FALSE)
anom     <- read_csv("reports/anomaly_alerts.csv", show_col_types = FALSE)

# ---- 4. Prepare data types ----
sales <- sales |>
  mutate(
    invoice_day = as.Date(invoice_day),
    stock_code = as.character(stock_code),
    quantity = as.numeric(quantity),
    unit_price = as.numeric(unit_price)
  )

inv <- inv |>
  mutate(
    product_id = as.character(product_id),
    stock_quantity = as.numeric(stock_quantity),
    fc_demand_cover = as.numeric(fc_demand_cover),
    fc_demand_90 = as.numeric(fc_demand_90),
    cost_price = as.numeric(cost_price),
    lead_time = as.numeric(lead_time),
    safety_stock = as.numeric(safety_stock),
    inventory_value = as.numeric(inventory_value),
    fc_reorder_qty = as.numeric(fc_reorder_qty)
  )

profit <- profit |>
  mutate(
    stock_code = as.character(stock_code),
    revenue = readr::parse_number(as.character(revenue)),
    margin_pct = readr::parse_number(as.character(margin_pct))
  )

churn <- churn |>
  mutate(
    monetary = readr::parse_number(as.character(monetary)),
    churn_risk = readr::parse_number(as.character(churn_risk))
  ) |>
  mutate(
    # Accept either decimal risk (0-1) or percentage risk (0-100).
    churn_risk = if_else(churn_risk > 1, churn_risk / 100, churn_risk)
  )

anom <- anom |>
  mutate(
    invoice_day = as.Date(invoice_day),
    revenue = readr::parse_number(as.character(revenue)),
    expected = readr::parse_number(as.character(expected)),
    z = readr::parse_number(as.character(z))
  )

# ---- 5. Generate product return rates ----
# IMPORTANT: This assumes negative sales quantity represents a return.
# Confirm this meaning in your source data before interpreting the results.
product_return_rates <- sales |>
  group_by(stock_code) |>
  summarise(
    description = first(description),
    sold = sum(quantity[quantity > 0], na.rm = TRUE),
    returned = sum(abs(quantity[quantity < 0]), na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(sold > 0) |>
  mutate(return_rate_pct = round(returned / sold * 100, 2))

write_csv(product_return_rates, "reports/product_return_rates.csv")
cat("Product return rates generated successfully!\n")

rets <- read_csv("reports/product_return_rates.csv", show_col_types = FALSE) |>
  mutate(stock_code = as.character(stock_code))

# ---- 6. Reference date and assumptions ----
ref <- max(sales$invoice_day, na.rm = TRUE)
if (!is.finite(as.numeric(ref))) {
  stop("No valid invoice_day values were found in sales_store.rds.")
}

years_covered <- as.numeric(ref - min(sales$invoice_day, na.rm = TRUE) + 1) / 365
if (!is.finite(years_covered) || years_covered <= 0) years_covered <- 1

holding_rate <- 0.20  # Assumed yearly holding cost: 20%

price_tbl <- sales |>
  group_by(product_id = stock_code) |>
  summarise(price = median(unit_price, na.rm = TRUE), .groups = "drop")

#---- 7. Standard layout for every recommendation ----
  rec_cols <- c(
    "type", "store_id", "store_name", "entity_id", "entity_name",
    "situation", "recommendation", "priority", "evidence",
    "financial_impact_gbp", "impact_basis"
  )

empty <- tibble(
  type = character(), store_id = character(), store_name = character(),
  entity_id = character(), entity_name = character(), situation = character(),
  recommendation = character(), priority = character(), evidence = character(),
  financial_impact_gbp = numeric(), impact_basis = character()
)

mk <- function(df) {
  if (nrow(df) == 0) return(empty)
  
  df |>
    mutate(
      across(
        c(store_id, store_name, entity_id, entity_name),
        as.character
      ),
      financial_impact_gbp = as.numeric(financial_impact_gbp)
    ) |>
    select(all_of(rec_cols))
}
  
#---- Rule 1: stock below forecast need -> replenish ----
    r1 <- inv |>
    filter(fc_status %in% c("Critical stockout", "Replenish")) |>
    left_join(price_tbl, by = "product_id") |>
    mutate(
      type = "Replenishment",
      entity_id = product_id,
      entity_name = product_name,
      situation = if_else(
        fc_status == "Critical stockout",
        "Stock is zero", "Stock below forecast need"
      ),
      priority = fc_priority,
      recommendation = paste0(
        "Replenish ", product_name, " at ", store_name,
        " according to the forecast reorder quantity."
      ),
      evidence = paste0(
        "Stock ", stock_quantity, " units; forecast demand ",
        round(fc_demand_cover), " units over ", lead_time + 14,
        " days; supplier lead time ", lead_time,
        " days; safety stock ", safety_stock
      ),
      financial_impact_gbp = pmax(0, fc_demand_cover - stock_quantity) *
        pmax(0, price - cost_price),
      impact_basis = "Estimated margin at risk from shortfall (units short x unit margin)"
    ) |>
    mk()
  
  # ---- Rule 2: high stock, low demand -> reduce purchasing ----
  overstock <- inv |>
    filter(fc_status == "Overstocked") |>
    mutate(
      financial_impact_gbp = pmax(0, stock_quantity - fc_demand_90) *
        cost_price * holding_rate * 90 / 365
    ) |>
    filter(is.finite(financial_impact_gbp))
  
  r2 <- empty
  if (nrow(overstock) > 0) {
    impact_limit <- quantile(overstock$financial_impact_gbp, 0.80, na.rm = TRUE)
    r2 <- overstock |>
      mutate(
        type = "Reduce purchasing",
        entity_id = product_id,
        entity_name = product_name,
        situation = "Stock exceeds 90-day forecast demand",
        priority = if_else(financial_impact_gbp >= impact_limit, "Medium", "Low"),
        recommendation = paste0(
          "Hold new orders for ", product_name, " at ", store_name,
          " until stock falls below forecast demand."
        ),
        evidence = paste0(
          "Stock ", stock_quantity, " units vs 90-day forecast of ",
          round(fc_demand_90), " units"
        ),
        impact_basis = "Estimated holding cost of excess stock over 90 days (20% yearly, assumed)"
      ) |>
      mk()
  }
  
  # After acting on a recommendation, run for example:
  # mark_done("Replenishment|STR_01|85123A", "Purchase order sent")
  mark_done <- function(keys, note = "") {
    h <- read_csv(hist_path, col_types = cols(.default = "c"), show_col_types = FALSE)
    hit <- h$rec_key %in% keys
    h$status[hit] <- "Done"
    h$acted_on[hit] <- as.character(Sys.Date())
    h$notes[hit] <- note
    write_csv(h, hist_path)
    message(sum(hit), " history row(s) updated.")
  }
  
  # ---- 10. Summary and chart ----
  summ <- recs |>
    group_by(type, priority) |>
    summarise(
      recommendations = n(),
      financial_impact_gbp = sum(financial_impact_gbp, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(summ, "reports/recommendation_summary.csv")
  print(summ, n = 50)
  cat("Total recommendations:", nrow(recs), "\n")
  
  if (nrow(recs) > 0) {
    recommendation_plot <- ggplot(
      recs,
      aes(
        x = reorder(type, type, length),
        fill = factor(priority, levels = c("Critical", "High", "Medium", "Low"))
      )
    ) +
      geom_bar() +
      coord_flip() +
      scale_fill_manual(values = c(
        Critical = "#A32D2D", High = "#EF9F27",
        Medium = "#378ADD", Low = "#888780"
      )) +
      labs(
        title = "Recommendations by type and priority",
        x = NULL, y = "Recommendations", fill = "Priority"
      ) +
      theme_minimal()
    
    ggsave(
      "reports/figures/40_recommendations_by_type.png",
      recommendation_plot,
      width = 8, height = 5, dpi = 150
    )
  } else {
    message("No recommendations were generated; chart was not created.")
  }
  
  # ---- 11. Example output in the project brief layout ----
  fmt <- inv |>
    filter(fc_status %in% c("Critical stockout", "Replenish")) |>
    arrange(
      factor(fc_priority, levels = c("Critical", "High", "Medium", "Low")),
      desc(fc_reorder_qty)
    ) |>
    head(5)
  
  if (nrow(fmt) > 0) {
    lines <- unlist(pmap(fmt, function(...) {
      r <- list(...)
      c(
        paste0("STORE: ", toupper(gsub(" ", "_", r$store_name))),
        paste0("PRODUCT: ", r$product_name), "",
        paste0("CURRENT STOCK: ", r$stock_quantity, " units"),
        paste0("FORECAST DEMAND: ", round(r$fc_demand_cover), " units"),
        paste0("SUPPLIER LEAD TIME: ", r$lead_time, " days"), "",
        paste0(
          "STATUS: ",
          if (r$fc_status == "Critical stockout") "STOCKOUT" else "HIGH STOCK RISK"
        ),
        "", "RECOMMENDATION:",
        paste0(
          "Order ", round(r$fc_reorder_qty), " units",
          if (r$fc_priority %in% c("Critical", "High"))
            " immediately." else " in the next order cycle."
        ),
        "", paste0("PRIORITY: ", toupper(r$fc_priority)),
        "Assumptions: demand forecast from the 90-day model, 95% service level, simulated stock.",
        "----------------------------------------"
      )
    }))
  } else {
    lines <- "No replenishment examples were generated."
  }
  
  writeLines(lines, "reports/recommendation_examples.txt")
  cat(head(lines, 14), sep = "\n")
  
  # ---- 12. Documented recommendation rules ----
  writeLines(c(
    "# Recommendation rules", "",
    "| Type | Trigger | Priority |",
    "|---|---|---|",
    "| Replenishment | Stock below forecast need over lead time + 14 days (plus safety stock) | Uses forecast priority; critical stockout is highest |",
    "| Reduce purchasing | Stock above 90-day forecast, or dead or slow-moving stock | Medium for highest estimated holding-cost items; dead stock Medium; others Low |",
    "| Festival stocking | Festival within 45 days and last year's demand 1.5x normal or more | High at 3x or more; else Medium |",
    "| Customer retention | High churn risk and spend in the top quarter | High for the top 10% of spend; else Medium |",
    "| Pricing review | Revenue in the top quarter and margin in the bottom quarter | High for the top 10% of revenue; else Medium |",
    "| Investigate sales drop | Unusually low sales in the last 30 days | High when source severity is High; else Medium |",
    "| Investigate returns | Return rate in the top 10% of products | High at 20% or more; else Medium |",
    "| Supplier review | Lead time in the longest quarter and items needing replenishment | High at 20 or more items; else Medium |", "",
    "Assumptions: holding cost 20% a year; 5-percentage-point margin improvement from a price or cost review;",
    "negative sales quantities are treated as returns; cost prices, stock, stores, lead times and promotions may be simulated.",
    "Return value is sales value exposed, not confirmed financial loss. These are decision-support suggestions,",
    "not guaranteed outcomes."
  ), "documentation/recommendation_rules.md")
  
  cat("\nPhase 9 complete. Check the reports folder for the generated outputs.\n")
  