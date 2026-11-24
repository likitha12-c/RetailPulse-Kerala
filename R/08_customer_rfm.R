library(tidyverse)
library(lubridate)
library(scales)

sales <- readRDS("data/processed/sales_store.rds") |>
  filter(!is.na(customer_id))

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 150)

# ---- 1. Unique customers and RFM values ----
reference_date <- max(sales$invoice_day) + 1

rfm <- sales |>
  group_by(customer_id) |>
  summarise(first_purchase = min(invoice_day),
            last_purchase  = max(invoice_day),
            recency   = as.numeric(reference_date - max(invoice_day)),
            frequency = n_distinct(invoice),
            monetary  = sum(revenue),
            .groups = "drop")

cat("Unique customers:", nrow(rfm), "\n")

# ---- 2. RFM scores (5 = best) ----
rfm <- rfm |>
  mutate(R = ntile(-recency, 5),
         F = ntile(frequency, 5),
         M = ntile(monetary, 5),
         rfm_score = paste0(R, F, M))

# ---- 3. Segments (rules are applied in this order) ----
rfm <- rfm |>
  mutate(segment = case_when(
    frequency == 1 & recency <= 60 ~ "New Customers",
    R >= 4 & F >= 4 & M >= 4       ~ "Champions",
    R >= 3 & F >= 4                ~ "Loyal Customers",
    R >= 3                         ~ "Potential Loyalists",
    R <= 2 & (F >= 3 | M >= 4)     ~ "At Risk",
    TRUE                           ~ "Hibernating"),
    segment = factor(segment, levels = c("Champions", "Loyal Customers",
                                         "Potential Loyalists", "New Customers", "At Risk", "Hibernating")))

segment_summary <- rfm |>
  group_by(segment) |>
  summarise(customers = n(),
            avg_recency_days = round(mean(recency)),
            avg_orders = round(mean(frequency), 1),
            avg_spend = round(mean(monetary)),
            revenue = sum(monetary), .groups = "drop") |>
  mutate(customer_share_pct = round(100 * customers / sum(customers), 1),
         revenue_share_pct  = round(100 * revenue / sum(revenue), 1))
print(segment_summary)

# ---- 4. Charts ----
seg_cols <- c("Champions" = "#1D9E75", "Loyal Customers" = "#378ADD",
              "Potential Loyalists" = "#7F77DD", "New Customers" = "#EF9F27",
              "At Risk" = "#E24B4A", "Hibernating" = "#888780")

save_plot(ggplot(segment_summary, aes(reorder(segment, customers), customers,
                                      fill = segment)) +
            geom_col(show.legend = FALSE) + coord_flip() +
            scale_fill_manual(values = seg_cols) +
            geom_text(aes(label = comma(customers)), hjust = -0.1) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
            labs(title = "Customers by segment", x = NULL, y = "Customers") +
            theme_minimal(), "23_segment_customers")

save_plot(ggplot(segment_summary, aes(reorder(segment, revenue), revenue,
                                      fill = segment)) +
            geom_col(show.legend = FALSE) + coord_flip() +
            scale_fill_manual(values = seg_cols) +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Revenue by segment", x = NULL, y = "Revenue (GBP)") +
            theme_minimal(), "24_segment_revenue")

save_plot(ggplot(rfm, aes(frequency, monetary, colour = segment)) +
            geom_point(alpha = 0.5) + scale_x_log10() +
            scale_y_log10(labels = label_comma()) +
            scale_colour_manual(values = seg_cols) +
            labs(title = "Customer segments: orders vs spend",
                 x = "Orders (log)", y = "Total spend (log)", colour = NULL) +
            theme_minimal(), "25_segment_scatter")

save_plot(ggplot(rfm, aes(recency)) + geom_histogram(bins = 50) +
            labs(title = "Recency distribution", x = "Days since last purchase",
                 y = "Customers") + theme_minimal(), "26_recency_hist")

# ---- 5. High-value customers (top 10% by spend) ----
high_value <- rfm |> filter(monetary >= quantile(monetary, 0.90)) |>
  arrange(desc(monetary)) |>
  select(customer_id, segment, recency, frequency, monetary)
cat("High-value customers:", nrow(high_value), "\n")
write_csv(high_value, "reports/high_value_customers.csv")

# ---- 6. Reactivation recommendations for At Risk customers ----
fav_product <- sales |>
  group_by(customer_id, description) |>
  summarise(spend = sum(revenue), .groups = "drop") |>
  group_by(customer_id) |> slice_max(spend, n = 1, with_ties = FALSE) |>
  ungroup() |> select(customer_id, favourite_product = description)

fav_category <- sales |> filter(!is.na(category)) |>
  group_by(customer_id, category) |>
  summarise(spend = sum(revenue), .groups = "drop") |>
  group_by(customer_id) |> slice_max(spend, n = 1, with_ties = FALSE) |>
  ungroup() |> select(customer_id, favourite_category = category)

at_risk <- rfm |> filter(segment == "At Risk") |>
  left_join(fav_product,  by = "customer_id") |>
  left_join(fav_category, by = "customer_id") |>
  mutate(favourite_category = coalesce(favourite_category, "n/a"),
         priority = case_when(monetary >= quantile(monetary, 0.75) ~ "High",
                              monetary >= quantile(monetary, 0.40) ~ "Medium",
                              TRUE ~ "Low"),
         offer_pct = case_when(priority == "High" ~ 15,
                               priority == "Medium" ~ 10, TRUE ~ 5),
         recommendation = paste0("Send a ", offer_pct, "% win-back offer on '",
                                 favourite_product, "'. Last purchase ",
                                 recency, " days ago.")) |>
  arrange(desc(monetary)) |>
  select(customer_id, priority, recency, frequency, monetary,
         favourite_product, favourite_category, offer_pct, recommendation)

cat("At Risk customers:", nrow(at_risk), "\n")
print(head(at_risk, 5))
write_csv(at_risk, "reports/reactivation_recommendations.csv")

# ---- 7. Save outputs ----
write_csv(segment_summary, "reports/rfm_segment_summary.csv")
write_csv(rfm, "reports/rfm_customers.csv")
saveRDS(rfm, "data/processed/rfm.rds")

print("Phase 5 complete")