library(tidyverse)
library(lubridate)
library(scales)

sales <- readRDS("data/processed/sales_store.rds")
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 150)

# ---- Headline KPIs ----
orders <- sales |> group_by(invoice) |>
  summarise(order_value = sum(revenue), .groups = "drop")
kpi <- tibble(
  total_revenue = sum(sales$revenue),
  total_quantity = sum(sales$quantity),
  total_orders = n_distinct(sales$invoice),
  avg_order_value = mean(orders$order_value))
print(kpi)
write_csv(kpi, "reports/kpi_summary.csv")

monthly <- sales |> mutate(month = floor_date(invoice_day, "month")) |>
  group_by(month) |> summarise(revenue = sum(revenue), .groups = "drop") |>
  mutate(mom_growth = revenue / lag(revenue) - 1)

top_products <- sales |> group_by(stock_code, description) |>
  summarise(revenue = sum(revenue), quantity = sum(quantity), .groups = "drop")

# 1 Monthly revenue
save_plot(ggplot(monthly, aes(month, revenue)) + geom_line(linewidth = 1) +
            geom_point() + scale_y_continuous(labels = label_comma()) +
            labs(title = "Monthly revenue", x = NULL, y = "Revenue (GBP)") +
            theme_minimal(), "01_monthly_revenue")

# 2 Month-over-month growth
save_plot(ggplot(filter(monthly, !is.na(mom_growth)),
                 aes(month, mom_growth)) + geom_col() +
            scale_y_continuous(labels = label_percent()) +
            labs(title = "Month-over-month growth", x = NULL, y = "Growth") +
            theme_minimal(), "02_mom_growth")

# 3 Weekly revenue
weekly <- sales |> mutate(week = floor_date(invoice_day, "week")) |>
  group_by(week) |> summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(weekly, aes(week, revenue)) + geom_line() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Weekly revenue", x = NULL, y = "Revenue") + theme_minimal(),
          "03_weekly_revenue")

# 4 Revenue by weekday
wd <- sales |> mutate(weekday = wday(invoice_day, label = TRUE, week_start = 1)) |>
  group_by(weekday) |> summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(wd, aes(weekday, revenue)) + geom_col() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Revenue by weekday", x = NULL, y = "Revenue") + theme_minimal(),
          "04_weekday_revenue")

# 5 Weekday vs month heatmap
hm <- sales |> mutate(weekday = wday(invoice_day, label = TRUE, week_start = 1),
                      month = format(invoice_day, "%Y-%m")) |>
  group_by(month, weekday) |> summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(hm, aes(month, weekday, fill = revenue)) + geom_tile() +
            scale_fill_viridis_c(labels = label_comma()) +
            labs(title = "Revenue: weekday vs month", x = NULL, y = NULL) +
            theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          "05_heatmap_weekday_month")

# 6 Weekday vs weekend
we <- sales |> mutate(type = if_else(wday(invoice_day, week_start = 1) >= 6,
                                     "Weekend", "Weekday")) |>
  group_by(type) |> summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(we, aes(type, revenue)) + geom_col() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Weekday vs weekend revenue", x = NULL, y = "Revenue") +
            theme_minimal(), "06_weekday_vs_weekend")

# 7 Top 10 products by revenue
save_plot(top_products |> slice_max(revenue, n = 10) |>
            ggplot(aes(reorder(str_trunc(description, 30), revenue), revenue)) +
            geom_col() + coord_flip() + scale_y_continuous(labels = label_comma()) +
            labs(title = "Top 10 products by revenue", x = NULL, y = "Revenue") +
            theme_minimal(), "07_top10_products")

# 8 Bottom 10 products by revenue
save_plot(top_products |> slice_min(revenue, n = 10) |>
            ggplot(aes(reorder(str_trunc(description, 30), revenue), revenue)) +
            geom_col() + coord_flip() +
            labs(title = "Bottom 10 products by revenue", x = NULL, y = "Revenue") +
            theme_minimal(), "08_bottom10_products")

# 9 Quantity vs revenue (log scales)
save_plot(ggplot(top_products, aes(quantity, revenue)) + geom_point(alpha = 0.3) +
            scale_x_log10(labels = label_comma()) + scale_y_log10(labels = label_comma()) +
            labs(title = "Quantity vs revenue by product", x = "Quantity (log)",
                 y = "Revenue (log)") + theme_minimal(), "09_scatter_qty_revenue")

# 10 Order value histogram
save_plot(ggplot(orders, aes(order_value)) + geom_histogram(bins = 60) +
            scale_x_log10(labels = label_comma()) +
            labs(title = "Order value distribution", x = "Order value (log)", y = "Orders") +
            theme_minimal(), "10_order_value_hist")

# 11 Boxplot of line revenue by category
cat_sales <- filter(sales, !is.na(category))
save_plot(ggplot(cat_sales, aes(category, revenue)) + geom_boxplot() +
            scale_y_log10() + labs(title = "Line revenue by category (outliers)",
                                   x = NULL, y = "Revenue (log)") + theme_minimal(),
          "11_boxplot_category")

# 12 Category contribution (donut)
cat_rev <- cat_sales |> group_by(category) |>
  summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(cat_rev, aes(x = 2, y = revenue, fill = category)) +
            geom_col(width = 1) + coord_polar("y") + xlim(0.5, 2.5) + theme_void() +
            labs(title = "Category contribution (simulated categories)"),
          "12_category_donut")

# 13 Revenue by district (simulated)
dist <- sales |> group_by(district) |>
  summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(dist, aes(reorder(district, revenue), revenue)) + geom_col() +
            coord_flip() + scale_y_continuous(labels = label_comma()) +
            labs(title = "Revenue by district (simulated stores)", x = NULL,
                 y = "Revenue") + theme_minimal(), "13_district_revenue")

# 14 Revenue by store (simulated)
store_rev <- sales |> group_by(store_name) |>
  summarise(revenue = sum(revenue), .groups = "drop")
save_plot(ggplot(store_rev, aes(reorder(store_name, revenue), revenue)) +
            geom_col() + coord_flip() + scale_y_continuous(labels = label_comma()) +
            labs(title = "Revenue by store (simulated)", x = NULL, y = "Revenue") +
            theme_minimal(), "14_store_revenue")

# 15 Top 10 countries
save_plot(sales |> group_by(country) |>
            summarise(revenue = sum(revenue), .groups = "drop") |>
            slice_max(revenue, n = 10) |>
            ggplot(aes(reorder(country, revenue), revenue)) + geom_col() + coord_flip() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Top 10 countries by revenue", x = NULL, y = "Revenue") +
            theme_minimal(), "15_top_countries")

# 16 Sales by hour of day
save_plot(sales |> mutate(hour = hour(invoice_date)) |>
            group_by(hour) |> summarise(revenue = sum(revenue), .groups = "drop") |>
            ggplot(aes(hour, revenue)) + geom_col() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Revenue by hour of day", x = "Hour", y = "Revenue") +
            theme_minimal(), "16_hourly_revenue")

# ---- Facts for your 10 findings ----
print(slice_max(monthly, revenue, n = 1))
print(slice_max(wd, revenue, n = 1))
print(slice_max(top_products, revenue, n = 3))
print(slice_max(dist, revenue, n = 1))
print(slice_max(cat_rev, revenue, n = 1))
print(we)
print(mean(!sales$has_customer))
print("EDA complete: check reports/figures")