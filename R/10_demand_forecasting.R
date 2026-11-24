library(tidyverse)
library(lubridate)
library(forecast)
library(ranger)
library(scales)
set.seed(42)

sales <- readRDS("data/processed/sales_store.rds")
inv   <- readRDS("data/processed/inventory_risk.rds")

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 9, height = 5, dpi = 150)

review_days <- 14   # same order cycle as Phase 6

# ---- 1. Daily and weekly series, missing dates ----
all_days <- tibble(date = seq(min(sales$invoice_day), max(sales$invoice_day),
                              by = "day"))
daily <- all_days |>
  left_join(sales |> group_by(date = invoice_day) |>
              summarise(units = sum(quantity), revenue = sum(revenue),
                        .groups = "drop"), by = "date") |>
  mutate(had_sales = !is.na(units),
         units = replace_na(units, 0), revenue = replace_na(revenue, 0))

cat("Days in range:", nrow(daily), "| days with no sales:", sum(!daily$had_sales), "\n")
print(daily |> filter(!had_sales) |>
        count(weekday = wday(date, label = TRUE, week_start = 1)))

weekly <- daily |> mutate(week = floor_date(date, "week", week_start = 1)) |>
  group_by(week) |> summarise(units = sum(units), revenue = sum(revenue),
                              .groups = "drop")
write_csv(daily,  "reports/daily_series.csv")
write_csv(weekly, "reports/weekly_series.csv")

# ---- 2. Trend and seasonality ----
stl_fit <- stl(ts(daily$units, frequency = 7), s.window = "periodic", robust = TRUE)
comp <- as.data.frame(stl_fit$time.series)
seasonal_strength <- max(0, 1 - var(comp$remainder) / var(comp$seasonal + comp$remainder))
trend_strength    <- max(0, 1 - var(comp$remainder) / var(comp$trend + comp$remainder))
cat("Weekly seasonality strength:", round(seasonal_strength, 2),
    "| trend strength:", round(trend_strength, 2), "\n")

save_plot(autoplot(stl_fit) + labs(title = "Decomposition of daily units (weekly cycle)"),
          "31_decomposition")
save_plot(ggplot(weekly, aes(week, units)) + geom_line() +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Weekly units sold", x = NULL, y = "Units") + theme_minimal(),
          "32_weekly_units")

# ---- 3. Model functions ----
yfour <- function(d, K = 3) {
  k <- 1:K; ang <- 2 * pi * outer(yday(d), k) / 365.25
  m <- cbind(sin(ang), cos(ang)); colnames(m) <- c(paste0("S", k), paste0("C", k)); m
}

rf_forecast <- function(dates, y, h) {
  n <- length(y)
  all_dates <- c(dates, seq(max(dates) + 1, by = "day", length.out = h))
  y_all <- c(y, rep(NA_real_, h))
  cal <- tibble(dow = wday(all_dates, week_start = 1), month = month(all_dates),
                doy = yday(all_dates), woy = isoweek(all_dates))
  lag_vec <- function(i) c(lag7 = y_all[i - 7], lag14 = y_all[i - 14],
                           lag21 = y_all[i - 21], lag28 = y_all[i - 28])
  rows <- 29:n
  train_df <- bind_cols(cal[rows, ], as_tibble(t(sapply(rows, lag_vec)))) |>
    mutate(y = y[rows])
  fit <- ranger(y ~ ., data = train_df, num.trees = 500, seed = 42)
  for (i in (n + 1):(n + h)) {
    xi <- bind_cols(cal[i, ], as_tibble(as.list(lag_vec(i))))
    y_all[i] <- max(0, predict(fit, data = xi)$predictions)
  }
  tail(y_all, h)
}

all_models <- function(dates, y, h) {
  fut <- seq(max(dates) + 1, by = "day", length.out = h)
  y_ts <- ts(y, frequency = 7)
  list(
    naive    = rep(tail(y, 1), h),
    snaive   = rep(tail(y, 7), length.out = h),
    ma7      = rep(mean(tail(y, 7)), h),
    ets      = as.numeric(forecast(ets(y_ts), h = h)$mean),
    arima    = as.numeric(forecast(auto.arima(y_ts), h = h)$mean),
    arima_yearly = as.numeric(forecast(auto.arima(y_ts, xreg = yfour(dates)),
                                       h = h, xreg = yfour(fut))$mean),
    random_forest = rf_forecast(dates, y, h)
  ) |> map(~ pmax(0, as.numeric(.x)))
}

# ---- 4. Time-based split: train on history, test on the last 90 days ----
n <- nrow(daily); h_test <- 90
train <- daily[1:(n - h_test), ]
test  <- daily[(n - h_test + 1):n, ]
cat("Train:", format(min(train$date)), "to", format(max(train$date)),
    "| Test:", format(min(test$date)), "to", format(max(test$date)), "\n")

preds <- all_models(train$date, train$units, h_test)

metrics <- function(actual, pred) {
  nz <- actual > 0
  tibble(MAE = mean(abs(actual - pred)), RMSE = sqrt(mean((actual - pred)^2)),
         Bias = mean(pred - actual),
         MAPE = 100 * mean(abs(actual[nz] - pred[nz]) / actual[nz]))
}

results <- expand_grid(model = names(preds), horizon = c(7, 30, 90)) |>
  mutate(m = map2(model, horizon, ~ metrics(test$units[1:.y], preds[[.x]][1:.y]))) |>
  unnest(m) |>
  group_by(horizon) |>
  mutate(rmse_vs_snaive_pct = round(100 * (1 - RMSE / RMSE[model == "snaive"]), 1)) |>
  ungroup() |>
  mutate(across(c(MAE, RMSE, Bias, MAPE), ~ round(.x, 1)))
print(results, n = 30)
write_csv(results, "reports/forecast_model_comparison.csv")

best <- results |> group_by(horizon) |> slice_min(RMSE, n = 1, with_ties = FALSE) |>
  ungroup()
print(best)
write_csv(best, "reports/best_model_per_horizon.csv")

# ---- 5. Charts: errors and actual vs predicted ----
save_plot(ggplot(results, aes(model, RMSE, fill = model)) + geom_col(show.legend = FALSE) +
            facet_wrap(~ horizon, labeller = label_both) + coord_flip() +
            labs(title = "Forecast error (RMSE) by model and horizon", x = NULL,
                 y = "RMSE (units per day)") + theme_minimal(), "33_model_comparison")

test_long <- bind_cols(date = test$date, as_tibble(preds)) |>
  pivot_longer(-date, names_to = "model", values_to = "forecast")
save_plot(ggplot() +
            geom_line(data = test_long, aes(date, forecast, colour = model), alpha = 0.7) +
            geom_line(data = test, aes(date, units), colour = "black", linewidth = 1) +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "Actual (black) vs predicted units, test period", x = NULL,
                 y = "Units per day", colour = "Model") + theme_minimal(),
          "34_actual_vs_predicted")

# ---- 6. Final forecast: refit on all data, next 90 days ----
final <- all_models(daily$date, daily$units, 90)
future_dates <- seq(max(daily$date) + 1, by = "day", length.out = 90)

pick <- function(hz) best$model[best$horizon == hz]
fc_final <- tibble(date = future_dates, day_ahead = 1:90) |>
  mutate(model = case_when(day_ahead <= 7  ~ pick(7),
                           day_ahead <= 30 ~ pick(30), TRUE ~ pick(90)),
         forecast = map2_dbl(model, day_ahead, ~ final[[.x]][.y]),
         rmse = map2_dbl(model, day_ahead, function(m, d) {
           hz <- if (d <= 7) 7 else if (d <= 30) 30 else 90
           results$RMSE[results$model == m & results$horizon == hz] }),
         lower_80 = pmax(0, forecast - 1.28 * rmse),
         upper_80 = forecast + 1.28 * rmse) |>
  select(date, day_ahead, model, forecast, lower_80, upper_80)

write_csv(fc_final, "reports/forecast_next90_days.csv")
write_csv(fc_final |> filter(day_ahead <= 7),  "reports/forecast_7_day.csv")
write_csv(fc_final |> filter(day_ahead <= 30), "reports/forecast_30_day.csv")
cat("Forecast units: next 7 days =", round(sum(fc_final$forecast[1:7])),
    "| 30 days =", round(sum(fc_final$forecast[1:30])),
    "| 90 days =", round(sum(fc_final$forecast)), "\n")

save_plot(ggplot() +
            geom_line(data = tail(daily, 120), aes(date, units)) +
            geom_ribbon(data = fc_final, aes(date, ymin = lower_80, ymax = upper_80),
                        alpha = 0.25) +
            geom_line(data = fc_final, aes(date, forecast), colour = "#185FA5") +
            scale_y_continuous(labels = label_comma()) +
            labs(title = "90-day demand forecast with approximate 80% interval",
                 x = NULL, y = "Units per day") + theme_minimal(), "35_forecast_90_day")

# ---- 7. Demand-aware inventory recommendations ----
company_avg <- mean(tail(daily$units, 90))
cumF <- cumsum(fc_final$forecast)

inv_fc <- inv |>
  mutate(share = avg_daily_demand / company_avg,
         horizon_days   = lead_time + review_days,
         fc_demand_cover = share * cumF[horizon_days],
         fc_demand_90    = share * sum(fc_final$forecast),
         fc_daily        = share * mean(fc_final$forecast),
         fc_days_cover   = if_else(fc_daily > 0, stock_quantity / fc_daily, Inf),
         required_stock  = ceiling(fc_demand_cover + safety_stock),
         fc_reorder_qty  = pmax(0, required_stock - stock_quantity),
         fc_status = case_when(
           stock_quantity == 0 & fc_daily > 0 ~ "Critical stockout",
           fc_reorder_qty > 0                 ~ "Replenish",
           stock_quantity > fc_demand_90 & fc_demand_90 > 0 ~ "Overstocked",
           TRUE ~ "OK"),
         fc_priority = case_when(
           fc_status == "Critical stockout"                   ~ "Critical",
           fc_status == "Replenish" & fc_days_cover < lead_time ~ "High",
           fc_status == "Replenish"                            ~ "Medium",
           TRUE ~ "Low"),
         recommendation = case_when(
           fc_status %in% c("Critical stockout", "Replenish") ~ paste0(
             "Order ", round(fc_reorder_qty), " units of ", product_name, " for ",
             store_name, ". Forecast demand is ", round(fc_demand_cover),
             " units over the next ", horizon_days, " days (lead time ", lead_time,
             " + ", review_days, " review days); stock is ", stock_quantity,
             ", safety stock ", safety_stock, "."),
           fc_status == "Overstocked" ~ paste0(
             "Hold orders for ", product_name, " at ", store_name,
             ": stock exceeds the 90-day forecast of ", round(fc_demand_90), " units."),
           TRUE ~ "No action needed."))

print(count(inv_fc, fc_status, fc_priority))

fc_reorder <- inv_fc |> filter(fc_status != "OK") |>
  arrange(factor(fc_priority, levels = c("Critical", "High", "Medium", "Low")),
          desc(fc_reorder_qty)) |>
  select(fc_priority, fc_status, store_name, district, product_id, product_name,
         stock_quantity, fc_demand_cover, safety_stock, lead_time, fc_reorder_qty,
         supplier_name, recommendation)
write_csv(fc_reorder, "reports/forecast_reorder_recommendations.csv")
saveRDS(inv_fc, "data/processed/inventory_forecast.rds")

print("Phase 7 complete")