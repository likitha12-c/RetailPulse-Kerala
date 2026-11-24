library(tidyverse)
library(lubridate)
library(ranger)
library(rpart)
library(zoo)
library(scales)
set.seed(42)

sales      <- readRDS("data/processed/sales_store.rds")
returns    <- readRDS("data/processed/returns.rds")
rfm        <- readRDS("data/processed/rfm.rds")
promotions <- readRDS("data/processed/promotions.rds") |>
  mutate(across(c(start_date, end_date), as.Date))
holidays   <- readRDS("data/processed/holidays.rds") |> mutate(date = as.Date(date))

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(p, name)
  ggsave(file.path("reports/figures", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 150)

ref <- max(sales$invoice_day)

# =====================================================================
# MODEL 1: CUSTOMER CHURN (no purchase in the next 90 days)
# =====================================================================
churn_features <- function(cutoff, horizon = 90) {
  hist <- sales |> filter(!is.na(customer_id), invoice_day < cutoff)
  fut  <- sales |> filter(!is.na(customer_id), invoice_day >= cutoff,
                          invoice_day < cutoff + horizon)
  hist |> group_by(customer_id) |>
    summarise(recency   = as.numeric(cutoff - max(invoice_day)),
              frequency = n_distinct(invoice),
              monetary  = sum(revenue),
              tenure    = as.numeric(cutoff - min(invoice_day)),
              n_products    = n_distinct(stock_code),
              orders_last90 = n_distinct(invoice[invoice_day >= cutoff - 90]),
              spend_last90  = sum(revenue[invoice_day >= cutoff - 90]),
              .groups = "drop") |>
    mutate(avg_order_value     = monetary / frequency,
           recent_share        = orders_last90 / frequency,
           days_between_orders = tenure / frequency,
           churn = as.integer(!(customer_id %in% fut$customer_id)))
}

predictors <- c("recency", "frequency", "monetary", "tenure", "n_products",
                "orders_last90", "spend_last90", "avg_order_value",
                "recent_share", "days_between_orders")
prep <- function(d) d |> mutate(across(c(monetary, spend_last90, avg_order_value), log1p))

cut_test  <- ref - 89          # test labels: last 90 days
cut_train <- cut_test - 90     # train labels: the 90 days before that
train <- churn_features(cut_train)
test  <- churn_features(cut_test)
cat("Churn rate  train:", round(mean(train$churn), 3),
    " test:", round(mean(test$churn), 3), "\n")

bin_metrics <- function(truth, prob, model, thr = 0.5) {
  pred <- as.integer(prob >= thr)
  tp <- sum(pred == 1 & truth == 1); fp <- sum(pred == 1 & truth == 0)
  fn <- sum(pred == 0 & truth == 1); tn <- sum(pred == 0 & truth == 0)
  precision <- tp / (tp + fp); recall <- tp / (tp + fn)
  r <- rank(prob); n1 <- sum(truth == 1); n0 <- sum(truth == 0)
  tibble(model = model, precision = precision, recall = recall,
         f1 = 2 * precision * recall / (precision + recall),
         roc_auc = (sum(r[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0),
         accuracy = (tp + tn) / length(truth), TP = tp, FP = fp, FN = fn, TN = tn)
}

tr <- prep(train) |> select(all_of(predictors), churn)
te <- prep(test)  |> select(all_of(predictors), churn)

glm_fit  <- glm(churn ~ ., data = tr, family = binomial())
glm_prob <- predict(glm_fit, te, type = "response")

rf_tr  <- tr |> mutate(churn = factor(churn, levels = c(0, 1)))
rf_fit <- ranger(churn ~ ., data = rf_tr, probability = TRUE,
                 importance = "impurity", num.trees = 300, seed = 42)
rf_prob <- predict(rf_fit, te)$predictions[, "1"]

churn_cmp <- bind_rows(bin_metrics(te$churn, glm_prob, "Logistic regression"),
                       bin_metrics(te$churn, rf_prob,  "Random forest")) |>
  mutate(across(where(is.numeric) & !c(TP, FP, FN, TN), ~ round(.x, 3)))
print(churn_cmp)
write_csv(churn_cmp, "reports/churn_model_comparison.csv")   # includes confusion matrix counts

imp <- tibble(feature = names(rf_fit$variable.importance),
              importance = rf_fit$variable.importance) |> arrange(desc(importance))
write_csv(imp, "reports/churn_feature_importance.csv")
print(imp)
save_plot(ggplot(imp, aes(reorder(feature, importance), importance)) + geom_col() +
            coord_flip() + labs(title = "Churn model: feature importance (random forest)",
                                x = NULL, y = "Importance") + theme_minimal(), "36_churn_importance")

# Final model on both periods, score all customers as of today's data end
final_rf <- ranger(churn ~ ., probability = TRUE, num.trees = 300, seed = 42,
                   data = bind_rows(tr, te) |> mutate(churn = factor(churn, levels = c(0, 1))))
current <- churn_features(ref + 1)
current$churn_risk <- predict(final_rf, prep(current) |> select(all_of(predictors)))$predictions[, "1"]

churn_pred <- current |>
  left_join(select(rfm, customer_id, segment), by = "customer_id") |>
  mutate(risk_band = case_when(churn_risk >= 0.7 ~ "High",
                               churn_risk >= 0.4 ~ "Medium", TRUE ~ "Low"),
         action = case_when(
           risk_band == "High"   & monetary >= quantile(monetary, 0.75) ~
             "Personal outreach with a 15% win-back offer",
           risk_band == "High"   ~ "Win-back email with a 10% offer",
           risk_band == "Medium" ~ "Reminder email featuring their usual products",
           TRUE                  ~ "Keep in regular newsletter")) |>
  arrange(desc(churn_risk)) |>
  select(customer_id, segment, churn_risk, risk_band, recency, frequency,
         monetary, action)
write_csv(churn_pred, "reports/churn_risk_predictions.csv")
print(count(churn_pred, risk_band))

# =====================================================================
# MODEL 2: PRODUCT DEMAND CLASSIFICATION (Low / Medium / High)
# =====================================================================
prod_features <- function(cutoff) {
  sales |> filter(invoice_day < cutoff) |> group_by(stock_code) |>
    summarise(description = first(description),
              units_prior90  = sum(quantity[invoice_day >= cutoff - 90]),
              units_prior180 = sum(quantity[invoice_day >= cutoff - 180]),
              units_total    = sum(quantity),
              orders         = n_distinct(invoice),
              customers      = n_distinct(customer_id, na.rm = TRUE),
              avg_price      = mean(unit_price),
              days_since_last = as.numeric(cutoff - max(invoice_day)),
              first_seen_days = as.numeric(cutoff - min(invoice_day)),
              active_days    = n_distinct(invoice_day), .groups = "drop") |>
    mutate(active_share = active_days / pmax(first_seen_days, 1)) |>
    select(-active_days)
}

cut_p <- ref - 89
next_units <- sales |> filter(invoice_day >= cut_p) |>
  group_by(stock_code) |> summarise(units_next = sum(quantity), .groups = "drop")

pf <- prod_features(cut_p) |>
  left_join(next_units, by = "stock_code") |>
  mutate(units_next = replace_na(units_next, 0))

q <- quantile(pf$units_next, c(1/3, 2/3))
cat("Demand thresholds (units in next 90 days): Low <=", q[1],
    "| Medium <=", q[2], "| High above", q[2], "\n")
pf <- pf |> mutate(demand = factor(case_when(units_next <= q[1] ~ "Low",
                                             units_next <= q[2] ~ "Medium",
                                             TRUE ~ "High"),
                                   levels = c("Low", "Medium", "High")))
dem_pred <- c("units_prior90", "units_prior180", "units_total", "orders",
              "customers", "avg_price", "days_since_last", "first_seen_days",
              "active_share")

idx <- sample(nrow(pf), round(0.7 * nrow(pf)))
dtr <- pf[idx, ]  |> select(all_of(dem_pred), demand)
dte <- pf[-idx, ] |> select(all_of(dem_pred), demand)

multi_metrics <- function(truth, pred, model) {
  lv <- levels(truth)
  cm <- table(factor(truth, lv), factor(pred, lv))
  rec <- diag(cm) / rowSums(cm); pre <- diag(cm) / colSums(cm)
  f1 <- 2 * pre * rec / (pre + rec)
  bind_cols(tibble(model = model, accuracy = sum(diag(cm)) / sum(cm),
                   macro_f1 = mean(f1, na.rm = TRUE)),
            as_tibble_row(setNames(rec, paste0("recall_", lv)))) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

tree_fit <- rpart(demand ~ ., data = dtr, method = "class",
                  control = rpart.control(cp = 0.005))
dem_rf   <- ranger(demand ~ ., data = dtr, importance = "impurity",
                   num.trees = 300, seed = 42)

dem_cmp <- bind_rows(
  multi_metrics(dte$demand, predict(tree_fit, dte, type = "class"), "Decision tree"),
  multi_metrics(dte$demand, predict(dem_rf, dte)$predictions, "Random forest"))
print(dem_cmp)
write_csv(dem_cmp, "reports/demand_model_comparison.csv")

dimp <- tibble(feature = names(dem_rf$variable.importance),
               importance = dem_rf$variable.importance) |> arrange(desc(importance))
write_csv(dimp, "reports/demand_feature_importance.csv")
print(dimp)

png("reports/figures/37_decision_tree.png", width = 1000, height = 700)
plot(tree_fit, uniform = TRUE, margin = 0.1)
text(tree_fit, use.n = TRUE, cex = 0.8)
dev.off()

# Final model: all products, predict demand class for the NEXT 90 days
final_dem <- ranger(demand ~ ., data = pf |> select(all_of(dem_pred), demand),
                    num.trees = 300, seed = 42)
pf_now <- prod_features(ref + 1)
pf_now$predicted_demand <- predict(final_dem, pf_now |> select(all_of(dem_pred)))$predictions
write_csv(pf_now |> select(stock_code, description, units_prior90, predicted_demand),
          "reports/product_demand_classes.csv")
print(count(pf_now, predicted_demand))

save_plot(count(pf_now, predicted_demand) |>
            ggplot(aes(predicted_demand, n)) + geom_col() +
            labs(title = "Predicted demand class of all products (next 90 days)",
                 x = NULL, y = "Products") + theme_minimal(), "38_demand_classes")

# =====================================================================
# MODEL 3: SALES ANOMALY DETECTION (store x category, daily revenue)
# =====================================================================
trading_days <- sort(unique(sales$invoice_day))

series <- sales |>
  mutate(category = coalesce(category, "Uncategorised")) |>
  group_by(store_name, category, invoice_day) |>
  summarise(revenue = sum(revenue), outlier_lines = sum(is_outlier), .groups = "drop")

series <- series |> distinct(store_name, category) |>
  crossing(invoice_day = trading_days) |>
  left_join(series, by = c("store_name", "category", "invoice_day")) |>
  mutate(revenue = replace_na(revenue, 0), outlier_lines = replace_na(outlier_lines, 0)) |>
  arrange(store_name, category, invoice_day) |>
  group_by(store_name, category) |>
  mutate(roll_mean = lag(rollapplyr(revenue, 28, mean, fill = NA)),
         roll_sd   = lag(rollapplyr(revenue, 28, sd,   fill = NA)),
         z = if_else(roll_sd > 0, (revenue - roll_mean) / roll_sd, NA_real_),
         q1 = quantile(revenue, 0.25), q3 = quantile(revenue, 0.75),
         iqr_flag = revenue < q1 - 1.5 * (q3 - q1) | revenue > q3 + 1.5 * (q3 - q1)) |>
  ungroup()

hol_days   <- as.numeric(unlist(map(holidays$date, ~ as.numeric(.x) + (-3:3))))
promo_days <- as.numeric(unlist(map2(promotions$start_date, promotions$end_date,
                                     ~ seq(as.numeric(.x), as.numeric(.y)))))
ret_daily  <- returns |> group_by(invoice_day) |>
  summarise(ret_units = sum(abs(quantity)), .groups = "drop")
ret_cut    <- quantile(ret_daily$ret_units, 0.90)

alerts <- series |>
  filter(!is.na(z), abs(z) >= 3) |>
  left_join(ret_daily, by = "invoice_day") |>
  mutate(ret_units = replace_na(ret_units, 0),
         direction = if_else(z > 0, "Unusually high", "Unusually low"),
         severity  = if_else(abs(z) >= 4, "High", "Medium"),
         near_festival = as.numeric(invoice_day) %in% hol_days,
         promo_active  = as.numeric(invoice_day) %in% promo_days,
         heavy_returns = ret_units >= ret_cut,
         possible_data_error = outlier_lines > 0,
         likely_cause = case_when(
           possible_data_error ~ "Extreme order lines: check for data error",
           near_festival       ~ "Within 3 days of a festival",
           promo_active        ~ "A promotion was running",
           heavy_returns       ~ "Heavy return day",
           TRUE                ~ "Unexplained: investigate")) |>
  arrange(desc(abs(z))) |>
  select(invoice_day, store_name, category, revenue, expected = roll_mean,
         z, direction, severity, confirmed_by_iqr = iqr_flag, likely_cause)
alerts <- alerts |> mutate(across(c(revenue, expected, z), ~ round(.x, 1)))
write_csv(alerts, "reports/anomaly_alerts.csv")
cat("Anomalies found:", nrow(alerts), "\n")
print(count(alerts, direction, severity))
print(count(alerts, likely_cause))

if (nrow(alerts) > 0) {
  top <- alerts |> count(store_name, category, sort = TRUE) |> slice(1)
  s1 <- series |> filter(store_name == top$store_name, category == top$category)
  a1 <- semi_join(s1, alerts, by = c("invoice_day", "store_name", "category"))
  save_plot(ggplot(s1, aes(invoice_day, revenue)) + geom_line(colour = "grey50") +
              geom_point(data = a1, colour = "red", size = 2) +
              scale_y_continuous(labels = label_comma()) +
              labs(title = paste("Anomalies:", top$store_name, "-", top$category),
                   subtitle = "Red = rolling z-score beyond 3 (simulated stores)",
                   x = NULL, y = "Daily revenue") + theme_minimal(), "39_anomalies")
}

print("Phase 8 complete")