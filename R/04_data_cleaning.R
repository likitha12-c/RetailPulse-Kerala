library(tidyverse)
library(lubridate)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE)

# ---- Load and rename columns ----
retail <- readRDS("data/processed/retail_imported.rds") |>
  rename(invoice = Invoice, stock_code = StockCode, description = Description,
         quantity = Quantity, invoice_date = InvoiceDate, unit_price = Price,
         customer_id = `Customer ID`, country = Country,
         source_sheet = SourceSheet)

# ---- Tasks 1-3: inspect ----
str(retail)
dim(retail)
glimpse(retail)

# ---- Task 4: missing values ----
missing_report <- retail |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "missing") |>
  mutate(missing_pct = round(100 * missing / nrow(retail), 2))
print(missing_report)

# ---- Task 7: dates ----
retail <- retail |>
  mutate(invoice_date = as.POSIXct(invoice_date, tz = "UTC"),
         invoice_day  = as_date(invoice_date))

# ---- Task 8: standardise identifiers ----
retail <- retail |>
  mutate(invoice     = str_trim(as.character(invoice)),
         stock_code  = str_to_upper(str_trim(as.character(stock_code))),
         customer_id = as.character(as.integer(customer_id)),
         description = str_squish(description),
         country     = str_trim(country))

# ---- Task 5: duplicates ----
key_cols <- c("invoice", "stock_code", "description", "quantity",
              "invoice_date", "unit_price", "customer_id", "country")
retail$is_dup <- duplicated(retail[key_cols])
n_dup <- sum(retail$is_dup)

# ---- Task 6: negative quantities and prices ----
n_neg_qty   <- sum(retail$quantity < 0, na.rm = TRUE)
n_zero_neg_price <- sum(retail$unit_price <= 0, na.rm = TRUE)

# ---- Task 10: cancelled orders and returns ----
retail <- retail |>
  mutate(is_cancelled = str_starts(invoice, "C"))

# ---- Task 9: documented rejection rules ----
non_product <- c("POST", "D", "M", "DOT", "BANK CHARGES", "C2", "CRUK", "S",
                 "B", "AMAZONFEE", "ADJUST", "ADJUST2", "TEST001", "TEST002")

retail <- retail |>
  mutate(reject_reason = case_when(
    is_dup                       ~ "Duplicate row",
    is.na(invoice_date)          ~ "Missing invoice date",
    unit_price <= 0              ~ "Zero or negative price",
    quantity == 0                ~ "Zero quantity",
    stock_code %in% non_product  ~ "Non-product code (postage, fees, etc.)",
    TRUE                         ~ NA_character_))

rejected <- retail |> filter(!is.na(reject_reason))
valid    <- retail |> filter(is.na(reject_reason))
returns  <- valid  |> filter(is_cancelled | quantity < 0)

sales <- valid |>
  filter(!(is_cancelled | quantity < 0)) |>
  mutate(revenue      = quantity * unit_price,
         has_customer = !is.na(customer_id))

# ---- Task 11: unusual values (flagged, not removed) ----
rev_cut <- quantile(sales$revenue, 0.999)
sales <- sales |> mutate(is_outlier = revenue > rev_cut)

# ---- Task 12: merge with product details (simulated) ----
products <- readRDS("data/processed/products.rds") |>
  mutate(product_id = as.character(product_id))

sales_clean <- sales |>
  left_join(products, by = c("stock_code" = "product_id"))

# ---- Task 13: save cleaned data ----
saveRDS(sales_clean, "data/processed/sales_clean.rds")
saveRDS(returns,     "data/processed/returns.rds")
write_csv(rejected |> select(-is_dup), "data/processed/rejected_records.csv")

# ---- Task 14: validate ----
stopifnot(
  all(sales_clean$quantity > 0),
  all(sales_clean$unit_price > 0),
  !any(duplicated(sales_clean[key_cols])),
  nrow(sales_clean) + nrow(returns) + nrow(rejected) == nrow(retail)
)

# ---- Data-quality report ----
dq <- tibble(
  metric = c("Raw rows", "Duplicate rows", "Rows with missing customer_id",
             "Negative-quantity rows", "Zero/negative price rows",
             "Cancelled invoice rows", "Rejected rows", "Return rows",
             "Clean sales rows", "Share of raw rows kept as clean sales (%)",
             "Outliers flagged (revenue > 99.9th percentile)"),
  value  = c(nrow(retail), n_dup, sum(is.na(retail$customer_id)),
             n_neg_qty, n_zero_neg_price, sum(retail$is_cancelled),
             nrow(rejected), nrow(returns), nrow(sales_clean),
             round(100 * nrow(sales_clean) / nrow(retail), 1),
             sum(sales_clean$is_outlier)))

write_csv(dq, "reports/data_quality_report.csv")
write_csv(missing_report, "reports/missing_values.csv")
write_csv(count(rejected, reject_reason), "reports/rejection_reasons.csv")

print(dq)
print(count(rejected, reject_reason))
print("Phase 2 cleaning complete and validated!")