
# ==========================================
# PHASE 11.8 - SQL AND DATABASE OPERATIONS
# Retail Pulse Kerala
# ==========================================

library(DBI)
library(RSQLite)
library(dplyr)

# Create database folder
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "reports",
  recursive = TRUE,
  showWarnings = FALSE
)

# Load existing sales data
sales <- readRDS("data/processed/sales_store.rds")

# Check required columns
required_columns <- c(
  "invoice",
  "invoice_day",
  "stock_code",
  "quantity",
  "revenue"
)

missing_columns <- setdiff(
  required_columns,
  names(sales)
)

if (length(missing_columns) > 0) {
  stop(
    paste("Missing columns:", paste(missing_columns, collapse = ", "))
  )
}

# Select columns for database
sales_sql <- sales |>
  select(all_of(required_columns))

# Convert date to SQLite-friendly format
sales_sql$invoice_day <- as.character(
  as.Date(sales_sql$invoice_day)
)

# Connect to SQLite database
con <- dbConnect(
  SQLite(),
  "data/processed/retail_pulse.sqlite"
)

# Write sales data into database
dbWriteTable(
  con,
  "sales",
  sales_sql,
  overwrite = TRUE
)

# Create index for faster queries
dbExecute(
  con,
  "CREATE INDEX idx_sales_product
   ON sales(stock_code)"
)

# SQL QUERY 1: Overall sales KPIs
sales_kpis <- dbGetQuery(
  con,
  "
  SELECT
    SUM(revenue) AS total_revenue,
    SUM(quantity) AS total_quantity,
    COUNT(DISTINCT invoice) AS total_orders
  FROM sales
  "
)

print("Overall Sales KPIs")
print(sales_kpis)


# SQL QUERY 2: Monthly revenue
monthly_sales <- dbGetQuery(
  con,
  "
  SELECT
    strftime('%Y-%m', invoice_day) AS month,
    SUM(revenue) AS monthly_revenue,
    SUM(quantity) AS total_quantity
  FROM sales
  GROUP BY month
  ORDER BY month
  "
)

print("Monthly Sales")
print(monthly_sales)


# SQL QUERY 3: Top 10 products
top_products <- dbGetQuery(
  con,
  "
  SELECT
    stock_code,
    SUM(quantity) AS total_quantity,
    SUM(revenue) AS total_revenue
  FROM sales
  GROUP BY stock_code
  ORDER BY total_revenue DESC
  LIMIT 10
  "
)

print("Top 10 Products")
print(top_products)


# Save SQL results
write.csv(
  sales_kpis,
  "reports/sql_sales_kpis.csv",
  row.names = FALSE
)

write.csv(
  monthly_sales,
  "reports/sql_monthly_sales.csv",
  row.names = FALSE
)

write.csv(
  top_products,
  "reports/sql_top_products.csv",
  row.names = FALSE
)

# Check database tables
print(dbListTables(con))

# Disconnect database
dbDisconnect(con)

cat("Phase 11.8 completed successfully!\n")
