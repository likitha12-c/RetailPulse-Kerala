
# ==========================================
# PHASE 11.3 - DATA.TABLE PROCESSING
# Retail Pulse Kerala
# ==========================================

library(data.table)

# Load retail sales data
sales <- readRDS("data/processed/sales_store.rds")

# Convert data frame into data.table
sales_dt <- as.data.table(sales)

# Display dataset information
cat("Total rows:", nrow(sales_dt), "\n")
cat("Total columns:", ncol(sales_dt), "\n")

# Calculate total revenue and quantity
sales_summary <- sales_dt[
  ,
  .(
    total_revenue = sum(revenue, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    total_orders = uniqueN(invoice)
  )
]

# Display summary
print(sales_summary)

# Monthly revenue analysis
sales_dt[, month := format(as.Date(invoice_day), "%Y-%m")]

monthly_sales <- sales_dt[
  ,
  .(
    monthly_revenue = sum(revenue, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE)
  ),
  by = month
]

# Sort monthly results
setorder(monthly_sales, month)

print(monthly_sales)

# Save results
fwrite(
  monthly_sales,
  "reports/datatable_monthly_sales.csv"
)

fwrite(
  sales_summary,
  "reports/datatable_sales_summary.csv"
)

cat("Phase 11.3 completed successfully!\n")
