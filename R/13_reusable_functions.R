
# ==========================================
# PHASE 11.1 - REUSABLE R FUNCTIONS
# Retail Pulse Kerala
# ==========================================

library(dplyr)

# Function 1: Check required columns
check_required_columns <- function(data, required_columns) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  return(TRUE)
}


# Function 2: Calculate sales KPIs
calculate_sales_kpis <- function(data) {
  
  check_required_columns(
    data,
    c("revenue", "quantity")
  )
  
  result <- data |>
    summarise(
      total_revenue = sum(revenue, na.rm = TRUE),
      total_quantity = sum(quantity, na.rm = TRUE),
      total_records = n()
    )
  
  return(result)
}


# Function 3: Calculate monthly sales
calculate_monthly_sales <- function(data) {
  
  check_required_columns(
    data,
    c("invoice_day", "revenue")
  )
  
  result <- data |>
    mutate(
      month = format(
        as.Date(invoice_day),
        "%Y-%m"
      )
    ) |>
    group_by(month) |>
    summarise(
      monthly_revenue = sum(revenue, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}


# Function 4: Safely read an RDS file
safe_read_rds <- function(file_path) {
  
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  readRDS(file_path)
}


print("Phase 11.1 functions loaded successfully!")
