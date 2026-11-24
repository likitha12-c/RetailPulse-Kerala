
# ==========================================
# PHASE 11.6 - TARGETS PIPELINE
# Retail Pulse Kerala
# ==========================================

library(targets)
library(dplyr)

# Load reusable functions
source("R/11_reusable_functions.R")

# Define pipeline targets
list(
  
  # Target 1: Track the functions file
  tar_target(
    functions_file,
    "R/11_reusable_functions.R",
    format = "file"
  ),
  
  # Target 2: Load sales data
  tar_target(
    sales_data,
    {
      functions_file
      safe_read_rds("data/processed/sales_store.rds")
    }
  ),
  
  # Target 3: Calculate sales KPIs
  tar_target(
    sales_kpis,
    {
      functions_file
      calculate_sales_kpis(sales_data)
    }
  ),
  
  # Target 4: Calculate monthly sales
  tar_target(
    monthly_sales,
    {
      functions_file
      calculate_monthly_sales(sales_data)
    }
  ),
  
  # Target 5: Save monthly sales report
  tar_target(
    monthly_sales_file,
    {
      dir.create(
        "reports",
        showWarnings = FALSE,
        recursive = TRUE
      )
      
      output_file <- "reports/targets_monthly_sales.csv"
      
      write.csv(
        monthly_sales,
        output_file,
        row.names = FALSE
      )
      
      output_file
    },
    format = "file"
  )
  
)
