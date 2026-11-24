
# ==========================================
# PHASE 11.2 - PURRR FUNCTIONAL WORKFLOW
# Retail Pulse Kerala
# ==========================================

library(dplyr)
library(purrr)

# Load datasets
sales <- readRDS("data/processed/sales_store.rds")
returns <- readRDS("data/processed/returns.rds")
products <- readRDS("data/processed/products.rds")
inventory <- readRDS("data/processed/inventory.rds")

# Store datasets in a list
retail_data <- list(
  sales = sales,
  returns = returns,
  products = products,
  inventory = inventory
)

# Function to check dataset information
get_dataset_info <- function(data) {
  
  tibble(
    rows = nrow(data),
    columns = ncol(data),
    missing_values = sum(is.na(data))
  )
}

# Apply function to every dataset using map()
dataset_summary <- map(
  retail_data,
  get_dataset_info
)

# Combine results into one table
summary_table <- imap_dfr(
  dataset_summary,
  ~ mutate(.x, dataset = .y, .before = 1)
)

# Display results
print(summary_table)

# Save results
write.csv(
  summary_table,
  "reports/purrr_dataset_summary.csv",
  row.names = FALSE
)

print("Phase 11.2 completed successfully!")
