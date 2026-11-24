
library(tidyverse)

set.seed(42)

# Load existing data
retail <- readRDS("data/processed/retail_imported.rds")
suppliers <- readRDS("data/processed/suppliers.rds")
stores <- readRDS("data/processed/stores.rds")

# Product categories
categories <- c(
  "Groceries", "Electronics", "Clothing",
  "Household", "Other"
)

# Build products dataset
products <- retail |>
  filter(Quantity > 0, Price > 0) |>
  mutate(
    StockCode = str_to_upper(str_trim(as.character(StockCode)))
  ) |>
  group_by(product_id = StockCode) |>
  summarise(
    product_name = first(Description),
    revenue = sum(Quantity * Price),
    median_price = median(Price),
    .groups = "drop"
  ) |>
  slice_max(revenue, n = 100) |>
  mutate(
    category = sample(categories, n(), replace = TRUE),
    cost_price = round(median_price * runif(n(), 0.55, 0.85), 2),
    supplier_id = sample(suppliers$supplier_id, n(), replace = TRUE)
  ) |>
  select(product_id, product_name, category, cost_price, supplier_id)

# Build inventory dataset
inventory <- expand_grid(
  store_id = stores$store_id,
  product_id = products$product_id
) |>
  mutate(
    stock_quantity = rpois(n(), 60),
    reorder_level = sample(10:40, n(), replace = TRUE)
  )

# Create output folders
dir.create("data/simulated", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Export datasets
write_csv(products, "data/simulated/products.csv")
write_csv(inventory, "data/simulated/inventory.csv")

saveRDS(products, "data/processed/products.rds")
saveRDS(inventory, "data/processed/inventory.rds")

# Check results
print(names(products))
print(nrow(inventory))

# Product matching percentage
matching_percentage <- mean(
  products$product_id %in%
    str_to_upper(str_trim(as.character(retail$StockCode)))
)

print(matching_percentage)

cat("Products and inventory rebuilt successfully!\n")
