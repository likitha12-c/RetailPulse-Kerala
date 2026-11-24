library(tidyverse)
set.seed(42)

sales  <- readRDS("data/processed/sales_clean.rds")
stores <- readRDS("data/processed/stores.rds")

# One random store per invoice (simulated)
invoice_store <- sales |>
  distinct(invoice) |>
  mutate(store_id = sample(stores$store_id, n(), replace = TRUE,
                           prob = runif(nrow(stores), 0.6, 1.4)))

sales_store <- sales |>
  left_join(invoice_store, by = "invoice") |>
  left_join(stores, by = "store_id")

saveRDS(sales_store, "data/processed/sales_store.rds")
print(count(sales_store, district))