library(tidyverse)
library(readxl)


file_path <- "data/raw/online_retail_II.xlsx"


if (!file.exists(file_path)) {
  stop("Dataset not found. Check the file location.")
}


sheets <- excel_sheets(file_path)

print(sheets)


retail_data <- map_dfr(
  sheets,
  function(sheet_name) {
    read_excel(
      file_path,
      sheet = sheet_name
    ) %>%
      mutate(SourceSheet = sheet_name)
  }
)


print(dim(retail_data))
glimpse(retail_data)
summary(retail_data)


dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  retail_data,
  "data/processed/retail_imported.rds"
)

print("Retail dataset imported successfully!")

stores <- read_csv("data/simulated/stores.csv", show_col_types = FALSE)
products <- read_csv("data/simulated/products.csv", show_col_types = FALSE)
suppliers <- read_csv("data/simulated/suppliers.csv", show_col_types = FALSE)
holidays <- read_csv("data/simulated/holidays.csv", show_col_types = FALSE)
promotions <- read_csv("data/simulated/promotions.csv", show_col_types = FALSE)
inventory <- read_csv("data/simulated/inventory.csv", show_col_types = FALSE)

# Check datasets
dim(stores)
dim(products)
dim(suppliers)
dim(holidays)
dim(promotions)
dim(inventory)

print("All simulated datasets imported successfully!")

dim(stores)
dim(products)
dim(suppliers)
dim(holidays)
dim(promotions)
dim(inventory)

glimpse(stores)
glimpse(products)
glimpse(suppliers)
glimpse(holidays)
glimpse(promotions)
glimpse(inventory)

# Save simulated datasets

saveRDS(stores, "data/processed/stores.rds")
saveRDS(products, "data/processed/products.rds")
saveRDS(suppliers, "data/processed/suppliers.rds")
saveRDS(holidays, "data/processed/holidays.rds")
saveRDS(promotions, "data/processed/promotions.rds")
saveRDS(inventory, "data/processed/inventory.rds")

print("All datasets saved successfully!")