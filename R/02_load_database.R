library(tidyverse)
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "data/retailpulse.sqlite")

# Public data: clean column names, dates as text (SQLite has no datetime type)
retail <- readRDS("data/processed/retail_imported.rds") |>
  rename(invoice = Invoice, stock_code = StockCode,
         description = Description, quantity = Quantity,
         invoice_date = InvoiceDate, unit_price = Price,
         customer_id = `Customer ID`, country = Country,
         source_sheet = SourceSheet) |>
  mutate(invoice_date = as.character(invoice_date))

dbWriteTable(con, "retail_raw", retail, overwrite = TRUE)

# Simulated data
dbWriteTable(con, "stores",     readRDS("data/processed/stores.rds"),     overwrite = TRUE)
dbWriteTable(con, "products",   readRDS("data/processed/products.rds"),   overwrite = TRUE)
dbWriteTable(con, "suppliers",  readRDS("data/processed/suppliers.rds"),  overwrite = TRUE)
dbWriteTable(con, "inventory",  readRDS("data/processed/inventory.rds"),  overwrite = TRUE)

dbWriteTable(con, "promotions",
             readRDS("data/processed/promotions.rds") |>
               mutate(across(where(~ inherits(.x, "Date")), as.character)),
             overwrite = TRUE)
dbWriteTable(con, "holidays",
             readRDS("data/processed/holidays.rds") |>
               mutate(across(where(~ inherits(.x, "Date")), as.character)),
             overwrite = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) AS n FROM retail_raw")
dbDisconnect(con)