library(tidyverse)
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "data/retailpulse.sqlite")
dict <- map_dfr(dbListTables(con), function(t) {
  info <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))
  tibble(table = t, column = info$name, type = info$type)
})
dbDisconnect(con)

dict <- dict |>
  mutate(source = if_else(table == "retail_raw",
                          "Public (UCI Online Retail II)",
                          "Simulated (generated in R)"),
         description = "")   # fill in next

dir.create("documentation", showWarnings = FALSE)
write_csv(dict, "documentation/data_dictionary.csv")
View(dict)