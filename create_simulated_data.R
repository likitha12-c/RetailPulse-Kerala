dir.create(
  
  "data/raw",
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)

dir.create(
  
  "data/processed",
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)

dir.create(
  
  "data/simulated",
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)



library(tidyverse)

set.seed(123)

stores <- tibble(
  
  store_id = c(
    
    "EKM01", "TVM01", "KZD01",
    
    "TSR01", "KTM01"
    
  ),
  
  store_name = c(
    
    "Ernakulam Central",
    
    "Trivandrum City",
    
    "Kozhikode Main",
    
    "Thrissur Central",
    
    "Kottayam Main"
    
  ),
  
  district = c(
    
    "Ernakulam",
    
    "Thiruvananthapuram",
    
    "Kozhikode",
    
    "Thrissur",
    
    "Kottayam"
    
  ),
  
  region = "Kerala"
  
)

write_csv(
  
  stores,
  
  "data/simulated/stores.csv"
  
)


products <- tibble(
  
  product_id = paste0(
    
    "P", sprintf("%03d", 1:100)
    
  ),
  
  product_name = paste(
    
    "Product",
    
    1:100
    
  ),
  
  category = sample(
    
    c(
      
      "Groceries",
      
      "Electronics",
      
      "Clothing",
      
      "Household",
      
      "Personal Care"
      
    ),
    
    100,
    
    replace = TRUE
    
  ),
  
  cost_price = round(
    
    runif(100, 50, 2000),
    
    2
    
  )
  
)

write_csv(
  
  products,
  
  "data/simulated/products.csv"
  
)

suppliers <- tibble(
  
  supplier_id = paste0(
    
    "SUP", 1:20
    
  ),
  
  supplier_name = paste(
    
    "Supplier",
    
    1:20
    
  ),
  
  lead_time = sample(
    
    2:15,
    
    20,
    
    replace = TRUE
    
  )
  
)

write_csv(
  
  suppliers,
  
  "data/simulated/suppliers.csv"
  
)

inventory <- expand_grid(
  
  store_id = stores$store_id,
  
  product_id = products$product_id
  
) %>%
  
  mutate(
    
    stock_quantity = sample(
      
      0:500,
      
      n(),
      
      replace = TRUE
      
    ),
    
    reorder_level = sample(
      
      10:100,
      
      n(),
      
      replace = TRUE
      
    ),
    
    supplier_id = sample(
      
      suppliers$supplier_id,
      
      n(),
      
      replace = TRUE
      
    )
    
  )

write_csv(
  
  inventory,
  
  "data/simulated/inventory.csv"
  
)

promotions <- tibble(
  
  product_id = sample(
    
    products$product_id,
    
    50
    
  ),
  
  start_date = sample(
    
    seq(
      
      as.Date("2025-01-01"),
      
      as.Date("2026-12-01"),
      
      by = "day"
      
    ),
    
    50
    
  )
  
) %>%
  
  mutate(
    
    end_date = start_date + 7,
    
    discount = sample(
      
      seq(5, 40, 5),
      
      n(),
      
      replace = TRUE
      
    )
    
  )

write_csv(
  
  promotions,
  
  "data/simulated/promotions.csv"
  
)

holidays <- tibble(
  
  date = as.Date(c(
    
    "2025-04-14",
    
    "2025-08-26",
    
    "2025-12-25",
    
    "2026-04-14",
    
    "2026-08-15",
    
    "2026-12-25"
    
  )),
  
  festival_name = c(
    
    "Vishu",
    
    "Onam",
    
    "Christmas",
    
    "Vishu",
    
    "Onam",
    
    "Christmas"
    
  ),
  
  holiday_type = "Festival"
  
)

write_csv(
  
  holidays,
  
  "data/simulated/holidays.csv"
  
)