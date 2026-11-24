
library(testthat)
library(dplyr)

source(testthat::test_path("..", "..", "R", "13_reusable_functions.R"))

test_that("Required columns are available", {
  sample_data <- data.frame(
    revenue = c(100, 200),
    quantity = c(2, 4)
  )
  
  expect_true(
    check_required_columns(
      sample_data,
      c("revenue", "quantity")
    )
  )
})

test_that("Sales KPIs are calculated correctly", {
  sample_data <- data.frame(
    revenue = c(100, 200, 300),
    quantity = c(2, 3, 5)
  )
  
  result <- calculate_sales_kpis(sample_data)
  
  expect_equal(result$total_revenue, 600)
  expect_equal(result$total_quantity, 10)
  expect_equal(result$total_records, 3)
})

test_that("Missing columns produce an error", {
  sample_data <- data.frame(
    revenue = c(100, 200)
  )
  
  expect_error(
    calculate_sales_kpis(sample_data),
    "Missing columns"
  )
})
