
# ==========================================
# PHASE 11.5 - UNIT TESTING
# Retail Pulse Kerala
# ==========================================

library(testthat)
library(dplyr)

# Load reusable functions
source(
  if (dir.exists("R")) {
    "R/13_reusable_functions.R"
  } else {
    "13_reusable_functions.R"
  }
)

# Test 1: Check required columns
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


# Test 2: Calculate sales KPIs
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


# Test 3: Missing columns should produce an error
test_that("Missing columns produce an error", {
  
  sample_data <- data.frame(
    revenue = c(100, 200)
  )
  
  expect_error(
    calculate_sales_kpis(sample_data),
    "Missing columns"
  )
})


cat("Phase 11.5 tests finished!\n")

