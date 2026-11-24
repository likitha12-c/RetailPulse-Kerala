# Retail Pulse Kerala - Phase 10 Shiny Application
# Run from the Retail_pulse_kerala project folder with:
# shiny::runApp()

# Install once if needed:
# install.packages(c("shiny", "bslib", "tidyverse", "lubridate",
#                    "plotly", "DT", "scales", "htmlwidgets"))

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(htmlwidgets)

# -------------------------------------------------------------------
# 1. Load project outputs safely
# -------------------------------------------------------------------
read_rds_safe <- function(path) {
  if (file.exists(path)) readRDS(path) else tibble()
}

read_csv_safe <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else {
    tibble()
  }
}

# Load original retail dataset
sales <- readRDS("data/processed/retail_imported.rds")

# Load supporting datasets
inventory <- read_rds_safe("data/processed/inventory.rds")
products <- read_rds_safe("data/processed/products.rds")
stores <- read_rds_safe("data/processed/stores.rds")
holidays <- read_rds_safe("data/processed/holidays.rds")

# Load Phase 9 reports
churn <- read_csv_safe("reports/churn_risk_predictions.csv")
profitability <- read_csv_safe("reports/product_profitability.csv")
anomalies <- read_csv_safe("reports/anomaly_alerts.csv")
recommendations <- read_csv_safe("reports/recommendations.csv")

# Standardise sales column names
sales <- sales |>
  mutate(
    invoice_no = as.character(Invoice),
    stock_code = str_to_upper(str_trim(as.character(StockCode))),
    description = as.character(Description),
    quantity = as.numeric(Quantity),
    invoice_day = as.Date(
      lubridate::parse_date_time(
        as.character(InvoiceDate),
        orders = c("ymd HMS", "mdy HMS", "dmy HMS",
                   "ymd HM", "mdy HM", "dmy HM")
      )
    ),
    unit_price = as.numeric(Price),
    customer_id = as.character(`Customer ID`),
    store_name = as.character(Country)
  )

# Add product categories
if (has_data(products) &&
    all(c("product_id", "category") %in% names(products))) {
  
  products <- products |>
    mutate(product_id = str_to_upper(str_trim(as.character(product_id))))
  
  sales <- sales |>
    left_join(
      products |> select(product_id, category),
      by = c("stock_code" = "product_id")
    )
}

# Create inventory status
if (has_data(inventory)) {
  inventory <- inventory |>
    mutate(
      fc_status = if_else(
        stock_quantity <= reorder_level,
        "Critical stockout",
        "Available"
      )
    )
}

# Normalise optional report columns
if (has_data(churn)) {
  for (nm in intersect(
    c("monetary", "churn_risk", "recency", "frequency"),
    names(churn)
  )) {
    churn[[nm]] <- readr::parse_number(as.character(churn[[nm]]))
  }
}

if (has_data(anomalies) && "invoice_day" %in% names(anomalies)) {
  anomalies$invoice_day <- as.Date(anomalies$invoice_day)
}


# Normalise commonly used fields without assuming every optional column exists.
if (nrow(sales) > 0) {
  if ("invoice_day" %in% names(sales)) sales$invoice_day <- as.Date(sales$invoice_day)
  if ("stock_code" %in% names(sales)) sales$stock_code <- as.character(sales$stock_code)
  if ("quantity" %in% names(sales)) sales$quantity <- suppressWarnings(as.numeric(sales$quantity))
  if ("unit_price" %in% names(sales)) sales$unit_price <- suppressWarnings(as.numeric(sales$unit_price))
  if ("store_name" %in% names(sales)) sales$store_name <- as.character(sales$store_name)
}
if (nrow(inventory) > 0) {
  for (nm in intersect(c("stock_quantity", "fc_demand_90", "fc_demand_cover",
                         "fc_reorder_qty", "cost_price", "lead_time"), names(inventory))) {
    inventory[[nm]] <- suppressWarnings(as.numeric(inventory[[nm]]))
  }
}
if (nrow(anomalies) > 0 && "invoice_day" %in% names(anomalies)) {
  anomalies$invoice_day <- as.Date(anomalies$invoice_day)
}
if (nrow(churn) > 0) {
  for (nm in intersect(c("monetary", "churn_risk", "recency", "frequency"), names(churn))) {
    churn[[nm]] <- readr::parse_number(as.character(churn[[nm]]))
  }
}

# -------------------------------------------------------------------
# 2. Helper functions
# -------------------------------------------------------------------
has_data <- function(x) is.data.frame(x) && nrow(x) > 0
has_cols <- function(x, cols) all(cols %in% names(x))

empty_message <- function(msg = "No data available. Run the relevant project phase first.") {
  div(class = "empty-state", icon("circle-info"), p(msg))
}

money <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("—")
  scales::dollar(sum(x, na.rm = TRUE), prefix = "£", accuracy = 1)
}

make_card <- function(label, value, note = NULL) {
  bslib::card(
    bslib::card_body(
      div(class = "kpi-label", label),
      div(class = "kpi-value", value),
      if (!is.null(note)) div(class = "kpi-note", note)
    )
  )
}

sales_filtered <- function(data, dates, store, category, search) {
  if (!has_data(data)) return(tibble())
  out <- data
  if ("invoice_day" %in% names(out) && length(dates) == 2 && !any(is.na(dates))) {
    out <- out |> filter(is.na(invoice_day) | (invoice_day >= dates[1] & invoice_day <= dates[2]))
  }
  if (store != "All stores" && "store_name" %in% names(out)) {
    out <- out |> filter(store_name == store)
  }
  if (category != "All categories" && "category" %in% names(out)) {
    out <- out |> filter(.data[["category"]] == .env$category)
  }
  if (nzchar(search)) {
    searchable <- intersect(c("description", "product_name", "stock_code", "product_id"), names(out))
    if (length(searchable) > 0) {
      keep <- rep(FALSE, nrow(out))
      for (nm in searchable) keep <- keep | stringr::str_detect(
        stringr::str_to_lower(as.character(out[[nm]])),
        stringr::fixed(stringr::str_to_lower(search))
      )
      out <- out[keep, , drop = FALSE]
    }
  }
  out
}

make_sales_chart <- function(data) {
  if (!has_data(data)) return(NULL)
  if (has_cols(data, c("invoice_day", "quantity", "unit_price"))) {
    data |>
      mutate(revenue_value = quantity * unit_price) |>
      group_by(invoice_day) |>
      summarise(revenue = sum(revenue_value, na.rm = TRUE), .groups = "drop") |>
      plot_ly(x = ~invoice_day, y = ~revenue, type = "scatter", mode = "lines",
              line = list(width = 2)) |>
      layout(title = "Revenue trend", xaxis = list(title = "Date"),
             yaxis = list(title = "Revenue (£)"))
  } else {
    plot_ly() |> layout(title = "Sales date, quantity and unit_price columns are required")
  }
}

# -------------------------------------------------------------------
# 3. User interface
# -------------------------------------------------------------------
ui <- navbarPage(
  title = div(class = "brand-title", "RETAIL PULSE ", span("KERALA")),
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#176B66",
                   secondary = "#E7A83E", base_font = font_google("Inter")),
  header = tagList(
    tags$head(tags$style(HTML("
      body { background:#f4f7f8; }
      .navbar { box-shadow:0 2px 12px rgba(20,40,50,.08); }
      .brand-title {font-weight:800; letter-spacing:1px;}
      .brand-title span {color:#e7a83e;}
      .page-wrap {padding:18px 24px 35px;}
      .page-heading {font-weight:800; color:#173b3a; margin:8px 0 4px;}
      .page-subtitle {color:#66777b; margin-bottom:18px;}
      .kpi-label {font-size:12px; text-transform:uppercase; letter-spacing:1px; color:#687b7d;}
      .kpi-value {font-size:27px; font-weight:800; color:#173b3a; margin-top:6px;}
      .kpi-note {font-size:12px; color:#7a898b; margin-top:4px;}
      .card {border:0; border-radius:14px; box-shadow:0 3px 15px rgba(24,55,60,.06); margin-bottom:15px;}
      .empty-state {padding:35px; text-align:center; color:#718184; background:white; border-radius:14px;}
      .filter-panel {background:white; padding:15px; border-radius:14px; margin-bottom:16px;}
      .section-title {font-weight:700; color:#244b49; margin:12px 0;}
      .small-note {font-size:12px;color:#718184;}
    ")))
  ),
  tabPanel("Executive Overview",
    div(class = "page-wrap",
      h2(class = "page-heading", "Executive Overview"),
      p(class = "page-subtitle", "A high-level view of retail performance across the selected period."),
      uiOutput("global_filters"),
      layout_column_wrap(
        width = 1/4,
        uiOutput("kpi_revenue"), uiOutput("kpi_profit"),
        uiOutput("kpi_orders"), uiOutput("kpi_units")
      ),
      layout_columns(
        card(card_header("Revenue trend"), plotlyOutput("overview_trend", height = 300)),
        card(card_header("Top stores"), plotlyOutput("top_stores", height = 300))
      ),
      uiOutput("download_controls")
    )
  ),
  tabPanel("Sales Analytics",
    div(class = "page-wrap",
      h2(class = "page-heading", "Sales Analytics"),
      p(class = "page-subtitle", "Explore sales trends, products and categories."),
      uiOutput("sales_filters"),
      layout_columns(
        card(card_header("Sales trend"), plotlyOutput("sales_trend", height = 330)),
        card(card_header("Category performance"), plotlyOutput("category_chart", height = 330))
      ),
      card(card_header("Top products"), DTOutput("top_products"))
    )
  ),
  tabPanel("Store Analytics",
    div(class = "page-wrap",
      h2(class = "page-heading", "Store Analytics"),
      p(class = "page-subtitle", "Compare store performance and district-level results where available."),
      card(card_header("Store comparison"), plotlyOutput("store_chart", height = 380)),
      card(card_header("Store data"), DTOutput("store_table"))
    )
  ),
  tabPanel("Customer Intelligence",
    div(class = "page-wrap",
      h2(class = "page-heading", "Customer Intelligence"),
      p(class = "page-subtitle", "Review customer value and churn-risk indicators."),
      layout_column_wrap(width = 1/3,
        uiOutput("customer_count"), uiOutput("high_risk_count"), uiOutput("customer_value")
      ),
      card(card_header("Customer churn-risk data"), DTOutput("churn_table"))
    )
  ),
  tabPanel("Inventory Monitoring",
    div(class = "page-wrap",
      h2(class = "page-heading", "Inventory Monitoring"),
      p(class = "page-subtitle", "Monitor available stock, forecast demand and replenishment status."),
      layout_column_wrap(width = 1/3,
        uiOutput("inventory_count"), uiOutput("stockout_count"), uiOutput("overstock_count")
      ),
      card(card_header("Inventory status"), plotlyOutput("inventory_chart", height = 320)),
      card(card_header("Inventory details"), DTOutput("inventory_table"))
    )
  ),
  tabPanel("Demand Forecasting",
    div(class = "page-wrap",
      h2(class = "page-heading", "Demand Forecasting"),
      p(class = "page-subtitle", "Inspect forecast outputs produced by the forecasting phase."),
      selectInput("forecast_horizon", "Forecast horizon", choices = c("7 days", "30 days", "90 days"), selected = "90 days"),
      card(card_header("Forecast summary"), plotlyOutput("forecast_chart", height = 350)),
      card(card_header("Forecast data"), DTOutput("forecast_table")),
      p(class = "small-note", "This page displays saved model outputs. It does not retrain the forecasting model.")
    )
  ),
  tabPanel("Machine Learning",
    div(class = "page-wrap",
      h2(class = "page-heading", "Machine Learning"),
      p(class = "page-subtitle", "Review saved churn predictions and model metrics when available."),
      card(card_header("Churn risk distribution"), plotlyOutput("ml_chart", height = 320)),
      card(card_header("Prediction data"), DTOutput("ml_table")),
      p(class = "small-note", "Model comparison and performance metrics will appear when metric files are exported by the modelling phase.")
    )
  ),
  tabPanel("Anomaly Monitoring",
    div(class = "page-wrap",
      h2(class = "page-heading", "Anomaly Monitoring"),
      p(class = "page-subtitle", "Identify unusual sales records and review alert severity."),
      card(card_header("Anomaly alerts"), plotlyOutput("anomaly_chart", height = 320)),
      card(card_header("Alert details"), DTOutput("anomaly_table"))
    )
  ),
  tabPanel("Recommendations",
    div(class = "page-wrap",
      h2(class = "page-heading", "Management Recommendations"),
      p(class = "page-subtitle", "Prioritised actions generated by Phase 9."),
      selectInput("rec_priority", "Priority", choices = c("All", "Critical", "High", "Medium", "Low")),
      card(card_header("Recommendation priority"), plotlyOutput("rec_chart", height = 300)),
      card(card_header("Recommended actions"), DTOutput("rec_table"))
    )
  ),
  tabPanel("Data Quality",
    div(class = "page-wrap",
      h2(class = "page-heading", "Data Quality"),
      p(class = "page-subtitle", "Basic validation checks for the files loaded by this application."),
      card(card_header("File availability"), DTOutput("quality_files")),
      card(card_header("Sales data checks"), DTOutput("quality_sales")),
      p(class = "small-note", "These checks are descriptive and do not replace the detailed data-cleaning report.")
    )
  ),
  footer = div(class = "page-wrap small-note", "Retail Pulse Kerala | Decision-support dashboard")
)

# -------------------------------------------------------------------
# 4. Server: filters and reactive data
# -------------------------------------------------------------------
server <- function(input, output, session) {

  store_choices <- if (has_data(sales) && "store_name" %in% names(sales)) {
    sort(unique(na.omit(as.character(sales$store_name))))
  } else character(0)
  category_choices <- if (has_data(sales) && "category" %in% names(sales)) {
    sort(unique(na.omit(as.character(sales$category))))
  } else character(0)
  date_limits <- if (has_data(sales) && "invoice_day" %in% names(sales) &&
                     any(!is.na(sales$invoice_day))) {
    range(sales$invoice_day, na.rm = TRUE)
  } else c(Sys.Date() - 30, Sys.Date())

  output$global_filters <- renderUI({
    div(class = "filter-panel",
      fluidRow(
        column(4, dateRangeInput("date_range", "Date range",
          start = date_limits[1], end = date_limits[2],
          min = date_limits[1], max = date_limits[2])),
        column(4, selectInput("store_filter", "Store",
          choices = c("All stores", store_choices), selected = "All stores")),
        column(4, selectInput("category_filter", "Category",
          choices = c("All categories", category_choices), selected = "All categories"))
      )
    )
  })

  output$sales_filters <- renderUI({
    div(class = "filter-panel",
      fluidRow(
        column(4, dateRangeInput("sales_dates", "Date range",
          start = date_limits[1], end = date_limits[2],
          min = date_limits[1], max = date_limits[2])),
        column(4, selectInput("sales_store", "Store",
          choices = c("All stores", store_choices), selected = "All stores")),
        column(4, textInput("product_search", "Search product", placeholder = "Enter product name or code"))
      )
    )
  })

  filtered_sales <- reactive({
    req(input$date_range)
    sales_filtered(sales, input$date_range, input$store_filter %||% "All stores",
                   input$category_filter %||% "All categories", "")
  })
  filtered_sales_page <- reactive({
    req(input$sales_dates)
    sales_filtered(sales, input$sales_dates, input$sales_store %||% "All stores",
                   "All categories", input$product_search %||% "")
  })

  revenue_total <- function(d) {
    if (has_cols(d, c("quantity", "unit_price"))) sum(d$quantity * d$unit_price, na.rm = TRUE) else NA_real_
  }
  profit_total <- function(d) {
    if ("profit" %in% names(d)) sum(d$profit, na.rm = TRUE) else NA_real_
  }

  output$kpi_revenue <- renderUI(make_card("Revenue", money(revenue_total(filtered_sales()))))
  output$kpi_profit <- renderUI(make_card("Profit", if (is.na(profit_total(filtered_sales()))) "Not available" else money(profit_total(filtered_sales()))))
  output$kpi_orders <- renderUI({
    d <- filtered_sales()
    n <- if ("invoice_no" %in% names(d)) n_distinct(d$invoice_no) else if ("order_id" %in% names(d)) n_distinct(d$order_id) else NA_integer_
    make_card("Orders", ifelse(is.na(n), "Not available", comma(n)))
  })
  output$kpi_units <- renderUI({
    d <- filtered_sales()
    make_card("Units sold", if ("quantity" %in% names(d)) comma(sum(d$quantity[d$quantity > 0], na.rm = TRUE)) else "Not available")
  })
  output$overview_trend <- renderPlotly(make_sales_chart(filtered_sales()))
  output$sales_trend <- renderPlotly(make_sales_chart(filtered_sales_page()))

  output$top_stores <- renderPlotly({
    d <- filtered_sales()
    validate(need(has_cols(d, c("store_name", "quantity", "unit_price")), "Store and sales columns are not available."))
    d |> mutate(revenue = quantity * unit_price) |>
      group_by(store_name) |> summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") |>
      slice_max(revenue, n = 10) |>
      plot_ly(x = ~reorder(store_name, revenue), y = ~revenue, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue (£)"))
  })

  output$category_chart <- renderPlotly({
    d <- filtered_sales_page()
    validate(need("category" %in% names(d), "A category column is not available in sales data."))
    validate(need(has_cols(d, c("quantity", "unit_price")), "Quantity or unit price is missing."))
    d |> mutate(revenue = quantity * unit_price) |>
      group_by(category) |> summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") |>
      plot_ly(x = ~category, y = ~revenue, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue (£)"))
  })

  output$top_products <- renderDT({
    d <- filtered_sales_page()
    validate(need(has_cols(d, c("stock_code", "quantity", "unit_price")), "Product sales columns are unavailable."))
    d |> mutate(revenue = quantity * unit_price) |>
      group_by(stock_code) |>
      summarise(units = sum(quantity, na.rm = TRUE), revenue = sum(revenue, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(revenue)) |> head(20) |> datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$store_chart <- renderPlotly({
    validate(need(has_cols(sales, c("store_name", "quantity", "unit_price")), "Store sales data is unavailable."))
    sales |> mutate(revenue = quantity * unit_price) |>
      group_by(store_name) |> summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") |>
      plot_ly(x = ~reorder(store_name, revenue), y = ~revenue, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue (£)"))
  })
  output$store_table <- renderDT({
    validate(need(has_cols(sales, c("store_name", "quantity", "unit_price")), "Store sales data is unavailable."))
    sales |> mutate(revenue = quantity * unit_price) |>
      group_by(store_name) |> summarise(revenue = sum(revenue, na.rm = TRUE),
        transactions = n(), .groups = "drop") |> arrange(desc(revenue)) |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$customer_count <- renderUI(make_card("Customers", if (has_data(churn) && "customer_id" %in% names(churn)) comma(n_distinct(churn$customer_id)) else "Not available"))
  output$high_risk_count <- renderUI(make_card("High-risk customers", if (has_data(churn) && "risk_band" %in% names(churn)) comma(sum(churn$risk_band == "High", na.rm = TRUE)) else "Not available"))
  output$customer_value <- renderUI(make_card("Average customer spend", if (has_data(churn) && "monetary" %in% names(churn)) money(mean(churn$monetary, na.rm = TRUE)) else "Not available"))
  output$churn_table <- renderDT({
    validate(need(has_data(churn), "Churn predictions file is not available."))
    datatable(churn, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$inventory_count <- renderUI(make_card("Product-store records", if (has_data(inventory)) comma(nrow(inventory)) else "Not available"))
  output$stockout_count <- renderUI(make_card("Critical stockouts", if (has_data(inventory) && "fc_status" %in% names(inventory)) comma(sum(inventory$fc_status == "Critical stockout", na.rm = TRUE)) else "Not available"))
  output$overstock_count <- renderUI(make_card("Overstocked records", if (has_data(inventory) && "fc_status" %in% names(inventory)) comma(sum(inventory$fc_status == "Overstocked", na.rm = TRUE)) else "Not available"))
  output$inventory_chart <- renderPlotly({
    validate(need(has_data(inventory) && "fc_status" %in% names(inventory), "Inventory forecast data is unavailable."))
    inventory |> count(fc_status) |> plot_ly(x = ~fc_status, y = ~n, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Records"))
  })
  output$inventory_table <- renderDT({
    validate(need(has_data(inventory), "Inventory forecast data is unavailable."))
    datatable(inventory, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$forecast_chart <- renderPlotly({
    validate(need(has_data(inventory), "Forecast data is unavailable."))
    metric <- switch(input$forecast_horizon,
      "7 days" = intersect(c("fc_demand_7", "forecast_7", "demand_7"), names(inventory))[1],
      "30 days" = intersect(c("fc_demand_30", "forecast_30", "demand_30"), names(inventory))[1],
      "90 days" = intersect(c("fc_demand_90", "fc_demand_cover"), names(inventory))[1]
    )
    validate(need(length(metric) == 1 && !is.na(metric), "The selected forecast horizon column is not present in the saved inventory file."))
    d <- inventory |> group_by(product_name) |> summarise(forecast = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
      slice_max(forecast, n = 15)
    plot_ly(d, x = ~reorder(product_name, forecast), y = ~forecast, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Forecast units"))
  })
  output$forecast_table <- renderDT({
    validate(need(has_data(inventory), "Forecast data is unavailable."))
    datatable(inventory, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$ml_chart <- renderPlotly({
    validate(need(has_data(churn) && "risk_band" %in% names(churn), "Churn risk band data is unavailable."))
    churn |> count(risk_band) |> plot_ly(x = ~risk_band, y = ~n, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Customers"))
  })
  output$ml_table <- renderDT({
    validate(need(has_data(churn), "Churn prediction file is unavailable."))
    datatable(churn, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$anomaly_chart <- renderPlotly({
    validate(need(has_data(anomalies), "Anomaly alerts file is unavailable."))
    if ("severity" %in% names(anomalies)) {
      anomalies |> count(severity) |> plot_ly(x = ~severity, y = ~n, type = "bar")
    } else if ("direction" %in% names(anomalies)) {
      anomalies |> count(direction) |> plot_ly(x = ~direction, y = ~n, type = "bar")
    } else {
      plot_ly() |> layout(title = "No severity or direction column found")
    }
  })
  output$anomaly_table <- renderDT({
    validate(need(has_data(anomalies), "Anomaly alerts file is unavailable."))
    datatable(anomalies, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  filtered_recommendations <- reactive({
    if (!has_data(recommendations)) return(tibble())
    d <- recommendations
    if (input$rec_priority != "All" && "priority" %in% names(d)) {
      d <- d |> filter(priority == input$rec_priority)
    }
    d
  })
  output$rec_chart <- renderPlotly({
    d <- filtered_recommendations()
    validate(need(has_data(d) && "type" %in% names(d), "Recommendation output is unavailable. Run Phase 9 first."))
    d |> count(type, priority) |> plot_ly(x = ~type, y = ~n, color = ~priority, type = "bar") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Recommendations"))
  })
  output$rec_table <- renderDT({
    d <- filtered_recommendations()
    validate(need(has_data(d), "Recommendation output is unavailable. Run Phase 9 first."))
    datatable(d, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$quality_files <- renderDT({
    paths <- c("data/processed/sales_store.rds", "data/processed/inventory_forecast.rds",
      "data/processed/stores.rds", "data/processed/holidays.rds",
      "reports/churn_risk_predictions.csv", "reports/product_profitability.csv",
      "reports/anomaly_alerts.csv", "reports/recommendations.csv")
    tibble(file = paths, status = ifelse(file.exists(paths), "Available", "Missing")) |>
      datatable(options = list(dom = "t"), rownames = FALSE)
  })
  output$quality_sales <- renderDT({
    if (!has_data(sales)) {
      return(datatable(tibble(check = "Sales file", result = "Missing or empty"), options = list(dom = "t")))
    }
    tibble(
      check = c("Rows", "Columns", "Missing invoice dates", "Missing quantities",
                "Missing unit prices", "Duplicate rows"),
      result = c(nrow(sales), ncol(sales),
        if ("invoice_day" %in% names(sales)) sum(is.na(sales$invoice_day)) else "Column missing",
        if ("quantity" %in% names(sales)) sum(is.na(sales$quantity)) else "Column missing",
        if ("unit_price" %in% names(sales)) sum(is.na(sales$unit_price)) else "Column missing",
        sum(duplicated(sales)))
    ) |> datatable(options = list(dom = "t"), rownames = FALSE)
  })

  # -----------------------------------------------------------------
  # 5. Downloads: filtered sales, recommendations and HTML chart
  # -----------------------------------------------------------------
  output$download_sales <- downloadHandler(
    filename = function() paste0("retail_sales_filtered_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_sales(), file)
  )
  output$download_recommendations <- downloadHandler(
    filename = function() paste0("retail_recommendations_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_recommendations(), file)
  )
  output$download_chart <- downloadHandler(
    filename = function() paste0("retail_sales_chart_", Sys.Date(), ".html"),
    content = function(file) {
      p <- make_sales_chart(filtered_sales())
      if (is.null(p)) p <- plot_ly() |> layout(title = "No sales data available")
      htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
    }
  )

  # Download controls appear in the page footer area.
  output$download_controls <- renderUI({
    div(class = "filter-panel",
      h5("Download reports"),
      downloadButton("download_sales", "Filtered sales CSV"),
      downloadButton("download_recommendations", "Recommendations CSV"),
      downloadButton("download_chart", "Sales chart (HTML)")
    )
  })
}

# -------------------------------------------------------------------
# 6. Run application
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
