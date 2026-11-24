
# ==========================================
# PHASE 11.7 - LOGGING AND ERROR HANDLING
# Retail Pulse Kerala
# ==========================================

library(logger)

# Create logs folder
dir.create(
  "logs",
  showWarnings = FALSE,
  recursive = TRUE
)

# Save log messages in a file
log_appender(
  appender_file("logs/retail_pulse.log")
)

# Set logging level
log_threshold(INFO)

# Function to safely process data
safe_process <- function(file_path) {
  
  log_info("Starting data processing")
  
  tryCatch({
    
    # Check whether file exists
    if (!file.exists(file_path)) {
      stop("Required data file not found")
    }
    
    # Load data
    data <- readRDS(file_path)
    
    # Check empty dataset
    if (nrow(data) == 0) {
      stop("Dataset is empty")
    }
    
    log_info("Data loaded successfully")
    log_info("Rows processed: {nrow(data)}")
    
    return(data)
    
  }, error = function(e) {
    
    log_error("Processing failed: {conditionMessage(e)}")
    
    return(NULL)
    
  })
}


# Run data processing
sales <- safe_process(
  "data/processed/sales_store.rds"
)

# Check result
if (!is.null(sales)) {
  
  log_info("Retail Pulse Kerala processing completed")
  
  print("Data processing successful!")
  
} else {
  
  log_warn("Data processing did not complete")
  
  print("Please check the log file.")
  
}

cat("Phase 11.7 execution finished!\n")
