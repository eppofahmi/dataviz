process_boxplot_data <- function(df, x_var, y_var, z_var = NULL) {
  # Initialize output message
  msg <- ""
  plot_msg <- ""
  result <- NULL
  
  # Check if dataframe is NULL or not a dataframe
  if (is.null(df)) {
    msg <- "Error: Input data frame is NULL"
    plot_msg <- "Plot Status: Cannot be plotted - no data available"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  if (!is.data.frame(df)) {
    msg <- "Error: Input must be a data frame"
    plot_msg <- "Plot Status: Cannot be plotted - invalid data format"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check if required variables exist in data frame
  required_vars <- c(x_var, y_var)
  if (!is.null(z_var)) {
    required_vars <- c(required_vars, z_var)
  }
  
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    msg <- sprintf("Error: Variables not found in data frame: %s", 
                   paste(missing_vars, collapse = ", "))
    plot_msg <- "Plot Status: Cannot be plotted - missing variables"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check if y variable is numeric (required for boxplot)
  if (!is.numeric(df[[y_var]])) {
    msg <- sprintf("Error: Y variable '%s' must be numeric for boxplot", y_var)
    plot_msg <- "Plot Status: Cannot be plotted - y variable is not numeric"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Base selection of variables and remove NA values
  result <- tryCatch({
    if (is.null(z_var)) {
      df %>% 
        select(!!sym(x_var), !!sym(y_var)) %>%
        filter(!is.na(!!sym(x_var)), !is.na(!!sym(y_var)))
    } else {
      df %>% 
        select(!!sym(x_var), !!sym(y_var), !!sym(z_var)) %>%
        filter(!is.na(!!sym(x_var)), !is.na(!!sym(y_var)), !is.na(!!sym(z_var)))
    }
  }, error = function(e) {
    msg <<- paste("Error in selecting variables:", e$message)
    plot_msg <<- "Plot Status: Cannot be plotted - error in data selection"
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    msg <- "Error: No valid data points after filtering NA values"
    plot_msg <- "Plot Status: Cannot be plotted - no valid data"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Calculate summary statistics for the boxplot
  stats <- result %>%
    group_by(!!sym(x_var)) %>%
    summarise(
      min = min(!!sym(y_var)),
      q1 = quantile(!!sym(y_var), 0.25),
      median = median(!!sym(y_var)),
      q3 = quantile(!!sym(y_var), 0.75),
      max = max(!!sym(y_var)),
      count = n(),
      .groups = "drop"
    )
  
  # If everything succeeded
  msg <- "Success: Data processed successfully for boxplot"
  plot_msg <- "Plot Status: Can be plotted - data prepared"
  
  # Return the processed data and summary statistics
  return(list(
    data = result,
    stats = stats,
    message = msg,
    plot_message = plot_msg
  ))
}

# Test the boxplot processing function
# result <- process_boxplot_data(
#   df = sample_data,
#   x_var = "sales_amount",
#   y_var = "quantity", 
#   z_var = "category"
# )
# result

