# Process data for scatter plot
process_scatter_data <- function(df, x_var, y_var, z_var = NULL) {
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
  
  # Check if x and y variables are numeric
  if (!is.numeric(df[[x_var]])) {
    msg <- sprintf("Error: X variable '%s' must be numeric for scatter plot", x_var)
    plot_msg <- "Plot Status: Cannot be plotted - x variable is not numeric"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  if (!is.numeric(df[[y_var]])) {
    msg <- sprintf("Error: Y variable '%s' must be numeric for scatter plot", y_var)
    plot_msg <- "Plot Status: Cannot be plotted - y variable is not numeric"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check if z variable is numeric (if provided)
  if (!is.null(z_var) && !is.numeric(df[[z_var]])) {
    msg <- sprintf("Error: Z variable '%s' must be numeric for scatter plot", z_var)
    plot_msg <- "Plot Status: Cannot be plotted - z variable is not numeric"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Process data - select columns and remove NA values
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
    msg <<- paste("Error in data processing:", e$message)
    plot_msg <<- "Plot Status: Cannot be plotted - processing error"
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    msg <- "Error: No valid data points after processing"
    plot_msg <- "Plot Status: Cannot be plotted - no valid data"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Calculate correlation
  correlation <- cor(result[[x_var]], result[[y_var]], use = "complete.obs")
  
  # If everything succeeded
  msg <- sprintf("Success: Data processed successfully. Correlation: %.3f", correlation)
  plot_msg <- "Plot Status: Ready to plot"
  
  # Return the processed data
  return(list(
    data = result,
    message = msg,
    plot_message = plot_msg
  ))
}