# Function to process data for boxplots
process_boxplot_data <- function(df, y_var, group_var, agg = "none") {
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
  required_vars <- c(y_var, group_var)
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    msg <- sprintf("Error: Variables not found in data frame: %s", 
                   paste(missing_vars, collapse = ", "))
    plot_msg <- "Plot Status: Cannot be plotted - missing variables"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check if y variable is numeric
  if (!is.numeric(df[[y_var]])) {
    msg <- sprintf("Error: Value variable '%s' must be numeric for boxplot", y_var)
    plot_msg <- "Plot Status: Cannot be plotted - value variable is not numeric"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check aggregation type
  if (!agg %in% c("none", "sum", "count")) {
    msg <- sprintf("Error: Invalid aggregation type '%s'. Must be one of: none, sum, count", agg)
    plot_msg <- "Plot Status: Cannot be plotted - invalid aggregation"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Process data based on aggregation type
  result <- tryCatch({
    if (agg == "none") {
      # No aggregation, just select and filter NA
      df %>%
        select(!!sym(group_var), !!sym(y_var)) %>%
        filter(!is.na(!!sym(group_var)), !is.na(!!sym(y_var)))
    } else if (agg == "count") {
      # Count aggregation
      df %>%
        group_by(!!sym(group_var)) %>%
        summarise(
          !!y_var := n(),
          .groups = "drop"
        )
    } else if (agg == "sum") {
      # Sum aggregation
      df %>%
        group_by(!!sym(group_var)) %>%
        summarise(
          !!y_var := sum(!!sym(y_var), na.rm = TRUE),
          .groups = "drop"
        )
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
  
  # Calculate summary statistics for plotting
  stats_data <- result %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      min = min(!!sym(y_var), na.rm = TRUE),
      q1 = quantile(!!sym(y_var), 0.25, na.rm = TRUE),
      median = median(!!sym(y_var), na.rm = TRUE),
      q3 = quantile(!!sym(y_var), 0.75, na.rm = TRUE),
      max = max(!!sym(y_var), na.rm = TRUE),
      count = n(),
      mean = mean(!!sym(y_var), na.rm = TRUE),
      sd = sd(!!sym(y_var), na.rm = TRUE),
      .groups = "drop"
    )
  
  # If everything succeeded
  msg <- "Success: Data processed successfully for boxplot"
  plot_msg <- sprintf("Plot Status: Can be plotted - %s aggregation", 
                      if(agg == "none") "no" else agg)
  
  # Return the processed data and summary statistics
  return(list(
    data = result,
    stats = stats_data,
    message = msg,
    plot_message = plot_msg
  ))
}