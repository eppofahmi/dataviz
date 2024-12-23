aggregate_map_data <- function(df, longitude_var, latitude_var, value_var, agg = "none") {
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
  required_vars <- c(longitude_var, latitude_var, value_var)
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    msg <- sprintf("Error: Variables not found in data frame: %s", 
                   paste(missing_vars, collapse = ", "))
    plot_msg <- "Plot Status: Cannot be plotted - missing variables"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check aggregation type
  if (!agg %in% c("none", "sum", "count")) {
    msg <- sprintf("Error: Invalid aggregation type '%s'. Must be one of: none, sum, count", agg)
    plot_msg <- "Plot Status: Cannot be plotted - invalid aggregation"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Check coordinate bounds
  result <- tryCatch({
    valid_data <- df %>%
      filter(!is.na(!!sym(longitude_var)), 
             !is.na(!!sym(latitude_var)),
             !is.na(!!sym(value_var))) %>%
      filter(between(!!sym(longitude_var), 95, 141)) %>%  # Indonesia longitude bounds
      filter(between(!!sym(latitude_var), -11, 6)) %>%    # Indonesia latitude bounds
      select(!!sym(longitude_var), !!sym(latitude_var), !!sym(value_var))
    
    if (nrow(valid_data) == 0) {
      msg <- "Error: No valid data points within Indonesia's boundaries"
      plot_msg <- "Plot Status: Cannot be plotted - no valid coordinates"
      return(NULL)
    }
    
    valid_data
  }, error = function(e) {
    msg <<- paste("Error in validating coordinates:", e$message)
    plot_msg <<- "Plot Status: Cannot be plotted - coordinate validation failed"
    return(NULL)
  })
  
  if (is.null(result)) {
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Apply aggregation based on type
  if (agg == "count") {
    result <- tryCatch({
      result %>%
        group_by(!!sym(longitude_var), !!sym(latitude_var)) %>%
        summarise(
          value = n(),
          .groups = "drop"
        ) %>%
        rename(
          longitude = !!longitude_var,
          latitude = !!latitude_var
        )
    }, error = function(e) {
      msg <<- paste("Error in count aggregation:", e$message)
      plot_msg <<- "Plot Status: Cannot be plotted - aggregation failed"
      return(NULL)
    })
    
    if (!is.null(result)) {
      plot_msg <- "Plot Status: Can be plotted - count is numeric"
    }
    
  } else if (agg == "sum") {
    # Check if value_var is numeric
    if (!is.numeric(df[[value_var]])) {
      msg <- sprintf("Error: Variable '%s' must be numeric for sum aggregation", value_var)
      plot_msg <- "Plot Status: Cannot be plotted - non-numeric value variable"
      return(list(data = NULL, message = msg, plot_message = plot_msg))
    }
    
    result <- tryCatch({
      result %>%
        group_by(!!sym(longitude_var), !!sym(latitude_var)) %>%
        summarise(
          value = sum(!!sym(value_var), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(
          longitude = !!longitude_var,
          latitude = !!latitude_var
        )
    }, error = function(e) {
      msg <<- paste("Error in sum aggregation:", e$message)
      plot_msg <<- "Plot Status: Cannot be plotted - aggregation failed"
      return(NULL)
    })
    
    if (!is.null(result)) {
      plot_msg <- "Plot Status: Can be plotted - sum is numeric"
    }
    
  } else {
    # For "none" aggregation, check if value variable is numeric for plotting
    result <- result %>%
      rename(
        longitude = !!longitude_var,
        latitude = !!latitude_var,
        value = !!value_var
      )
    
    plot_msg <- if(is.numeric(df[[value_var]])) {
      "Plot Status: Can be plotted - value variable is numeric"
    } else {
      "Plot Status: Cannot be plotted - value variable is not numeric"
    }
  }
  
  if (is.null(result)) {
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # If everything succeeded
  msg <- "Success: Data aggregated successfully"
  
  # Return result, message, and plot feasibility
  return(list(
    data = result,
    message = msg,
    plot_message = plot_msg
  ))
}

# sample
# df2 = aggregate_map_data(
#   df = sample_data, 
#   longitude_var = "longitude",
#   latitude_var = "latitude", 
#   value_var = "customer_rating",
#   agg = "sum")
# 
# glimpse(df2)
