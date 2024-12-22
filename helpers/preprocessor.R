aggregate_data <- function(df, x_var, y_var, z_var = NULL, agg = "none") {
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
  
  # Check if required variables exist in dataframe
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
  
  # Check aggregation type
  if (!agg %in% c("none", "sum", "count")) {
    msg <- sprintf("Error: Invalid aggregation type '%s'. Must be one of: none, sum, count", agg)
    plot_msg <- "Plot Status: Cannot be plotted - invalid aggregation"
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Base selection of variables
  result <- tryCatch({
    if (is.null(z_var)) {
      df %>% select(!!sym(x_var), !!sym(y_var))
    } else {
      df %>% select(!!sym(x_var), !!sym(y_var), !!sym(z_var))
    }
  }, error = function(e) {
    msg <<- paste("Error in selecting variables:", e$message)
    plot_msg <<- "Plot Status: Cannot be plotted - error in data selection"
    return(NULL)
  })
  
  if (is.null(result)) {
    return(list(data = NULL, message = msg, plot_message = plot_msg))
  }
  
  # Apply aggregation based on type
  if (agg == "count") {
    result <- tryCatch({
      # Create new column name for count aggregation
      count_col_name <- if(x_var == y_var) {
        paste0("count_of_", y_var)
      } else {
        y_var
      }
      
      if (is.null(z_var)) {
        result %>%
          group_by(!!sym(x_var)) %>%
          summarise(
            !!count_col_name := n_distinct(!!sym(y_var)),
            .groups = "drop"
          )
      } else {
        result %>%
          group_by(!!sym(x_var), !!sym(z_var)) %>%
          summarise(
            !!count_col_name := n_distinct(!!sym(y_var)),
            .groups = "drop"
          )
      }
    }, error = function(e) {
      msg <<- paste("Error in count aggregation:", e$message)
      plot_msg <<- "Plot Status: Cannot be plotted - aggregation failed"
      return(NULL)
    })
    
    if (!is.null(result)) {
      plot_msg <- "Plot Status: Can be plotted - count is numeric"
    }
    
  } else if (agg == "sum") {
    # Check if y_var is numeric
    if (!is.numeric(df[[y_var]])) {
      msg <- sprintf("Error: Variable '%s' must be numeric for sum aggregation", y_var)
      plot_msg <- "Plot Status: Cannot be plotted - non-numeric y-variable"
      return(list(data = NULL, message = msg, plot_message = plot_msg))
    }
    
    result <- tryCatch({
      if (is.null(z_var)) {
        result %>%
          group_by(!!sym(x_var)) %>%
          summarise(
            !!y_var := sum(!!sym(y_var), na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        result %>%
          group_by(!!sym(x_var), !!sym(z_var)) %>%
          summarise(
            !!y_var := sum(!!sym(y_var), na.rm = TRUE),
            .groups = "drop"
          )
      }
    }, error = function(e) {
      msg <<- paste("Error in sum aggregation:", e$message)
      plot_msg <<- "Plot Status: Cannot be plotted - aggregation failed"
      return(NULL)
    })
    
    if (!is.null(result)) {
      plot_msg <- "Plot Status: Can be plotted - sum is numeric"
    }
    
  } else {
    # For "none" aggregation, check if y variable is numeric for plotting
    plot_msg <- if(is.numeric(df[[y_var]])) {
      "Plot Status: Can be plotted - y-variable is numeric"
    } else {
      "Plot Status: Cannot be plotted - y-variable is not numeric"
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

# Example usage:
# # Create sample data
# sample_data <- data.frame(
#   region = c("A", "A", "B", "B", "C"),
#   sales = c(10, 20, 15, 25, 30),
#   category = c("X", "Y", "X", "Y", "X")
# )
# 
# # Test cases
# # 1. Two variables - No aggregation
# print("Test 1 - Two variables, no aggregation:")
# aggregate_data(sample_data, "region", "sales")
# 
# # 2. Two variables - Count
# print("\nTest 2 - Two variables, count:")
# aggregate_data(sample_data, "region", "sales", agg = "count")
# 
# # 3. Two variables - Sum
# print("\nTest 3 - Two variables, sum:")
# aggregate_data(sample_data, "region", "province", "category", agg = "count")
# 
# # 4. Three variables - No aggregation
# print("\nTest 4 - Three variables, no aggregation:")
# aggregate_data(sample_data, "region", "sales", "category", "none")
# 
# # 5. Three variables - Count
# print("\nTest 5 - Three variables, count:")
# aggregate_data(sample_data, "region", "sales", "category", "count")
# 
# # 6. Three variables - Sum
# print("\nTest 6 - Three variables, sum:")
# aggregate_data(sample_data, "region", "sales", "category", "sum")
# 
# # Error cases
# # 7. Invalid column with two variables
# print("\nTest 7 - Invalid column, two variables:")
# aggregate_data(sample_data, "invalid_col", "sales", agg = "sum")
# 
# # 8. Non-numeric y-variable with two variables
# sample_data_char <- data.frame(
#   region = c("A", "A", "B", "B", "C"),
#   product = c("P1", "P2", "P3", "P4", "P5"),
#   category = c("X", "Y", "X", "Y", "X")
# )
# 
# print("\nTest 8 - Non-numeric y-variable, two variables:")
# aggregate_data(sample_data_char, "region", "product", agg = "sum")
