# Plot generation function
generate_plot <- function(data, x_var, y_var, z_var = NULL, plot_type = "column", agg_type = "none") {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Determine the actual y variable name
  # Use count_of_ prefix when:
  # 1. aggregation type is count AND
  # 2. x and y variables are the same
  # (regardless of grouping variable)
  actual_y_var <- if(agg_type == "count" && x_var == y_var) {
    paste0("count_of_", y_var)
  } else {
    y_var
  }
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  plot <- switch(
    plot_type,
    "column" = {
      if (!is.null(z_var)) {
        data %>%
          group_by(!!sym(z_var)) %>%
          e_charts_(x_var) %>%
          e_bar_(actual_y_var) %>%
          e_tooltip()
      } else {
        data %>%
          e_charts_(x_var) %>%
          e_bar_(actual_y_var) %>%
          e_tooltip()
      }
    },
    "line" = {
      if (!is.null(z_var)) {
        data %>%
          group_by(!!sym(z_var)) %>%
          e_charts_(x_var) %>%
          e_line_(actual_y_var) %>%
          e_tooltip()
      } else {
        data %>%
          e_charts_(x_var) %>%
          e_line_(actual_y_var) %>%
          e_tooltip()
      }
    },
    "pie" = {
      # Ignore z_var for pie charts
      data %>%
        e_charts_(x_var) %>%
        e_pie_(actual_y_var) %>%
        e_tooltip()
    },
    "donut" = {
      # Ignore z_var for pie charts
      data %>%
        e_charts_(x_var) %>%
        e_pie_(actual_y_var, radius = c("50%", "70%")) %>%
        e_tooltip()
    }, 
    "rosetype" = {
      # Ignore z_var for pie charts
      data %>%
        e_charts_(x_var) %>%
        e_pie_(actual_y_var, roseType = "radius") %>%
        e_tooltip()
    }
  )
  
  return(plot)
}