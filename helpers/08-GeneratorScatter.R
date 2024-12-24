# Generate scatter plot
generate_scatter_plot <- function(processed_data, x_var, y_var, z_var = NULL) {
  if (is.null(processed_data$data)) {
    return(NULL)
  }
  
  # Create base plot with x variable
  plot <- processed_data$data %>%
    e_charts_(x_var)
  
  # Add scatter series with or without z variable
  if (is.null(z_var)) {
    plot <- plot %>%
      e_scatter_(y_var)
  } else {
    # Use z_var as a variable name string
    plot <- plot %>%
      e_scatter_(y_var, z_var) %>%
      e_visual_map_(z_var) %>%
      e_legend(show = FALSE)
  }
  
  # Add basic formatting
  plot <- plot %>%
    e_x_axis(name = x_var) %>%
    e_y_axis(name = y_var)
  
  return(plot)
}

# Example usage:
# result <- process_scatter_data(df, "x_column", "y_column", "z_column")
# plot <- generate_scatter_plot(result, "x_column", "y_column", "z_column")