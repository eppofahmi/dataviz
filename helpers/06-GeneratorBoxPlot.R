# Function to generate boxplot using echarts4r
generate_boxplot <- function(processed_data, y_var, group_var) {
  if (is.null(processed_data$data)) {
    return(NULL)
  }
  
  # Set title
  title <- paste("Distribution of", y_var, "by", group_var)
  
  # Create base plot
  plot <- processed_data$data %>%
    group_by(!!sym(group_var)) %>%
    e_charts_() %>%
    e_boxplot_(
      y_var,
      itemStyle = list(
        borderWidth = 2,
        borderColor = "#1f77b4",
        color = "rgba(31, 119, 180, 0.3)"
      ),
      tooltip = list(
        formatter = htmlwidgets::JS("
          function(params) {
            return `${params.name}<br/>
                   Maximum: ${params.data[5].toFixed(2)}<br/>
                   Upper Quartile: ${params.data[4].toFixed(2)}<br/>
                   Median: ${params.data[3].toFixed(2)}<br/>
                   Lower Quartile: ${params.data[2].toFixed(2)}<br/>
                   Minimum: ${params.data[1].toFixed(2)}<br/>
                   Count: ${params.data[6]}`
          }
        ")
      )
    ) %>%
    e_title(title) %>%
    e_x_axis(
      name = group_var,
      nameLocation = "center",
      nameGap = 35,
      axisLabel = list(
        rotate = if(length(unique(processed_data$data[[group_var]])) > 8) 45 else 0
      )
    ) %>%
    e_y_axis(
      name = y_var,
      nameLocation = "center",
      nameGap = 50
    ) %>%
    e_tooltip() %>%
    e_theme("light")
  
  return(plot)
}