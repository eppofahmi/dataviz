generate_boxplot <- function(processed_data, x_var, y_var, z_var = NULL) {
  if (is.null(processed_data$data)) {
    return(NULL)
  }
  
  data <- processed_data$data
  stats <- processed_data$stats
  
  # Base plot configuration
  plot <- data %>%
    group_by(!!sym(x_var)) %>%
    e_charts(!!sym(x_var)) %>%
    e_title(
      text = paste("Distribution of", y_var, "by", x_var),
      subtext = if(!is.null(z_var)) paste("Grouped by", z_var) else ""
    )
  
  if (is.null(z_var)) {
    # Single boxplot series
    plot <- plot %>%
      e_boxplot(
        !!sym(y_var),
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
      )
  } else {
    # Grouped boxplot
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    
    # Create a boxplot series for each group
    unique_groups <- unique(data[[z_var]])
    
    for (i in seq_along(unique_groups)) {
      group_name <- unique_groups[i]
      color <- colors[(i-1) %% length(colors) + 1]
      
      plot <- plot %>%
        e_boxplot(
          y_var,
          data = data[data[[z_var]] == group_name,],
          name = as.character(group_name),
          itemStyle = list(
            borderWidth = 2,
            borderColor = color,
            color = paste0(color, "4D")  # Adding transparency
          ),
          tooltip = list(
            formatter = htmlwidgets::JS("
              function(params) {
                return `${params.name} (${params.seriesName})<br/>
                       Maximum: ${params.data[5].toFixed(2)}<br/>
                       Upper Quartile: ${params.data[4].toFixed(2)}<br/>
                       Median: ${params.data[3].toFixed(2)}<br/>
                       Lower Quartile: ${params.data[2].toFixed(2)}<br/>
                       Minimum: ${params.data[1].toFixed(2)}<br/>
                       Count: ${params.data[6]}`
              }
            ")
          )
        )
    }
    
    # Add legend for grouped boxplot
    plot <- plot %>%
      e_legend(
        type = "scroll",
        orient = "horizontal",
        top = "bottom"
      )
  }
  
  # Add common features
  plot <- plot %>%
    e_x_axis(
      name = x_var,
      nameLocation = "center",
      nameGap = 35,
      axisLabel = list(
        rotate = if(length(unique(data[[x_var]])) > 8) 45 else 0
      )
    ) %>%
    e_y_axis(
      name = y_var,
      nameLocation = "center",
      nameGap = 50
    ) %>%
    e_tooltip() %>%
    e_toolbox(
      feature = list(
        saveAsImage = list(
          title = "Save"
        ),
        dataZoom = list(
          title = list(
            zoom = "Zoom",
            back = "Back"
          )
        ),
        restore = list(
          title = "Restore"
        )
      )
    ) %>%
    e_datazoom(
      type = "slider",
      show = TRUE,
      xAxisIndex = 0
    ) %>%
    e_theme("light")
  
  return(plot)
}

# Basic boxplot
result <- process_boxplot_data(
  df = sample_data,
  x_var = "sales_amount",
  y_var = "quantity"
)

generate_boxplot(processed_data = result, x_var = "sales_amount", y_var = "quantity")

# Grouped boxplot
result <- process_boxplot_data(
  df = sample_data,
  x_var = "sales_amount",
  y_var = "quantity",
  z_var = "category"
)
result

generate_boxplot(result, "sales_amount", "quantity", "category")
