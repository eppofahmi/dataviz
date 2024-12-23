# Plot generation function for map visualization
generate_map_plot <- function(processed_data, map_type = "points", 
                              point_size = 5, point_opacity = 0.6,
                              heat_radius = 20, heat_blur = 15) {
  
  if (is.null(processed_data$data)) {
    return(NULL)
  }
  
  data <- processed_data$data
  
  # Base map configuration
  plot <- data %>%
    e_charts(longitude) %>% 
    e_geo(
      boundingCoords = list(
        c(95, -11),   # Southwest corner of Indonesia
        c(141, 6)     # Northeast corner of Indonesia
      )
    ) %>% 
    e_heatmap(
      latitude, 
      value, 
      coord_system = "geo", 
      blurSize = 1, 
      pointSize = 4
    ) %>% 
    e_visual_map(value)
  
  return(plot)
}

# generate_map_plot(processed_data = df2, map_type = "points", 
#                   point_size = 5, point_opacity = 0.6,
#                   heat_radius = 20, heat_blur = 15)