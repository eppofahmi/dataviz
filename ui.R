ui <- fluidPage(
  titlePanel("Data Viz Prototype"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Radio button for data source
      radioButtons("data_source", "Choose Data Source:",
                  choices = c("Use Sample Data" = "sample",
                            "Upload Data" = "upload")),
      
      # 2. File upload input
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Choose CSV File",
                 accept = c(".csv"))
      ),
      
      # 3. Plot type selection
      # 3. Plot type selection
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Column" = "column",
                              "Line" = "line",
                              "Area" = "area",
                              "Pie" = "pie",
                              "Donut" = "donut", 
                              "Rosetype" = "rosetype",
                              "Map" = "map"
                  )
      ),
      
      # 4. Column selections
      # Dynamic variable selections based on plot type
      conditionalPanel(
        condition = "input.plot_type === 'map'",
        uiOutput("longitude_var_ui"),
        uiOutput("latitude_var_ui"),
        uiOutput("map_value_var_ui")
        # Optional map settings
        # selectInput("map_type", "Map Type:",
        #             choices = c("Points" = "points",
        #                         "Heatmap" = "heatmap"),
        #             selected = "points"),
        # conditionalPanel(
        #   condition = "input.map_type === 'points'",
        #   sliderInput("point_size", "Point Size:",
        #               min = 1, max = 20, value = 5),
        #   sliderInput("point_opacity", "Point Opacity:",
        #               min = 0, max = 1, value = 0.6)
        # ),
        # conditionalPanel(
        #   condition = "input.map_type === 'heatmap'",
        #   sliderInput("heat_radius", "Heat Radius:",
        #               min = 5, max = 50, value = 20),
        #   sliderInput("heat_blur", "Heat Blur:",
        #               min = 5, max = 50, value = 15)
        # )
      ),
      conditionalPanel(
        condition = "['pie', 'donut', 'rosetype'].includes(input.plot_type)",
        uiOutput("category_var_ui"),
        uiOutput("value_var_ui")
      ),
      conditionalPanel(
        condition = "!['pie', 'donut', 'rosetype', 'map'].includes(input.plot_type)",
        uiOutput("x_var_ui"),
        uiOutput("y_var_ui"), 
        uiOutput("group_var_ui")
      ),

      # 5. Aggregation selection
      selectInput("agg_type", "Select Aggregation:",
                 choices = c("None" = "none",
                           "Count" = "count",
                           "Sum" = "sum")),
      
      # 6. Generate button
      actionButton("generate", "Generate Chart"),
      
      # Status messages
      verbatimTextOutput("status_message"),
      verbatimTextOutput("plot_status")
    ),
    
    mainPanel(
      echarts4rOutput("plot"),
      hr(), # Add horizontal line between plot and table
      DT::dataTableOutput("data_table")
    )
  )
)