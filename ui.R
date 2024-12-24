ui <- fluidPage(
  titlePanel("Data Viz Prototype"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Data source selection
      radioButtons("data_source", "Choose Data Source:",
                   choices = c("Use Sample Data" = "sample",
                               "Upload Data" = "upload")),
      
      # 2. File upload
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Choose CSV File",
                  accept = c(".csv"))
      ),
      
      # 3. Plot type selection
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Column" = "column",
                              "Line" = "line",
                              "Area" = "area",
                              "Pie" = "pie",
                              "Donut" = "donut",
                              "Rosetype" = "rosetype",
                              "Map" = "map",
                              "Boxplot" = "boxplot",
                              "Scatter" = "scatter")),
      
      # 4. Variable selections based on plot type
      # Scatter plot (no aggregation)
      # conditionalPanel(
      #   condition = "input.plot_type == 'scatter'",
      #   uiOutput("scatter_x_var_ui"),  # Numeric x variable
      #   uiOutput("scatter_y_var_ui"),  # Numeric y variable
      #   uiOutput("scatter_z_var_ui")   # Optional z variable for color
      # ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        uiOutput("scatter_x_var_ui"),  # Numeric x variable
        uiOutput("scatter_y_var_ui"),  # Numeric y variable
        uiOutput("scatter_z_var_ui")   # Optional z variable for color
      ),
      
      # Column, Line, Area plots
      conditionalPanel(
        condition = "['column', 'line', 'area'].includes(input.plot_type)",
        uiOutput("y_var_ui"),
        uiOutput("x_var_ui"),
        uiOutput("z_var_ui"),
        selectInput("agg_type", "Select Aggregation:",
                    choices = c("None" = "none",
                                "Sum" = "sum",
                                "Count" = "count"))
      ),
      
      # Pie, Donut, Rose plots
      conditionalPanel(
        condition = "['pie', 'donut', 'rosetype'].includes(input.plot_type)",
        uiOutput("value_var_ui"),
        uiOutput("category_var_ui"),
        selectInput("agg_type", "Select Aggregation:",
                    choices = c("None" = "none",
                                "Sum" = "sum",
                                "Count" = "count"))
      ),
      
      # Map plot
      conditionalPanel(
        condition = "input.plot_type == 'map'",
        uiOutput("longitude_var_ui"),
        uiOutput("latitude_var_ui"),
        uiOutput("map_value_var_ui"),
        selectInput("agg_type", "Select Aggregation:",
                    choices = c("None" = "none",
                                "Sum" = "sum",
                                "Count" = "count")),
        selectInput("map_type", "Map Type:",
                    choices = c("Points" = "points",
                                "Heatmap" = "heatmap"))
      ),
      
      # Boxplot
      conditionalPanel(
        condition = "input.plot_type == 'boxplot'",
        uiOutput("box_value_var_ui"),
        uiOutput("box_group_var_ui")
      ),
      
      # Generate button
      actionButton("generate", "Generate Chart",
                   class = "btn-primary")
    ),
    
    mainPanel(
      echarts4rOutput("plot"),
      hr(),
      DT::dataTableOutput("data_table")
    )
  )
)