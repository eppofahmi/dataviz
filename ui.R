ui <- fluidPage(
  titlePanel("echarts4r Dashboard"),
  
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
      selectInput("plot_type", "Select Plot Type:",
                 choices = c("Column" = "column",
                           "Line" = "line",
                           "Pie" = "pie", 
                           "Donut" = "donut", 
                           "Rosetype" = "rosetype")
                 ),
      
      # 4. Column selections
      # Dynamic variable selections based on plot type
      conditionalPanel(
        condition = "!['pie', 'donut', 'rosetype'].includes(input.plot_type)",
        uiOutput("x_var_ui"),
        uiOutput("y_var_ui"), 
        uiOutput("group_var_ui")
      ),
      conditionalPanel(
        condition = "['pie', 'donut', 'rosetype'].includes(input.plot_type)",
        uiOutput("category_var_ui"),
        uiOutput("value_var_ui")
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