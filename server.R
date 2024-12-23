server <- function(input, output, session) {
    
    # Reactive data source ----
    data <- reactive({
        if(input$data_source == "sample") {
            return(sample_data)
        } else {
            req(input$file)
            read.csv(input$file$datapath)
        }
    })
    
    # Dynamic column selection UI ----
    output$x_var_ui <- renderUI({
        req(data())
        selectInput("x_var", "Select X Variable:",
                    choices = names(data()))
    })
    
    output$y_var_ui <- renderUI({
        req(data())
        selectInput("y_var", "Select Y Variable:",
                    choices = names(data()))
    })
    
    output$group_var_ui <- renderUI({
        req(data())
        selectInput("group_var", "Select Group Variable:",
                    choices = c("None" = "", names(data())))
    })
    
    # Pie chart specific inputs ----
    output$category_var_ui <- renderUI({
        req(data())
        selectInput("x_var", "Select Category:",
                    choices = names(data()))
    })
    
    output$value_var_ui <- renderUI({
        req(data())
        selectInput("y_var", "Select Value:",
                    choices = names(data()))
    })
    
    # Server-side handlers for map variables ----
    output$longitude_var_ui <- renderUI({
        selectInput("longitude_var", "Longitude:", 
                    choices = names(data()), 
                    selected = "longitude")
    })
    
    output$latitude_var_ui <- renderUI({
        selectInput("latitude_var", "Latitude:", 
                    choices = names(data()), 
                    selected = "latitude")
    })
    
    output$map_value_var_ui <- renderUI({
        selectInput("map_value_var", "Value:", 
                    choices = names(data()))
    })
    
    # Process data and generate plot ----
    # Process data and generate plot ----
    results <- eventReactive(input$generate, {
        req(data())
        
        if (input$plot_type == "map") {
            # Map-specific processing
            req(input$longitude_var, input$latitude_var, input$map_value_var)
            
            # Validate input data
            df <- data()
            if (!all(c(input$longitude_var, input$latitude_var, input$map_value_var) %in% names(df))) {
                return(list(
                    aggregation = list(
                        data = NULL,
                        message = "Required columns not found in data",
                        plot_message = "Plot Status: Cannot be plotted - missing columns"
                    ),
                    plot = NULL
                ))
            }
            
            # Use map-specific preprocessing with error catching
            processed_data <- tryCatch({
                aggregate_map_data(
                    df = df,
                    longitude_var = input$longitude_var,
                    latitude_var = input$latitude_var,
                    value_var = input$map_value_var,
                    agg = input$agg_type
                )
            }, error = function(e) {
                print(paste("Error in map preprocessing:", e$message))
                return(list(
                    data = NULL,
                    message = paste("Error processing map data:", e$message),
                    plot_message = "Plot Status: Cannot be plotted - processing error"
                ))
            })
            
            # Generate map plot if preprocessing was successful
            plot <- if(!is.null(processed_data$data)) {
                tryCatch({
                    generate_map_plot(
                        processed_data = processed_data,
                        map_type = input$map_type,
                        point_size = input$point_size,
                        point_opacity = input$point_opacity,
                        heat_radius = input$heat_radius,
                        heat_blur = input$heat_blur
                    )
                }, error = function(e) {
                    print(paste("Error generating map plot:", e$message))
                    NULL
                })
            } else {
                NULL
            }
            
            # Return results with consistent structure
            list(
                aggregation = processed_data,  # Contains data, message, and plot_message
                plot = plot
            )
            
        } else {
            # Regular plot processing (unchanged)
            req(input$x_var, input$y_var)
            z_var <- if(input$group_var != "") input$group_var else NULL
            
            # Use the aggregate_data function for non-map plots
            agg_results <- aggregate_data(
                df = data(),
                x_var = input$x_var,
                y_var = input$y_var,
                z_var = z_var,
                agg = input$agg_type
            )
            
            # Generate plot if data is available
            plot <- if(!is.null(agg_results$data)) {
                generate_plot(
                    data = agg_results$data,
                    x_var = input$x_var,
                    y_var = input$y_var,
                    z_var = z_var,
                    plot_type = input$plot_type,
                    agg_type = input$agg_type
                )
            } else {
                NULL
            }
            
            list(
                aggregation = agg_results,
                plot = plot
            )
        }
    })
    
    # Display status messages ----
    output$status_message <- renderText({
        req(results())
        results()$aggregation$message
    })
    
    output$plot_status <- renderText({
        req(results())
        results()$aggregation$plot_message
    })
    
    # Display plot ----
    output$plot <- renderEcharts4r({
        req(results())
        results()$plot
    })
    
    # Display data table ----
    output$data_table <- DT::renderDataTable({
        req(results())
        if (!is.null(results()$aggregation$data)) {
            DT::datatable(
                results()$aggregation$data,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                ),
                extensions = 'Buttons',
                rownames = FALSE
            ) %>%
                DT::formatRound(
                    columns = sapply(results()$aggregation$data, is.numeric),
                    digits = 2
                )
        }
    })
}