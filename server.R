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
    output$y_var_ui <- renderUI({
        req(data())
        selectInput("y_var", "Value Variable (Y):",
                    choices = names(data()))
    })
    
    output$x_var_ui <- renderUI({
        req(data())
        selectInput("x_var", "Category Variable (X):",
                    choices = names(data()))
    })
    
    output$z_var_ui <- renderUI({
        req(data())
        selectInput("z_var", "Group Variable (optional):",
                    choices = c("None" = "", names(data())))
    })
    
    output$value_var_ui <- renderUI({
        req(data())
        selectInput("value_var", "Value Variable:",
                    choices = names(data()))
    })
    
    output$category_var_ui <- renderUI({
        req(data())
        selectInput("category_var", "Category Variable:",
                    choices = names(data()))
    })
    
    output$longitude_var_ui <- renderUI({
        req(data())
        selectInput("longitude_var", "Longitude:",
                    choices = names(data()))
    })
    
    output$latitude_var_ui <- renderUI({
        req(data())
        selectInput("latitude_var", "Latitude:",
                    choices = names(data()))
    })
    
    output$map_value_var_ui <- renderUI({
        req(data())
        selectInput("map_value_var", "Value:",
                    choices = names(data()))
    })
    
    output$box_value_var_ui <- renderUI({
        req(data())
        numeric_cols <- names(select_if(data(), is.numeric))
        selectInput("box_value_var", "Value Variable:",
                    choices = numeric_cols)
    })
    
    output$box_group_var_ui <- renderUI({
        req(data())
        selectInput("box_group_var", "Group Variable:",
                    choices = names(data()))
    })
    
    # Server-side UI rendering for scatter plot
    output$scatter_x_var_ui <- renderUI({
        req(data())
        # Get numeric columns only
        numeric_cols <- names(select_if(data(), is.numeric))
        selectInput("scatter_x_var", "X-axis Variable (numeric):",
                    choices = numeric_cols)
    })
    
    output$scatter_y_var_ui <- renderUI({
        req(data())
        # Get numeric columns only
        numeric_cols <- names(select_if(data(), is.numeric))
        selectInput("scatter_y_var", "Y-axis Variable (numeric):",
                    choices = numeric_cols)
    })
    
    output$scatter_z_var_ui <- renderUI({
        req(data())
        # Get numeric columns only
        numeric_cols <- names(select_if(data(), is.numeric))
        selectInput("scatter_z_var", "Color Variable (optional):",
                    choices = c("None" = "", numeric_cols))
    })
    # output$scatter_x_var_ui <- renderUI({
    #     req(data())
    #     # Get numeric columns only
    #     numeric_cols <- names(select_if(data(), is.numeric))
    #     selectInput("x_var", "X-axis Variable (numeric):",
    #                 choices = numeric_cols)
    # })
    # 
    # output$scatter_y_var_ui <- renderUI({
    #     req(data())
    #     # Get numeric columns only
    #     numeric_cols <- names(select_if(data(), is.numeric))
    #     selectInput("y_var", "Y-axis Variable (numeric):",
    #                 choices = numeric_cols)
    # })
    # 
    # output$scatter_z_var_ui <- renderUI({
    #     req(data())
    #     # Get numeric columns only
    #     numeric_cols <- names(select_if(data(), is.numeric))
    #     selectInput("z_var", "Color Variable (optional):",
    #                 choices = c("None" = "", numeric_cols))
    # })
    
    # Process data and generate plot ----
    results <- eventReactive(input$generate, {
        req(data())
        
        # Get data frame
        df <- data()
        
        # Handle different plot types
        if (input$plot_type == "map") {
            # Map plot (longitude, latitude, value, aggregation)
            req(input$longitude_var, input$latitude_var, input$map_value_var)
            
            # Validate input data
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
            
            # Process map data
            processed_data <- aggregate_map_data(
                df = df,
                longitude_var = input$longitude_var,
                latitude_var = input$latitude_var,
                value_var = input$map_value_var,
                agg = input$agg_type
            )
            
            # Generate plot
            plot <- if(!is.null(processed_data$data)) {
                generate_map_plot(
                    processed_data = processed_data,
                    map_type = input$map_type,
                    point_size = input$point_size,
                    point_opacity = input$point_opacity,
                    heat_radius = input$heat_radius,
                    heat_blur = input$heat_blur
                )
            } else {
                NULL
            }
            
        } else if (input$plot_type == "boxplot") {
            # Boxplot (y_var, group)
            req(input$box_value_var, input$box_group_var)
            
            processed_data <- process_boxplot_data(
                df = df,
                y_var = input$box_value_var,
                group_var = input$box_group_var
            )
            
            plot <- if(!is.null(processed_data$data)) {
                generate_boxplot(
                    processed_data = processed_data,
                    y_var = input$box_value_var,
                    group_var = input$box_group_var
                )
            } else {
                NULL
            }
            
        } else if (input$plot_type == "scatter") {
            # Scatter plot (x_var, y_var, z_var optional)
            req(input$scatter_x_var, input$scatter_y_var)
            
            # Handle optional z_var
            z_var <- if(!is.null(input$scatter_z_var) && input$scatter_z_var != "") input$scatter_z_var else NULL
            
            # Process scatter data
            processed_data <- process_scatter_data(
                df = df,
                x_var = input$scatter_x_var,
                y_var = input$scatter_y_var,
                z_var = z_var
            )
            
            # Generate scatter plot if processing was successful
            plot <- if(!is.null(processed_data$data)) {
                generate_scatter_plot(
                    processed_data = processed_data,
                    x_var = input$scatter_x_var,
                    y_var = input$scatter_y_var,
                    z_var = z_var
                )
            } else {
                NULL
            }
        } else if (input$plot_type %in% c("pie", "donut", "rosetype")) {
            # Pie, Donut, Rosetype plots (y_var, x_var, aggregation)
            req(input$value_var, input$category_var)
            
            processed_data <- aggregate_data(
                df = df,
                x_var = input$category_var,
                y_var = input$value_var,
                agg = input$agg_type
            )
            
            plot <- if(!is.null(processed_data$data)) {
                generate_plot(
                    data = processed_data$data,
                    x_var = input$category_var,
                    y_var = input$value_var,
                    plot_type = input$plot_type,
                    agg_type = input$agg_type
                )
            } else {
                NULL
            }
            
        } else {
            # Column, Line, Area plots (y_var, x_var, z_var, aggregation)
            req(input$y_var, input$x_var)
            
            # Handle optional z_var
            z_var <- if(!is.null(input$z_var) && input$z_var != "") input$z_var else NULL
            
            processed_data <- aggregate_data(
                df = df,
                x_var = input$x_var,
                y_var = input$y_var,
                z_var = z_var,
                agg = input$agg_type
            )
            
            plot <- if(!is.null(processed_data$data)) {
                generate_plot(
                    data = processed_data$data,
                    x_var = input$x_var,
                    y_var = input$y_var,
                    z_var = z_var,
                    plot_type = input$plot_type,
                    agg_type = input$agg_type
                )
            } else {
                NULL
            }
        }
        
        # Return consistent structure for all plot types
        list(
            aggregation = processed_data,
            plot = plot
        )
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
        
        if (input$plot_type == "boxplot") {
            # For boxplots, show statistical summary
            if (!is.null(results()$aggregation$stats)) {
                DT::datatable(
                    results()$aggregation$stats,
                    options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel')
                    ),
                    extensions = 'Buttons',
                    rownames = FALSE,
                    caption = "Statistical Summary by Group"
                ) %>%
                    DT::formatRound(
                        columns = c("min", "q1", "median", "q3", "max", "mean", "sd"),
                        digits = 2
                    )
            }
        } else {
            # For other plot types, show processed data
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
        }
    })
    
    # # Display data table ----
    # output$data_table <- DT::renderDataTable({
    #     req(results())
    #     if (!is.null(results()$aggregation$data)) {
    #         DT::datatable(
    #             results()$aggregation$data,
    #             options = list(
    #                 pageLength = 10,
    #                 scrollX = TRUE,
    #                 dom = 'Bfrtip',
    #                 buttons = c('copy', 'csv', 'excel')
    #             ),
    #             extensions = 'Buttons',
    #             rownames = FALSE
    #         ) %>%
    #             DT::formatRound(
    #                 columns = sapply(results()$aggregation$data, is.numeric),
    #                 digits = 2
    #             )
    #     }
    # })
}