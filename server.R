server <- function(input, output, session) {
    
    # Reactive data source
    data <- reactive({
        if(input$data_source == "sample") {
            return(sample_data)
        } else {
            req(input$file)
            read.csv(input$file$datapath)
        }
    })
    
    # Dynamic column selection UI
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
    
    # Pie chart specific inputs
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
    
    # Process data and generate plot
    results <- eventReactive(input$generate, {
        req(data(), input$x_var, input$y_var)
        
        z_var <- if(input$group_var != "") input$group_var else NULL
        
        # Use the aggregate_data function
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
    })
    
    # Display status messages
    output$status_message <- renderText({
        req(results())
        results()$aggregation$message
    })
    
    output$plot_status <- renderText({
        req(results())
        results()$aggregation$plot_message
    })
    
    # Display plot
    output$plot <- renderEcharts4r({
        req(results())
        results()$plot
    })
    
    # Display data table
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