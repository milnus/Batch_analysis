#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

function(input, output) {

  # output$reactor <- renderPrint(input$reactor)
  reactor_groups <- eventReactive(input$process, {
    input$reactor_groups
  })

  output$reactor_groups <- renderPrint(reactor_groups())

  # Event observer on the input of Pioreactor groupings
  observeEvent(input$process, {
    message(paste("Pioreactor grouping updated:", reactor_groups()))
  })

  observeEvent(input$process, {
    message(paste("Pioreactor input OD file:", input$upload[1, 4]))
  })
 
  observeEvent(input$process, {
    message(paste("Pioreactor filtering:", input$reactor_selection))
  })
 
  #### Process the data for individual raw data plotting ####
  observeEvent(input$plot_raw, {
    message(paste("Pioreactor input OD file:", input$upload[1, 4]))
  })
  
  read_data_raw <- reactive(raw_pio_od_data_to_wide_frame(input$upload[1, 4]))
  
  output$UserFilters <- renderUI({
    user_pio_selection(read_data_raw(), input$reactor_selection)
  })
  
  output$raw_data_plot <- renderPlot(plot_raw_data(read_data_raw(), input$reactor_selection, input$filt_strat))
  
  
  #### Process the data for individual growth curve analysis ####
  read_data <- eventReactive(input$process, {
    # Read the path to the file uploaded by the user
    raw_pio_od_data_to_wide_frame(input$upload[1,4])
  })
  
  filtered_data <- reactive(filter_reactors(read_data(), input$reactor_selection, input$filt_strat))
  
  outlier_filtered_data <- reactive(outlier_detection_workflow(filtered_data()))
  
  # tt <- reactive(renderTable(peak_detection))
  
  # output$plot <- renderPlot(growth_curve_plot())
  
  tidy_growth_result <- reactive(tidy_growth_format(outlier_filtered_data()))
  
  summarised_growth_data <- reactive(write_growth_data(tidy_growth_result(), "growth_data"))
  plot_data <- reactive(write_growth_data(tidy_growth_result(), "exponential_od_data"))
  
  
  #### Render the table of summarised growth data ####
  output$table <- renderTable(summarised_growth_data())
  
  #### Construct plot ####
  # Raw data to summarised
  output$plot <- renderPlot({
    plot_growth_data(tidy_growth_result(), input$remove_points)
  }, res = 96)
  
  ## Let the user turn of points in growth plot
  output$UserPointRemoval <- renderUI({
    user_point_removal(input$remove_points)
  })
  
  #### Allow download of data ####
  # Summarised data
  output$download_table <- downloadHandler(
    filename = function() {
      "Summaried_growth_rate_data.csv"
    },
    content = function(file) {
      write.table(summarised_growth_data(), file, row.names = F, col.names = T, sep = ",")
      
    }
  )
  
  # Raw data values from bootstrap
  output$download_raw_table <- downloadHandler(
    filename = function() {
      "Raw_growth_rate_data_bootstrap_resamplings.csv"
    },
    content = function(file) {
      write.table(plot_data(), file, row.names = F, col.names = T, sep = ",")
    }
  )
  
  # Growth rate plot download
  output$download_growth_rate_plot <- downloadHandler(
    filename = function() {
      "Growth_rate_plot.svg"
    },
    content = function(file) {
      ggsave(file, plot_growth_data(tidy_growth_result(), input$remove_points), device = 'svg', 
             width = 33, height = 19, units = 'cm')
    }
  ) 
}
