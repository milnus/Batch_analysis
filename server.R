library(shiny)

function(input, output) {
  ### Function for running server side operations of the batch growth analysis app ###

  #### Observer events trickering on pressing "Process data botton on UI ####
  # Observe possible grouping of reactors
  reactor_groups <- eventReactive(input$process, {
    input$reactor_groups
  })

  # output$reactor_groups <- renderPrint(reactor_groups()) # TODO - Likely remove due to not being used

  # Observe PioReactor groupings
  observeEvent(input$process, {
    message(paste("Pioreactor grouping updated:", reactor_groups()))
  })

  # Observing path name of the input OD readings file - TODO - Likely remove function below as it is performed by the observer event function
  # observeEvent(input$process, {
  #   message(paste("Pioreactor input OD file:", input$upload[1, 4]))
  # })
  # Observing path name of the input OD readings file
  observeEvent(input$plot_raw, {
    message(paste("Pioreactor input OD file:", input$upload[1, 4]))
  })

  # Observe reactors selected
  observeEvent(input$process, {
    message(paste("Pioreactor filtering:", input$reactor_selection))
  })

  #### Process the data for individual raw data plotting ####
  # Read raw data
  read_data_raw <- reactive(raw_pio_od_data_to_wide_frame(input$upload[1, 4]))

  # Construct selection feature for ui to select reactors to filter
  output$UserFilters <- renderUI({
    user_pio_selection(read_data_raw(), input$reactor_selection)
  })

  # Render plot of raw data with filtering strategy by colour
  output$raw_data_plot <- renderPlot(plot_raw_data(
    read_data_raw(),
    input$reactor_selection,
    input$filt_strat
  ))


  #### Process the data for individual growth curve analysis ####

  # Let the user set the spline smoothing
  output$UserSplineSmoothing <- renderUI({
    smoothing_slider(input$spline_smoothing)
  })

  # Read raw data upon starting processing
  read_data <- eventReactive(input$process, {
    # Read the path to the file uploaded by the user
    raw_pio_od_data_to_wide_frame(input$upload[1, 4])
  })

  # Filter reactors of interest based on selection and strategy for filtering (keep or remove)
  filtered_data <- reactive(filter_reactors(
    read_data(),
    input$reactor_selection,
    input$filt_strat
  ))

  # Identify outliers in the OD readings
  outlier_filtered_data <- reactive(outlier_detection_workflow(filtered_data()))

  negative_corrected_data <- reactive(correct_neg_data_median(outlier_filtered_data()))

  # Run the growth analysis for datasets
  # tidy_growth_result <- reactive(spline_growth_integration(outlier_filtered_data(),
  #                                                          spline_smoothing = input$spline_smoothing))

  tidy_growth_result <- reactive(spline_growth_integration(negative_corrected_data(),
    spline_smoothing = input$spline_smoothing
  ))


  # summarised_growth_data <- reactive(write_growth_data(tidy_growth_result(), "growth_data")) ## TODO - RE-ADD
  # plot_data <- reactive(write_growth_data(tidy_growth_result(), "exponential_od_data")) ## TODO - RE-ADD


  #### Render the table of summarised growth data ####
  # output$table <- renderTable(summarised_growth_data()) ## TODO - RE-ADD
  # Let the user define the percentage of a high µ
  output$UserHighMuPercentage <- renderUI({
    high_mu_range_slider(input$high_mu_percentage)
  })
  # Summarise growth data
  summarised_growth_data <- reactive(summarise_growth_data(
    tidy_growth_result(),
    input$high_mu_percentage / 100
  ))
  output$table <- renderTable(summarised_growth_data()) ## TODO - RE-ADD

  #### Construct plot ####
  # Raw data to summarised
  output$plot <- renderPlot(
    {
      plot_growth_data(tidy_growth_result(), input$remove_points, input$add_tanget)
    },
    res = 96
  )

  ## Construct log-OD plog
  output$log_plot <- renderPlot(
    {
      plot_growth_data_log(tidy_growth_result(), input$remove_points, input$add_tanget)
    },
    res = 96
  )

  ## Let the user turn of points in growth plot
  output$UserPointRemoval <- renderUI({
    user_point_removal(input$remove_points)
  })

  ## Let the user add tanget at µmax in growth plot
  output$UserTangetAddition <- renderUI({
    add_tangent(input$add_tanget)
  })

  output$growth_rate_plot <- renderPlot(
    {
      plot_growth_rate_data(tidy_growth_result(), summarised_growth_data())
    },
    res = 96
  )

  #### Allow download of data ####
  # Summarised data
  output$download_table <- downloadHandler( ## TODO - RE-ADD
    filename = function() {
      "Summaried_growth_rate_data.csv"
    },
    content = function(file) {
      write.table(summarised_growth_data(), file, row.names = F, col.names = T, sep = ",")
    }
  )

  # Raw data values from bootstrap
  output$download_raw_table <- downloadHandler( ## TODO - RE-ADD
    filename = function() {
      "Raw_growth_rate_data_batch.csv"
    },
    content = function(file) {
      write.table(tidy_growth_result(), file, row.names = F, col.names = T, sep = ",")
    }
  )

  # Growth rate plot download
  output$download_growth_rate_plot <- downloadHandler(
    filename = function() {
      "Growth_rate_plot.svg"
    },
    content = function(file) {
      ggsave(file, plot_growth_data(tidy_growth_result(), input$remove_points),
        device = "svg",
        width = 33, height = 19, units = "cm"
      )
    }
  )
}
