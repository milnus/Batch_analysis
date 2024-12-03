# Function to take batch growth and extract growth parameters.
# A smooth spline and its derivative is used to extract growth parameters

spline_growth_integration <- function(growth_data, bootstaps = 0, spline_smoothing = 1.0) {
  # TODO - Implement that boot strapping is a possibility
  # TODO - Allow a user defined value for smoothing of the spline

  #### Initialise lists ####
  tidy_list_data <- list()
  growth_data_list <- list()
  spline_data_list <- list()

  #### Loop through each reactor to find growth parameters####
  for (description in names(growth_data)) {
    print(paste("Processing Reactor:", description))

    #### Format data for spline analysis ####
    # Extract relevant data
    tidy_list_data[[description]] <- data.frame(
      "Time" = growth_data[[description]][, 1],
      "OD_values" = growth_data[[description]][, 2],
      "Reactor" = description
    )

    # QurvE use the following to "control" negative growth values:
    # if (length(data[data < 0]) >
    #     0)
    # {
    #   data <- data + abs(min(data[data < 0])) +
    #     0.01  # add the absolute value of the minimum negative growth (+ 0.01) to the data
    # }

    # Our implementation of the above?
    # if (length(data[data < 0]) >
    #     0)
    # {
    #   data <- data + abs(min(data[data < 0])) +
    #     0.01  # add the absolute value of the minimum negative growth (+ 0.01) to the data
    # }

    # Remove negative OD readings to avoid problems with log10 transformation
    # tidy_list_data[[description]] <- tidy_list_data[[description]][tidy_list_data[[description]]$OD_values > 0,]

    # Transform OD values to log (taking ln(y/y0))
    tidy_list_data[[description]]$OD_values_log <- log(tidy_list_data[[description]]$OD_values / tidy_list_data[[description]]$OD_values[1]) + 0.01

    #### Do spline analysis ####
    # Construct spline smoothing function
    smoothed_spline_function <- smooth.spline(tidy_list_data[[description]]$Time,
      tidy_list_data[[description]]$OD_values_log,
      cv = NA, # Skips cross validation and speeds things up
      spar = spline_smoothing
    )

    #### Get growth curve metrics ####
    # Get the growth parameters for spline - 0th derivative
    tidy_list_data[[description]]$Spline_OD <- predict(smoothed_spline_function, deriv = 0)$y
    # Get the growth parameters for spline - 1st derivative
    tidy_list_data[[description]]$Spline_growth_rate <- predict(smoothed_spline_function, deriv = 1)$y

    # # Get the growth parameters for spline that is log10 transformed
    # tidy_list_data[[description]]$Spline_OD_log <- predict(smoothed_spline_function, deriv = 0)$y

    print(head(tidy_list_data[[description]]))
    print(str(tidy_list_data[[description]]))
    tidy_list_data[[description]]$Spline_OD_log <- tidy_list_data[[description]]$Spline_OD
    # Convert spline from log(y/y0) to regular OD
    tidy_list_data[[description]]$Spline_OD <- exp(tidy_list_data[[description]]$Spline_OD_log) * tidy_list_data[[description]]$OD_values[1]
  }

  # Concatenate the growth data and set type of variables
  fitted_growth_data_return <- do.call("rbind", tidy_list_data)
  fitted_growth_data_return$Time <- as.numeric(fitted_growth_data_return$Time)

  return(fitted_growth_data_return)
}
