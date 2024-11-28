# Function to take batch growth and extract growth parameters. 
# A smooth spline and its derivative is used to extract growth parameters

spline_growth_integration <- function(growth_data, bootstaps = 0, spline_smoothing = 1.0){
  # TODO - Implement that boot strapping is a possibility
  # TODO - Allow a user defined value for smoothing of the spline
  
  #### Initialise lists ####
  tidy_list_data <- list()
  growth_data_list <- list()
  spline_data_list <- list()
  
  #### Loop through each reactor to find growth parameters####
  for (description in names(growth_data)){
    print(paste('Processing Reactor:', description))
    
    #### Format data for spline analysis ####
    # Extract relevant data
    tidy_list_data[[description]] <- data.frame("Time" = growth_data[[description]][,1],
                                                "OD_values" = growth_data[[description]][,2],
                                                "Reactor" = description)
    
    #### Do spline analysis ####
    # Construct spline smoothing function
    smoothed_spline_function <- smooth.spline(tidy_list_data[[description]]$Time, 
                                              tidy_list_data[[description]]$OD_values, 
                                              spar = spline_smoothing)
    
    #### Get growth curve metrics ####
    # Get the growth parameters for spline - 0th derivative
    tidy_list_data[[description]]$Spline_OD <- predict(smoothed_spline_function, deriv = 0)$y
    # Get the growth parameters for spline - 1st derivative
    tidy_list_data[[description]]$Spline_growth_rate <- predict(smoothed_spline_function, deriv = 1)$y
  }

  # Concatenate the growth data and set type of variables
  fitted_growth_data_return <- do.call("rbind", tidy_list_data)
  fitted_growth_data_return$Time <- as.numeric(fitted_growth_data_return$Time)
  
  return(fitted_growth_data_return)
}