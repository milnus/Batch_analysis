# Function to run batch growth analysis on a set of growth data in a list.
tidy_growth_format <- function(growth_data, bootstaps = 1){
  #### Initialise lists ####
  tidy_list_data <- list()
  growth_data_list <- list()
  spline_data_list <- list()
  
  #### Loop through each reactor to find growth parameters####
  for (description in names(growth_data)){
    print(paste('Processing Reactor:', description))
    # Convert data to tidy format - The 2:(nrow(growth_data[[description]])-1) removes the peak and trough detected from the data
    # tidy_list_data[[description]] <- data.frame("Time" = growth_data[[description]][2:(nrow(growth_data[[description]])-1),1],
    #                                             "Description" = 'Exponential_growth',
    #                                             'Replicate' = 1,
    #                                             "Values" = growth_data[[description]][2:(nrow(growth_data[[description]])-1),2])
    
    tidy_list_data[[description]] <- data.frame("Time" = growth_data[[description]][,1],
                                                "Description" = 'Exponential_growth',
                                                'Replicate' = 1,
                                                "Values" = growth_data[[description]][,2])
    
    tt_grodata <- read_data(tidy_list_data[[description]], data.format = 'col')
    
    tt_growth_rate <- growth.workflow(tt_grodata,
                                      fit.opt = c('s'), # Fit spline
                                      nboot.gc = bootstaps,
                                      ec50 = F,
                                      growth.thresh = 0.05, # *** Should this be increase or do we trust input data, as we have made peak detection and are running turbidostat?
                                      t0 = min(tt_grodata$time),
                                      log.y.spline = T,
                                      smooth.gc = 1.0, suppress.messages = T)
    
    # Extract growth parameters
    growth_data_list[[description]] <- data.frame('mu' = tt_growth_rate[[3]]$gcFittedSplines[[1]]$parameters$mu ,
                                                  'time_point' = tt_growth_rate[[3]]$gcFittedSplines[[1]]$parameters$t.max,
                                                  'growth_phase' = description)
    print("growth_data_list[[description]]")
    print(growth_data_list[[description]])
    # add reactor name for plotting
    tidy_list_data[[description]] <- cbind.data.frame(tidy_list_data[[description]], "growth_phase" = description)
    
    # Add spline data
    spline_data_list[[description]] <- data.frame("Time" = tt_growth_rate$gcFit$gcFittedSplines[[1]]$fit.time, 
                                                  "Values" = tt_growth_rate$gcFit$gcFittedSplines[[1]]$fit.data,
                                                  "Rate" = tt_growth_rate$gcFit$gcFittedSplines[[1]]$spline.deriv1$y,
                                                  "growth_phase" = description,
                                                  "raw_OD" = tt_growth_rate$gcFit$gcFittedSplines[[1]]$data.in[1] * exp(tt_growth_rate$gcFit$gcFittedSplines[[1]]$fit.data))
    
    # plot(tt_growth_rate$gcFit$gcBootSplines[[1]], combine = T, lwd = 0.8)
    
    # summarise the growth curve boot strapping
    # mean(tt_growth_rate[[3]]$gcBootSplines$`Exponential_growth | 1 | NA`$mu)
    # mu_observations <- tt_growth_rate[[3]]$gcBootSplines$`Exponential_growth | 1 | NA`$mu
    # mean(mu_observations[mu_observations > quantile(mu_observations, probs = 0.25) & mu_observations < quantile(mu_observations, probs = 0.75)])
    ### What do we want from here?! what data and fit do we want? ****
  }
  
  # row bind all data for output
  return_od_data <- do.call('rbind', tidy_list_data)
  return_growth_data <- do.call('rbind', growth_data_list)
  return_spline_data <- do.call("rbind", spline_data_list)
  
  return_growth_data$time_point <- as.numeric(return_growth_data$time_point)
  
  return_list <- list('exponential_od_data' = return_od_data, 'growth_data' = return_growth_data, "spline_data" = return_spline_data)
  
  
  return(return_list)
}
