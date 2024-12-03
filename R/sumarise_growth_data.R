summarise_growth_data <- function(tidy_growth_data, high_mu_percentage = 0.9) {
  if (is.null(tidy_growth_data)) {
    return()
  }
  #### Summarise the main outputs from the growth analysis ####
  max_growth_rate_list <- sapply(unique(tidy_growth_data$Reactor),
    function(x) {
      reactor_data <- tidy_growth_data[
        tidy_growth_data$Reactor == x,
        c("Time", "Spline_growth_rate", "OD_values", "Reactor", "Spline_OD_log")
      ]

      max_reactor_data <- reactor_data[which.max(reactor_data$Spline_growth_rate), ]

      return(max_reactor_data)
    },
    simplify = F
  )

  summarised_data <- do.call("rbind.data.frame", max_growth_rate_list)


  #### Summarise time at high µ ####
  # Calculate high µ percentage
  high_mu_time_name <- paste("Total_high_mu_time_", high_mu_percentage * 100, "%", sep = "")

  # Summarise time spent at high µ
  time_spent_at_high_mu <- sapply(unique(tidy_growth_data$Reactor),
    function(x) {
      # Isolate data from given reactor
      reactor_data <- tidy_growth_data[tidy_growth_data$Reactor == x, ]
      max_growth_data <- summarised_data[summarised_data$Reactor == x, ]

      min_time <- min(reactor_data$Time[reactor_data$Spline_growth_rate >=
        max_growth_data$Spline_growth_rate * high_mu_percentage])

      max_time <- max(reactor_data$Time[reactor_data$Spline_growth_rate >=
        max_growth_data$Spline_growth_rate * high_mu_percentage])

      high_mu_time <- max_time - min_time

      return(data.frame(
        "High_mu_min_time" = min_time,
        "High_mu_max_time" = max_time,
        "high_mu_time_time" = high_mu_time
      ))
    },
    simplify = F
  )

  time_spent_at_high_mu <- do.call("rbind.data.frame", time_spent_at_high_mu)

  # Summarise the


  summarised_data <- merge(summarised_data,
    by.x = 4,
    time_spent_at_high_mu, by.y = 0
  )

  colnames(summarised_data) <- c("Reactor", "Time", "mu", "Reactor_OD", "Spline_OD", "High_mu_min_time", "High_mu_max_time", high_mu_time_name)
  print(head(summarised_data))
  summarised_data[, 2:ncol(summarised_data)] <- round(summarised_data[, 2:ncol(summarised_data)], 4)

  return(summarised_data)
}
