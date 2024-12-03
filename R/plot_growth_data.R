plot_growth_data <- function(fitted_growth_data_return, remove_points, add_tangent) {
  # if ((min(summarised_data$time_point) - max(summarised_data$time_point)) > 24){
  #   break_step <- 2
  # } else {
  #   break_step <- 1
  # }

  # Initiate plot
  # p <- ggplot(summarised_data$spline_data) #summarised_data$growth_data) +
  p <- ggplot(fitted_growth_data_return)

  # Add points if user want these
  print(paste("remove_points:", remove_points))
  if (remove_points != TRUE) {
    p <- p + geom_point(mapping = aes(Time, OD_values), size = 0.5, alpha = 0.2, shape = 16)
  }

  #### Add in max growth rate estimates ####
  # Find max growth rates
  max_growth_rate_list <- sapply(unique(fitted_growth_data_return$Reactor),
    function(x) {
      reactor_data <- fitted_growth_data_return[fitted_growth_data_return$Reactor == x, c("Time", "Spline_growth_rate", "OD_values", "Reactor", "Spline_OD")]

      max_reactor_data <- reactor_data[which.max(reactor_data$Spline_growth_rate), ]

      return(max_reactor_data)
    },
    simplify = F
  )

  max_growth_rate_df <- do.call("rbind.data.frame", max_growth_rate_list)
  # Find x and y placement for text
  y_text_pos <- max(fitted_growth_data_return$Spline_OD) * 0.85
  x_test_pos <- max(fitted_growth_data_return$Time) * 0.2

  p <- p + geom_text(
    inherit.aes = F,
    data = max_growth_rate_df,
    mapping = aes(
      x = x_test_pos,
      y = y_text_pos,
      label = paste0(
        "µ max = ", round(Spline_growth_rate, 2), "\n",
        "at time ", round(Time, 1)
      )
    ),
    # "at time ", round(Time), 2)),
    size = 3
  )

  # Add the remaining layers of the plot
  p <- p +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(limits = c(0, NA)) +
    geom_line(mapping = aes(Time, Spline_OD, colour = Spline_growth_rate), lwd = 1) +
    scale_color_gradient(name = "Growth\nrate", low = "blue", high = "orange") +
    facet_wrap(. ~ Reactor) +
    theme_light() +
    theme(
      strip.background = element_rect(fill = "transparent", colour = "transparent"),
      strip.text = element_text(colour = "black")
    ) +
    labs(y = "Density", x = "Time")

  print(paste("add_tangent:", add_tangent))
  # if (add_tangent){
  #   # Add tangent line
  #   p <- p + geom_abline(data = max_growth_rate_df, mapping = aes(slope = Spline_growth_rate, intercept = - (Time * Spline_growth_rate) + Spline_OD), size = 0.8, alpha = 1.0)
  #   # Add intersect point of tangent and line
  #   p <- p + geom_point(data = max_growth_rate_df, mapping = aes(x = Time, y = Spline_OD), size = 0.8, alpha = 1.0, col = "red")
  # }


  return(p)
}
