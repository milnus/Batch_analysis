plot_growth_data <- function(summarised_data, remove_points){
  if ((min(summarised_data$time_point) - max(summarised_data$time_point)) > 24){
    break_step <- 2
  } else {
    break_step <- 1
  }
  
  # Initiate plot
  p <- ggplot(summarised_data$spline_data) #summarised_data$growth_data) +
  
  # Add points if user want these
  print(paste("remove_points:", remove_points))
    if (remove_points != TRUE){
      p <- p + geom_point(data = summarised_data$exponential_od_data, inherit.aes = F, mapping = aes(Time, Values), size = 0.5, alpha = 0.2, shape = 16)
    }
  
  # Add the ramining layers of the plot
    p <- p + scale_y_continuous(limits = c(0,NA)) +
    geom_line(data = summarised_data$spline_data, aes(Time, Values, colour = Rate), lwd = 1) +
    scale_color_gradient(name = "Growth\nrate", low = "blue", high = "orange") +
    geom_text(data = summarised_data$growth_data, inherit.aes = F, mapping = aes(x = max(summarised_data$exponential_od_data$Time) * 0.2, 
                            y = max(summarised_data$exponential_od_data$Values)*0.85, 
                            label = paste0("µ max = ", round(mu, 2),"\n","at time ", round(time_point, 2))), 
              size = 3) +
    facet_wrap(.~growth_phase) +
    theme_light() +
    theme(strip.background = element_rect(fill = "transparent", colour = "transparent"),
          strip.text = element_text(colour = "black"))
    labs(y = 'Density') #+
    # guides(colour=guide_legend(title="Growth\nrate"))
    
    
    # ggplot(summarised_data, aes(time_point, mean_mu, colour = reactor)) +
    # geom_smooth(method = 'loess', se = F, span = 1.0) +
    # geom_linerange(aes(ymax = mean_mu + sd_mu, ymin = mean_mu - sd_mu)) +
    # geom_point() +
    # # geom_line() +
    # ggprism::theme_prism() +
    # scale_x_continuous(limits = c(0, max(summarised_data$time_point)),
    #                    breaks = seq(0, max(summarised_data$time_point), break_step), labels = seq(0, max(summarised_data$time_point), break_step))
  
  return(p)
}