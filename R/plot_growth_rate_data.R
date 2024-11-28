plot_growth_rate_data <- function(fitted_growth_data_return, summarised_growth_data, show_mu_max_interval = T){
  # Initiate plot
  p <- ggplot(fitted_growth_data_return)
  
  # Add the remaining layers of the plot
  p <- p + scale_y_continuous(limits = c(NA, NA)) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
    geom_line(mapping =  aes(Time, Spline_growth_rate, colour = Spline_growth_rate), lwd = 1) +
    scale_color_gradient(name = "Growth\nrate", low = "blue", high = "orange") +
    facet_wrap(. ~ Reactor) +
    theme_light() +
    theme(strip.background = element_rect(fill = "transparent", colour = "transparent"),
          strip.text = element_text(colour = "black"))
  labs(y = 'Density')

  
  if (show_mu_max_interval){
    colnames(summarised_growth_data)[ncol(summarised_growth_data)] <- "max_time"
    p <- p + geom_segment(inherit.aes = F, data = summarised_growth_data, aes(x = High_mu_min_time, xend = High_mu_max_time, y = mu*.01, yend = mu*.01),
                          col = "red", lwd = 1) +
      geom_label(inherit.aes = F, data = summarised_growth_data, aes(x = (High_mu_max_time+High_mu_min_time)/2, y = 0, label = round(max_time,2)), hjust = 0.5, vjust = 1)
  }
  
  # TODO - Add line indicating time spent in 90% of max growth - Could give a slider for this
  
  return(p)
}