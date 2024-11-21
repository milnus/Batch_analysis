# Function to identify the most point dense od values:
density_estimate_function <- function(od_values){
  ## isolate peak from density function
  dens_res <- density(od_values, na.rm = T)
  
  # Find max density
  max_density <- which.max(dens_res$y)
  
  # Find density above and below max
  above_density <- seq_along(dens_res$y)[seq_along(dens_res$y) > max_density]
  below_density <- seq_along(dens_res$y)[seq_along(dens_res$y) < max_density]
  
  # Find where density approached zero
  above_density_zero <- round(dens_res$y[above_density], digits = 0)  == 0
  below_density_zero <- round(dens_res$y[below_density], digits = 0)  == 0
  
  # Find sequence from max to zero
  above_seq_index <- above_density[min(which(above_density_zero == T))]
  below_seq_index <- rev(below_density)[min(which(rev(below_density_zero) == T))]
  
  # Isolate peak
  od_min_dens <- min(dens_res$x[below_seq_index:above_seq_index])
  od_max_dens <- max(dens_res$x[below_seq_index:above_seq_index])
  
  od_delta <- od_max_dens - od_min_dens
  
  return(od_delta)
}