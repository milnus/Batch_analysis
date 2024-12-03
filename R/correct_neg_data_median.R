correct_neg_data_median <- function(growth_data) {
  # Set window to take median
  k_range <- 31
  k <- k_range %/% 2

  for (description in names(growth_data)) {
    # Get indexes of negative measurments
    neg_indexes <- which(growth_data[[description]]$od_reading < 0)
    if (length(neg_indexes)) {
      growth_data[[description]]$od_reading[neg_indexes] <-
        sapply(neg_indexes, function(i) {
          lower_bound <- i - k
          upper_bound <- i + k

          lower_bound <- ifelse(lower_bound > 0, lower_bound, 1)
          upper_bound <- ifelse(upper_bound > nrow(growth_data[[description]]), nrow(growth_data[[description]]), upper_bound)


          # Find the values in the range k
          k_values <- growth_data[[description]]$od_reading[lower_bound:upper_bound]
          # Find number of positive measurements in k values
          n_pos <- sum(k_values > 0)


          # if range is less than the specified expand until k positive values are reached
          while (n_pos < k_range) {
            k <- k + 1

            lower_bound <- i - k
            upper_bound <- i + k

            lower_bound <- ifelse(lower_bound > 0, lower_bound, 1)
            upper_bound <- ifelse(upper_bound > nrow(growth_data[[description]]), nrow(growth_data[[description]]), upper_bound)

            k_values <- growth_data[[description]]$od_reading[lower_bound:upper_bound]

            n_pos <- sum(k_values > 0)
          }

          # reset k
          k <- k_range %/% 2

          # Calculate the median
          median_k_value <- median(k_values[k_values > 0])

          return(median_k_value)
        })
    }
  }
  return(growth_data)
}
