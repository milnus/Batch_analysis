write_growth_data <- function(tidy_growth_result, data_type){
  table <- tidy_growth_result[[data_type]]
  
  return(table)
}