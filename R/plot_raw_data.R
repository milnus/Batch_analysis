# Function for plotting all raw od readings from reactors
plot_raw_data <- function(raw_data, filter_vector, filt_strat){
  if(is.null(raw_data)){return()}
  
  print(paste("filter_vector plot_raw_data:", filter_vector))
  df_list <- apply(raw_data[,2:ncol(raw_data)], 2, function(x) data.frame('timestamp' = raw_data[,1], 'od_reading' = x))
  
  ind_plots <- sapply(1:length(df_list), function(i) plot_dataframe_raw(df_list[[i]], names(df_list)[i], filter_vector, filt_strat), simplify = F)
  do.call(gridExtra::grid.arrange, c(ind_plots))
}