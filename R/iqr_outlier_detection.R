# Detect outlisers based on a Inner Quartile Range outlier detection mechanism
iqr_outlier_detection <- function(wide_pioreactor_od_frame, od_columns){
  # Set the width of the window to search and find center of the window
  k <- 31 # 21 also worked
  
  # Check that K is odd, so that it has a center point.
  if (k %% 2 == 0){
    print("k cannot be even!")
    return (NULL)
  }
  
  # For each reactor identify outliers and add a column indicating these
  for (column_i in od_columns){
    print(paste("pre iqr detection:", sum(!is.na(wide_pioreactor_od_frame[,column_i]))))
    
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[column_i], '\\.'))[2]
    outlier_column_name <- paste0(reactor_name, '.outliers')
    wide_pioreactor_od_frame[,outlier_column_name] <- FALSE
    
    # condence column for NA's
    data_oi <- wide_pioreactor_od_frame[,c(1,column_i)]
    data_oi <- data_oi[!is.na(data_oi[,2]),]
    
    # add roll to find median and IQR
    # median_roll <- zoo::rollmedian(data_oi[,2], align = 'center', k=k, na.pad = T, partial = T)
    median_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = median, partial = T)
    iqr_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = IQR, partial = T)
    
    for (row_i in 1:(nrow(data_oi))){
      # Remove points 2.5 Inner Quartile Ranges above or below median
      if (data_oi[row_i, 2] > median_roll[row_i] + iqr_roll[row_i] * 1.5 | # 2.5 worked well- this was the previous
          data_oi[row_i, 2] < median_roll[row_i] - iqr_roll[row_i] * 1.5){
        wide_pioreactor_od_frame[row_i, column_i] <- NA
        wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] == data_oi[row_i,1],
                                 outlier_column_name] <- TRUE
      }
    }
    print(paste("post iqr detection:", sum(!is.na(wide_pioreactor_od_frame[,column_i]))))
  }
  return(wide_pioreactor_od_frame)
}