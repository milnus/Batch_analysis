# Workflow to detect outliers in pioreactor data and return a list of growth data
outlier_detection_workflow <- function(wide_pioreactor_od_frame) {
  if (is.null(wide_pioreactor_od_frame)) {
    return()
  }

  print("[START] peak_detection_workflow")

  od_columns <- 2:ncol(wide_pioreactor_od_frame)

  # Filter outliers by inner quartile range of window of values
  wide_pioreactor_od_frame <- iqr_outlier_detection(wide_pioreactor_od_frame, od_columns)

  # Isolate information for each pioreactor and isolate readings
  isolated_growth_data <- list()

  for (col in od_columns) {
    reactor_name <- str_remove(colnames(wide_pioreactor_od_frame)[col], "od_reading.")
    # Sample rows
    sample_rows <- wide_pioreactor_od_frame[, (col + length(od_columns))]
    # isolate time
    sample_time <- wide_pioreactor_od_frame[!sample_rows & !is.na(wide_pioreactor_od_frame[, col]), 1]
    # isolate od
    sample_od <- wide_pioreactor_od_frame[!sample_rows & !is.na(wide_pioreactor_od_frame[, col]), col]
    isolated_growth_data[[reactor_name]] <- data.frame(
      sample_time = sample_time,
      od_reading = sample_od
    )
  }

  return(isolated_growth_data)
}
