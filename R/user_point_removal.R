# Function to allow user to select pioreactors of interest
user_point_removal <- function(remove_points){
  radioButtons(
    inputId = "remove_points",
    label = "Remove raw data points from output plot",
    choices = c(FALSE, TRUE),
    selected = remove_points
    )
}