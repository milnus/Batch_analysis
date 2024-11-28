# Function to allow user to select reactors of interest
user_pio_selection <- function(read_data, reactor_selection){
  if(is.null(read_data)){return()}
  
  selectInput(
    inputId = "reactor_selection",
    label = "Select Pioreactors to remove or keep based on option above\n(red plots will be removed and green will be keep in further analyses)",
    choices = str_replace(names(read_data), pattern = 'od_reading\\.', '')[-1],
    selected = reactor_selection,
    multiple = TRUE)
}