# add_tanget UserTangetAddition

# Function to allow user to add tangent of max growth rate
add_tangent <- function(add_tanget){
  if(is.null(read_data)){return()}
  
  radioButtons(
    inputId = "add_tanget",
    label = "Add tanget of µmax to growth plots",
    choices = c(FALSE, TRUE),
    selected = add_tanget
  )
}