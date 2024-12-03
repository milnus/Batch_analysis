# Function to allow user to add tangent of max growth rate
add_tangent <- function(add_tanget) {
  radioButtons(
    inputId = "add_tanget",
    label = "Add tanget of µmax to growth plots",
    choices = c(FALSE, TRUE),
    selected = add_tanget
  )
}
