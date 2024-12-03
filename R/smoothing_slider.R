# Function to allow user to add tangent of max growth rate
smoothing_slider <- function(spline_smoothing) {
  sliderInput(
    inputId = "spline_smoothing",
    label = "Smoothing of the spline fitted to OD values",
    min = 0.00,
    max = 1.00,
    value = 1.00,
    step = 0.05, ticks = F
  )
}
