# Function to allow user to add tangent of max growth rate
high_mu_range_slider <- function(spline_smoothing){
  sliderInput(
    inputId = "high_mu_percentage",
    label = "Define percentage of\nµmax considered as high",
    min = 1, 
    max = 100, 
    value = 90, 
    step = 1,
    ticks = F, 
    post = "%", 
  )
}