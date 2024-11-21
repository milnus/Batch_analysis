library(shiny)
library(ggplot2)
library(ggprism)
library(QurvE)
options(shiny.maxRequestSize = 100*1024^2)

fluidPage(
  
  titlePanel("Batch growth analysis"),
  
  sidebarPanel(
    fileInput('upload', label = 'PioReactor OD table'),
    radioButtons('filt_strat', label = "Filtering strategy", choices = c('Remove', 'Keep')),
    uiOutput("UserFilters"),
    # textInput("reactor_groups", "Reactor Group - Members of a ground is comma seperated and groups are semicolon seperated (example P01,P02,P03;P04,P05,P06 is two groups if three reactors)"),
    actionButton("process", "Process data"),
    uiOutput("UserPointRemoval"),
    
    # Insert version text
    div("version 0.0.2")
  ),
  
  mainPanel(
    plotOutput('raw_data_plot'),
    
    downloadButton("download_growth_rate_plot", label = "Download Plot"),
    plotOutput('plot'),
    # Add tabbox displaying the output plots where raw and used data are visualised.
    # It should be noted that if there are too few utilised data then the delta can be lowered.
    # Make possible to download the plots.
    downloadButton("download_raw_table", label = "Download Raw Data"),
    downloadButton("download_table", label = "Download Summarised Data"),
    tableOutput("table"),
  )
)
