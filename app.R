library(shiny)
library(tidyverse)

source('main.R')
source('imaginator.R')
source('synthetic.R')
# source('cascsim.R')
source("plots.R")

# source('gabrielli_wuthrich.R')
# source('data_tables.R')

options(shiny.reactlog = TRUE) 

ui <- navbarPage(
    title = "Claim sim"
    , tab_main
    , tab_synthetic
    , tab_imaginator
    # , tab_gabrielli_wutrich
    # , tab_cascsim
    , tab_plots
    # , tab_data_tables
    # , tab_compare
)

server <- function(input, output, session) {

  eval(expr_synthetic)
  eval(expr_imaginator)
  # eval(expr_gabrielli_wutrich)
  # eval(expr_cascsim)
  eval(expr_plots)
  # eval(expr_compare)
  # eval(expr_data_tables)
}

shinyApp(ui, server)
