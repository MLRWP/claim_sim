library(shiny)
library(tidyverse)

source('main.R')
source('imaginator.R')
source('synthetic.R')
source('cascsim.R')
source('gabrielli_wuthrich.R')
source('data_tables.R')

options(shiny.reactlog = TRUE) 

ui <- navbarPage(
    title = "Claim sim"
    , tab_main
    # , tab_gabrielli_wutrich
    , tab_imaginator
    # , tab_synthetic
    # , tab_cascsim
    # , tab_data_tables
    # , tab_compare
)

server <- function(input, output, session) {
    
    # eval(expr_gabrielli_wutrich)
    eval(expr_imaginator)
    # eval(expr_cascsim)
    # eval(expr_synthetic)
    # eval(expr_compare)
    # eval(expr_data_tables)
}

shinyApp(ui, server)
