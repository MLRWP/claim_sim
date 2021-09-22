library(shiny)
library(tidyverse)

source('main.R')
source('imaginator.R')
source('cascsim.R')
source('spedicato.R')
source('gabrielli_wuthrich.R')
source('data_tables.R')

options(shiny.reactlog = TRUE) 

ui <- navbarPage(
    title = "Claim sim"
    , tab_main
    , tab_gabrielli_wutrich
    , tab_imaginator
    , tab_cascsim
    , tab_spedicato
    , tab_data_tables
    # , tab_compare
)

server <- function(input, output, session) {
    
    eval(expr_gabrielli_wutrich)
    eval(expr_imaginator)
    eval(expr_cascsim)
    eval(expr_spedicato)
    # eval(expr_compare)
    eval(expr_data_tables)
}

shinyApp(ui, server)
