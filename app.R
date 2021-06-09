library(shiny)
library(tidyverse)

source('main.R')
source('imaginator.R')
source('cascsim.R')
source('spedicato.R')

options(shiny.reactlog = TRUE) 

ui <- navbarPage(
    title = "Claim sim"
    , tab_main
    , tab_imaginator
    , tab_cascsim
    , tab_spedicato
    # , tab_compare
)

server <- function(input, output, session) {
    
    eval(expr_imaginator)
    eval(expr_cascsim)
    eval(expr_spedicato)
    # eval(expr_compare)
    
}

shinyApp(ui, server)
