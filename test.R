tab_test = fluidPage(
  fluidRow(
    column(2, wellPanel(p("Column width 2"))),
    column(10, wellPanel(p("Column width 10")))),
  fluidRow(
    column(4, wellPanel(p("Column width 4"))),
    column(4, wellPanel(p("Column width 4"))),
    column(4, wellPanel(p("Column width 4"))))
)


expr_test = function(input, output){    
  
  # server code
}
