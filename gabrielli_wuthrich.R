# Library support
library(data.table) 
library(ggplot2)
library(scales)

# Kuo package
if (!require(simulationmachine)) {
  remotes::install_github("kasaai/simulationmachine")
  library(simulationmachine)
}

tab_gabrielli_wutrich <- tabPanel(
  'Gabrielli Wütrich',
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "num_claims",
        "Choose number of claims:",
        choices = c(100, 500, 1000, 3000, 5000),
        selected = 3000
      ),
      selectInput(
        "sd_claim",
        "Choose sd of claim severity:",
        choices = c(0, 0.1, 0.3, 0.5, 0.7, 0.9, 1, 2, 4),
        selected = 0.3),
      selectInput(
        "sd_recovery",
        "Choose sd of subrogation recoveries:",
        choices = c(0,0.1,0.3, 1,2,4),
        selected = 0.3),
      numericInput(
        "seed",
        "Enter a random seed:",
        value = 1,
        min = 1,
        max = 10^9), 
    ),
    mainPanel(
      h2("Paid plot"),
      plotOutput("plt_triangle_paid", width = "60%"),
      # tableOutput("paidDT"),
      # "Detailed data",
      # tableOutput("detailedDT")
      h2("Paid LOB plot"), 
      plotOutput("plt_lob_paid", width = "60%"),
      h2("Loss distribution by age group"), 
      plotOutput("plt_age_box", width = "60%"),
      h2("Log(claim count) by reporting delay"), 
      plotOutput("plt_report_delay", width = "60%")
    )
  )
)

expr_gabrielli_wutrich <- quote({
  
  obj_claim_data <- reactiveVal(NULL)
  tbl_paid_triangle <- reactiveVal(NULL)
  tbl_records <- reactiveVal(NULL)

  # req(input$seed) #stops app crashing if user clears seed input box!
  observe({
    obj_claim_data(getTriangleData(
      num_claims = as.numeric(input$num_claims),
      sd_claim = as.numeric(input$sd_claim),
      sd_recovery = as.numeric(input$sd_recovery),
      seed = as.numeric(input$seed)
    ))
    tbl_paid_triangle(obj_claim_data()$paid_triangle_data)
    tbl_records(obj_claim_data()$records)
  })
  
  output$plt_triangle_paid <- renderPlot(plotTriangle(tbl_paid_triangle()))
  output$plt_lob_paid <- renderPlot(plotLOBTriangle(tbl_records()))
  output$plt_age_box <- renderPlot(plotAgeBoxplot(tbl_records()))
  output$plt_report_delay <- renderPlot(plotReportingDelay(tbl_records()))
  
  
})
