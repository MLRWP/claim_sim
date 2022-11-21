library(cascsim)

tab_cascsim <- tabPanel(
  'cascsim',
  fluidRow(
    column(
      width = 3,
      numericInput(
        "num_severity_mean",
        "Set the severity mean",
        min = .01,
        step = 5,
        value = 10e3
      ),
      numericInput(
        "num_severity_cv",
        "Set the severity cv",
        min = .01,
        step = 5,
        value = 1.5
      ),
      numericInput(
        "num_frequency_mean",
        "Set the frequency mean",
        min = .01,
        step = 5,
        value = 10
      ),
      # numericInput(
      #   "num_frequency_cv",
      #   "Set the frequency cv",
      #   min = .01,
      #   step = 5,
      #   value = 0.75
      # ),
      numericInput(
        "num_sims_cascsim",
        "How many simulations would you like?",
        min = 1,
        step = 5,
        value = 5
      )
    ),
    column(
      width = 9,
      h1("Cascsim data"), 
      downloadButton('download_cascsim_data', 'Download cascsim data'),
      dataTableOutput("tbl_cascsim")
    )
  )

)

expr_cascsim <- quote({
  
  tbl_cascsim <- reactiveVal(NULL)
  
  start_date <- lubridate::make_date(2000, 1, 1)

  obj_claim_type <- new(
    "ClaimType",
    line = "General Liability",
    claimType = "Bodily injury"
  )
  
  obj_claim_type@exposureIndex <- setIndex(
    new(
      "Index",
      indexID = "Exposue",
      tabulate = FALSE,
      startDate = start_date, 
      annualizedRate = 0)
  ) 
  
  obj_claim_type@severityIndex <- setIndex(
    new(
      "Index",
      indexID = "Severity index",
      tabulate = FALSE,
      startDate = start_date, 
      annualizedRate = 0.02)
  ) 
  
  obj_claim_type@p0 <- new(
    "DevFac",
    meanList = c(0, 0),
    volList = c(0, 0)
  )
  
  obj_claim_type@reportLag <- new("Exponential", p1 = 0.1)
  obj_claim_type@settlementLag <- new("Exponential", p1 = 0.05)
  obj_claim_type@iCopula <- FALSE
  
  obj_claim_type@laeDevFac <- new(
    "DevFac", 
    FacID = "F1", 
    FacModel = TRUE, 
    fun = "linear",
    paras = c(5, 1.5, 0.005, 1.2, 3)
  )
  
  obj_claim_type@fIBNER <- new(
    "DevFac",
    FacID = "D1",
    FacModel = FALSE,
    meanList = c(1.2, 1.15, 1.1, 1.05, 1),
    volList = c(0, 0, 0, 0, 0)
  )
  
  observe({
    
    lognormal_sigma <- sqrt(log(input$num_severity_cv^2 + 1))
    lognormal_mu <- log(input$num_severity_mean) - lognormal_sigma^2/2

    obj_claim_type@frequency <- new("Poisson", p1 = input$num_frequency_mean)
    obj_claim_type@severity <- new("Lognormal", p1 = lognormal_mu, p2 = lognormal_sigma)

    obj_simulation <- new(
      "Simulation",
      lines = "General Liability",
      types = "Bodily injury",
      claimobjs = list(obj_claim_type),
      workingFolder = tempdir()
    )
    
    obj_simulation@simNo <- input$num_sims_cascsim
    obj_simulation@iRBNER <-FALSE
    obj_simulation@iROPEN <-FALSE
    obj_simulation@iIBNR <-TRUE
    obj_simulation@iUPR <-FALSE
  
    tbl_cascsim(
      claimSimulation(
        obj_simulation,
        data.frame(),
        startDate = start_date,
        evaluationDate = as.Date("2004-12-31"),
        futureDate = as.Date("2005-12-31")
      )
    )
    
    output$download_cascsim_data <- downloadHandler(
      filename = function() {
        paste0('cascsim-data-', Sys.Date(), '.csv')
      },
      content = function(con) {
        write_csv(tbl_cascsim(), con)
      },
      contentType = 'text/csv'
    )

  })

  output$tbl_cascsim <- renderDataTable(
    tbl_cascsim(),
    options = list(
      filter = 'top'
    )
  )

})
