# Library support
library(data.table) 
library(ggplot2)
library(scales)

# Kuo package
# if (!require(simulationmachine)) {
#   remotes::install_github("kasaai/simulationmachine")
  library(simulationmachine)
# }

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
  
  output$plt_triangle_paid <- renderPlot({
    tbl_tri <- tbl_paid_triangle()
    #convert to cumulative paid
    tbl_tri$cumulative_paid <- tbl_tri[, .(paid = cumsum(paid)), by = c("accident_year")]$paid 
    
    #cumulative paid development by accident year
    ggplot(data = tbl_tri[accident_year + development_year <= 2005],  
           aes(x = development_year, y = cumulative_paid, colour = as.factor(accident_year))) + 
      geom_point() + 
      geom_line() + 
      scale_y_continuous(labels = comma) + 
      ggtitle("Cumulative paid by accident year") +  
      theme(
        legend.title = element_blank(), 
        axis.title.y = element_blank(), 
        plot.caption = element_text(hjust = 0, face = "italic")) + 
      scale_colour_viridis_d() +
      theme_bw() +
      labs(x = "Development year", y = "Cumulative paid", colour = "Accident year")
    
  })
  
  output$plt_lob_paid <- renderPlot({
    
    tbl_rec <- tbl_records()
    
    tbl_rec <- tbl_rec[,.(paid_loss = sum(paid_loss)), by = c("accident_year", "development_year", "lob")]
    setkey(tbl_rec, lob, accident_year, development_year)
    tbl_rec$cumulative_paid <- tbl_rec[,.(paid = cumsum(paid_loss)), by = c("accident_year","lob")]$paid
    
    #cumulative paid development by accident year
    ggplot(
      data = tbl_rec[accident_year + development_year <= 2005],
      aes(x = development_year, y = cumulative_paid, colour = as.factor(accident_year))) + 
      geom_point() + 
      geom_line() + 
      scale_y_continuous(labels = comma) + 
      ggtitle("Cumulative paid by accident year for lob 1 - 4") +  
      theme(
        legend.title = element_blank(), 
        axis.title.y = element_blank(), 
        plot.caption = element_text(hjust = 0, face = "italic")) + 
      scale_colour_viridis_d() +
      theme_bw() +
      labs(x = "Development year", y = "Cumulative paid", colour = "Accident year") + 
      facet_wrap(.~lob)
    
  })
  
  output$plt_age_box <- renderPlot({
    tbl_rec <- tbl_records()
    
    tbl_rec <- tbl_rec[,.(paid_loss = sum(paid_loss)), by = c("claim_id", "age")]
    
    ggplot(tbl_rec[!paid_loss == 0], aes(x = log(paid_loss), fill = cut(age, breaks = c(0,20,30,40,50,60,70,100)))) +
      geom_boxplot()
  })
  
  output$plt_report_delay <- renderPlot({
    tbl_rec <- tbl_records()
    tbl_rec <- tbl_rec[,.(report_delay = max(report_delay)), by = c("claim_id")][,.N, by = report_delay]
    ggplot(tbl_rec, aes(x = report_delay, y = log(N))) + 
      geom_bar(stat = "identity")
    
  })
  
})

getTriangleData <- function(
  num_claims = 2000, 
  lob_distribution = c(0.25, 0.25, 0.25, 0.25), 
  inflation = c(0.03, 0.01, 0.01, 0.01),
  sd_claim = 0.5, 
  sd_recovery = 0.1, 
  seed = NULL, 
  paid_non_negative = FALSE, 
  validation_type = "random"){
  
  # set up the simulation
  charm <- simulation_machine(
    num_claims, # Parameter for the expected total number of claims in the simulation output 
    lob_distribution, # there are 4 lines of business, so the proportions must sum to 1
    inflation, # inflation per year for each lob
    sd_claim, # how volatile are claim amounts?
    sd_recovery # how volatile are recovery payments?
  )
  
  # simulate the data and store it in a variable
  # setting a seed is optional but ensures the same output for a given seed
  records <- as.data.table(conjure(charm, seed = seed)) 
  
  #convert some fields to factors for convenience later
  records$lob <- as.factor(records$lob)
  records$cc <- as.factor(records$cc)
  records$injured_part <- as.factor(records$injured_part)
  
  # aggregate by AY and dev year
  # sum paid transactions by acc and dev year
  paid_triangle_data <- records[,.(paid = sum(paid_loss)), by = c("accident_year","development_year")] 
  paid_triangle_data[, ':='(accident_year_factor = as.factor(accident_year), development_year_factor = as.factor(development_year))]
  paid_triangle_data[, train_ind := (accident_year + development_year <= 2005)]
  
  if (validation_type == "random") {
    paid_triangle_data$fold <- "train"
    validation_rows <- sample(which(paid_triangle_data$train_ind), 23)
    paid_triangle_data[validation_rows]$fold <- "validation"
  } else{
    paid_triangle_data[train_ind == TRUE, fold := ifelse(accident_year + development_year > 2003, "validation", "train")]
  }
  
  paid_triangle_data[train_ind == FALSE, fold := "test"]
  
  if (paid_non_negative == TRUE) {
    paid_triangle_data[paid < 0, paid := 0]
  }
  
  list(
    records = records, 
    paid_triangle_data = paid_triangle_data
  )
  
}
