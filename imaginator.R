library(imaginator)
library(DT)
source("distributions3_ui.R")

tab_imaginator <- tabPanel(
  'imaginator',
  fluidRow(
    column(
      width = 4,
      fluidRow(
        column(
          width = 12,
          numericInput(
            "num_starting_policies",
            "Set the number of initial policies",
            min = 10,
            max = 10e3,
            step = 5,
            value = 10
          ),
          numericInput(
            "num_policy_retention",
            "Policy retention: Values between 0 and 1",
            min = 0.01,
            max = 1,
            value = 1,
            step = 0.01
          ),
          numericInput(
            "num_policy_growth",
            "Rate of policy growth",
            min = 0,
            max = 5,
            step = .01,
            value = 0
          ),
        ),
        column(
          width = 4,
          h4("Claim frequency"),
          wellPanel(
            distributions3_ui("claim_frequency")
          ),
          h4("Payment frequency"),
          wellPanel(
            distributions3_ui("payment_frequency")
          ),
        ), 
        column(
          width = 4,
          h4("Occurrence wait time"),
          wellPanel(
            distributions3_ui("wait_occurrence")
          ),
          h4("Report wait time"),
          wellPanel(
            distributions3_ui("wait_report")
          ),
        ),
        column(
          width = 4,
          h4("Payment wait"),
          wellPanel(
            distributions3_ui("wait_payment")
          ),
          h4("Payment severity"),
          wellPanel(
            distributions3_ui("payment_severity")
          )
        ),
      )
    ),
    column(
      width = 8,
      h1("Imaginator data"), 
      downloadButton('download_imaginator_policies', 'Download policies'),
      downloadButton('download_imaginator_claims', 'Download claims'),
      downloadButton('download_imaginator_claim_transactions', 'Download claim transactions'),
      radioButtons(
        'rdo_data_table_im',
        'Which data table would you like to see?',
        choices = c('Policies' = 'policies', 'Claims' = 'claims', 'Claim transactions' = 'transactions'),
        # selected = 'transactions'
        selected = 'policies'
      ),
      DTOutput("tbl_data_table_im")
    )
  )
)

expr_imaginator <- quote({
  
  tbl_policies <- reactiveVal(NULL)
  tbl_claim_transactions <- reactiveVal(NULL)
  tbl_claims <- reactiveVal(NULL)
  # tbl_data_table_im <- reactiveVal(NULL)
  
  # dist_policyholder_growth <- reactiveVal(NULL)
  # dist_policyholder_retention <- reactiveVal(NULL)

  dist_claim_frequency <- distributions3_server("claim_frequency")
  dist_payment_frequency <- distributions3_server("payment_frequency")
  dist_occurrence_wait <- distributions3_server("wait_occurrence")
  dist_report_wait <- distributions3_server("wait_report")
  dist_payment_wait <- distributions3_server("wait_payment")
  dist_payment_severity <- distributions3_server("payment_severity")
  
  observe({

    tbl_policies({
      validate(
        need(is.numeric(input$num_policy_retention), "Please enter a numeric value for policy retention"),
        need(input$num_policy_retention > 0.8, "Policy retention > 0.8")
      )
      policies_simulate(
        n = input$num_starting_policies,
        num_years = input$years_exposure,
        retention = input$num_policy_retention,
        growth = input$num_policy_growth
      )
    })
    
    tbl_claim_transactions({
      validate(need(tbl_policies(), "Waiting on policy table"))
      tbl_claim <- claims_by_wait_time(
        tbl_policies(),
        claim_frequency = dist_claim_frequency(),
        payment_frequency = dist_payment_frequency(),
        occurrence_wait = dist_occurrence_wait(),
        report_wait = dist_report_wait(),
        pay_wait = dist_payment_wait(),
        pay_severity = dist_payment_severity()
      )

    })

    tbl_claims({
      validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))
      tbl_claim_transactions() %>%
        group_by(claim_id, policy_effective_date, occurrence_date, report_date, number_of_payments) %>%
        summarise(
          payment_amount = sum(payment_amount, na.rm = TRUE)
        )
    })
    
  })

  output$download_imaginator_policies <- downloadHandler(
    filename = function() {
      paste0('imaginator-policies-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write_csv(tbl_policies(), con)
    },
    contentType = 'text/csv'
  )
  
  output$download_imaginator_claims <- downloadHandler(
    filename = function() {
      paste0('imaginator-claims-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write_csv(tbl_claims(), con)
    },
    contentType = 'text/csv'
  )
  
  output$download_imaginator_claim_transactions <- downloadHandler(
    filename = function() {
      paste0('imaginator-claim-transactions-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write_csv(tbl_claim_transactions(), con)
    },
    contentType = 'text/csv'
  )
  
  output$tbl_data_table_im <- renderDT(
    switch(
      input$rdo_data_table_im,
      policies = tbl_policies(),
      claims = tbl_claims(),
      transactions = tbl_claim_transactions(),
    )
  )
  
})
