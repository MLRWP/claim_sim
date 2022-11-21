library(imaginator)
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
        ),
        # h3("Policy growth and retention"),
        # fluidRow(
        #   column(6, 
        #     wellPanel(
        #     ),
        #   ), 
        #   column(6, 
        #     wellPanel(
        #     ),
        #   ),
        # ),
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
        selected = 'transactions'
      ),
      dataTableOutput("tbl_data_table_im")
    )
  )
)

expr_imaginator <- quote({
  
  tbl_policies <- reactiveVal(NULL)
  tbl_claim_transactions <- reactiveVal(NULL)
  tbl_claims <- reactiveVal(NULL)
  
  # dist_policyholder_growth <- reactiveVal(NULL)
  # dist_policyholder_retention <- reactiveVal(NULL)

  dist_claim_frequency <- distributions3_server("claim_frequency")
  dist_payment_frequency <- distributions3_server("payment_frequency")
  dist_occurrence_wait <- distributions3_server("wait_occurrence")
  dist_report_wait <- distributions3_server("wait_report")
  dist_payment_wait <- distributions3_server("wait_payment")
  dist_payment_severity <- distributions3_server("payment_severity")
  
  observe({

    tbl_policies(
      policies_simulate(
        n = input$num_starting_policies,
        num_years = input$years_exposure,
        retention = 1,
        growth = 0
      )
    )
    
    tbl_claim_transactions({
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
    
    if (input$rdo_data_table_im == 'policies') {
      output$tbl_data_table_im <- renderDataTable(
        tbl_policies(),
        options = list(
          filter = 'top'
        )
      )
    } else if (input$rdo_data_table_im == 'claims') {
      output$tbl_data_table_im <- renderDataTable(
        tbl_claims(),
        options = list(
          filter = 'top'
        )
      )
    } else {
      output$tbl_data_table_im <- renderDataTable(
        tbl_claim_transactions(),
        options = list(
          filter = 'top'
        )
      )
    }
    
  })

})
