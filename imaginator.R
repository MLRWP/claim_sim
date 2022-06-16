library(imaginator)
source("distributions3_ui.R")

tab_imaginator <- tabPanel(
  'imaginator',
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
    )
  ),
  fluidRow(
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
      h3("Claim frequency"),
      wellPanel(
        distributions3_ui("claim_frequency")
      ),
      h3("Payment frequency"),
      wellPanel(
        distributions3_ui("payment_frequency")
      ),
    ), 
    column(
      width = 4,
      h3("Occurrence wait time"),
      wellPanel(
        distributions3_ui("wait_occurrence")
      ),
      h3("Report wait time"),
      wellPanel(
        distributions3_ui("wait_report")
      ),
    ),
    column(
      width = 4,
      h3("Payment wait"),
      wellPanel(
        distributions3_ui("wait_payment")
      ),
      h3("Payment severity"),
      wellPanel(
        distributions3_ui("payment_severity")
      )
    ),
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
    
  })

})
