library(imaginator)
source("distributions3_ui.R")

tab_imaginator <- tabPanel(
  'imaginator',
  fluidRow(
    column(
      width = 3,
      numericInput(
        "num_starting_policies",
        "Set the number of initial policies",
        min = 10,
        max = 10e3,
        step = 5,
        value = 10
      ),
      numericInput(
        "num_policy_years",
        "Choose number of policy years:",
        min = 2,
        max = 20,
        step = 1,
        value = 5
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
      width = 3,
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
      width = 3,
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
      width = 3,
      h3("Payment wait"),
      wellPanel(
        distributions3_ui("wait_payment")
      ),
      h3("Payment severity"),
      wellPanel(
        distributions3_ui("payment_severity")
      )
    ),
    # fluidRow(
    #   column(
    #     3, 
    #     h2("Policy counts by policy year"),
    #     plotOutput("plt_policies", width = "60%")
    #   ),
    #   column(
    #     3, 
    #     h2("Claim counts by policy year"),
    #     plotOutput("plt_claim_counts_by_policy_year", width = "60%")
    #   ),
    #   column(
    #     3, 
    #     h2('Payment totals by policy year'),
    #     plotOutput("plt_payment_totals_by_policy_year", width = "60%")
    #   ),
    #   column(
    #     3, 
    #     h2('Payment totals by accident year'),
    #     plotOutput("plt_payment_totals_by_accident_year", width = "60%")
    #   )
    # )
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
        num_years = input$num_policy_years,
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

  output$plt_policies <- renderPlot({

    validate(need(tbl_policies(), "Waiting on policy data"))

    tbl_policies() %>%
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>%
      ggplot(aes(policy_year)) +
      geom_bar() +
      theme_minimal()
  })
  
  output$plt_claim_counts_by_policy_year <- renderPlot({

    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))

    tbl_claims() %>%
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>%
      group_by(policy_year) %>%
      summarise(n_claims = n()) %>%
      ggplot(aes(policy_year, n_claims)) +
      geom_bar(stat = 'identity') +
      theme_minimal()
  })

  output$plt_payment_totals_by_policy_year <- renderPlot({

    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))

    tbl_claims() %>%
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>%
      group_by(policy_year) %>%
      summarise(claim_amounts = sum(payment_amount)) %>%
      ggplot(aes(policy_year, claim_amounts)) +
      geom_bar(stat = 'identity') +
      theme_minimal()
  })

  output$plt_payment_totals_by_accident_year <- renderPlot({

    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))

    tbl_claims() %>%
      mutate(accident_year = lubridate::floor_date(occurrence_date, unit = 'year')) %>%
      group_by(accident_year) %>%
      summarise(claim_amounts = sum(payment_amount)) %>%
      ggplot(aes(accident_year, claim_amounts)) +
      geom_bar(stat = 'identity') +
      theme_minimal()
  })

})
