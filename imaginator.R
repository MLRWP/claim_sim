library(imaginator)

panel_payment_severity <- list(
  radio = radioButtons(
    "rdo_payment_severity_random",
    "Severity of each payment",
    choices = c("Fixed", "Random"),
    inline = TRUE
  ),
  fixed = conditionalPanel(
    condition = "input.rdo_payment_severity_random == 'Fixed'",
    numericInput(
      "pay_severity",
      "Severity of each payment",
      min = 1,
      max = 10e3,
      step = 50,
      value = 500
    )
  ),
  distribution = conditionalPanel(
    condition = "input.rdo_payment_severity_random == 'Random'",
    selectInput(
      "pay_severity_distribution",
      "Payment severity distribution",
      choices = c('Uniform', 'Normal', 'Gamma', 'Exponential')
    ),
    conditionalPanel(
      condition = "input.pay_severity_distribution == 'Uniform'",
      numericInput(
        "pay_severity_uniform_lower_bound",
        "Lower bound",
        min = 1,
        step = 50,
        value = 100
      ),
      numericInput(
        "pay_severity_uniform_upper_bound",
        "Upper bound",
        min = 1,
        step = 50,
        value = 10e3
      )
    ),
    conditionalPanel(
      condition = "input.pay_severity_distribution == 'Normal'",
      numericInput(
        "pay_severity_normal_mu",
        "Mu",
        min = 1,
        step = 50,
        value = 500
      ),
      numericInput(
        "pay_severity_normal_cv",
        "Coefficient of variation",
        min = .01,
        step = .01,
        value = .2
      )
    )
  )
)

tab_imaginator <- tabPanel(
  'imaginator',
  sidebarLayout(
    sidebarPanel(
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
      h4("Policy growth and retention"),
      fluidRow(
        column(6, 
          numericInput(
            "growth_rate",
            "Policyholder growth rate:",
            min = 0,
            max = 10,
            step = .01,
            value = 0
          ),
        ), 
        column(6, 
          numericInput(
            "retention",
            "Policyholder retention:",
            min = 0,
            max = 1,
            step = 0.01,
            value = 1
          ),
        ),
      ),
      numericInput(
        "claim_frequency",
        "Claim frequency",
        min = 1,
        max = 50,
        step = 1,
        value = 1
      ),
      numericInput(
        "payment_frequency",
        "Payment frequency",
        min = 1,
        max = 50,
        step = 1,
        value = 1
      ),
      numericInput(
        "occurrence_wait",
        "Occurrence wait time in days:",
        min = 1,
        max = 365,
        step = 1,
        value = 10
      ),
      numericInput(
        "report_wait",
        "Report wait time in days:",
        min = 1,
        max = 3650,
        step = 5,
        value = 5
      ),
      radioButtons(
        "rdo_wait_time_payment_random",
        "Wait time between each payment",
        choices = c("Fixed", "Random"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.rdo_wait_time_payment_random == 'Fixed'",
        numericInput(
          "wait_time_payment",
          "Wait time between each payment",
          min = 1,
          max = 10e3,
          step = 1,
          value = 5
        )
      ),
      conditionalPanel(
        condition = "input.rdo_wait_time_payment_random == 'Random'",
        selectInput(
          "wait_time_payment_distribution",
          "Wait time between payments distribution",
          choices = c('Uniform', 'Normal', 'Gamma', 'Exponential')
        ),
        conditionalPanel(
          condition = "input.wait_time_payment_distribution == 'Uniform'",
          numericInput(
            "wait_time_payment_uniform_lower_bound",
            "Lower bound",
            min = 1,
            step = 1,
            value = 1
          ),
          numericInput(
            "wait_time_payment_uniform_upper_bound",
            "Upper bound",
            min = 1,
            step = 1,
            value = 10
          )
        )
      ),
      panel_payment_severity$radio,
      panel_payment_severity$fixed,
      panel_payment_severity$distribution
    ),
    mainPanel(
      h2("Policy counts by policy year"),
      plotOutput("plt_policies", width = "60%"),
      h2("Claim counts by policy year"),
      plotOutput("plt_claim_counts_by_policy_year", width = "60%"),
      h2('Payment totals by policy year'),
      plotOutput("plt_payment_totals_by_policy_year", width = "60%"),
    )
  )
)

expr_imaginator <- quote({
  
  tbl_policies <- reactiveVal(NULL)
  tbl_claim_transactions <- reactiveVal(NULL)
  tbl_claims <- reactiveVal(NULL)
  dist_claim_frequency <- reactiveVal(NULL)
  dist_pay_severity <- reactiveVal(NULL)

  observe({

    tbl_policies(
      policies_simulate(
        n = input$num_starting_policies,
        num_years = input$num_policy_years,
        retention = input$retention,
        growth = input$growth_rate)    
    )

    dist_pay_severity(
      if (input$rdo_payment_severity_random == 'Fixed') {
        distributions3::Uniform(input$pay_severity, input$pay_severity)
      } else {
        if (input$pay_severity_distribution == 'Uniform') {
          distributions3::Uniform(
            input$pay_severity_uniform_lower_bound, 
            input$pay_severity_uniform_upper_bound
          )
        } else if (input$pay_severity_distribution == 'Normal') {
          distributions3::Normal(
            input$pay_severity_normal_mu,
            input$pay_severity_normal_cv * input$pay_severity_normal_mu
          )
        }
      }
    )

    tbl_claim_transactions({
      dist_pay_severity_val <- dist_pay_severity()
      if (inherits(dist_pay_severity_val, 'function')) {
        NULL
      } else {
        claims_by_wait_time(
          tbl_policies(),
          claim_frequency = input$claim_frequency,
          payment_frequency = input$payment_frequency,
          occurrence_wait = input$occurrence_wait,
          report_wait = input$report_wait,
          pay_wait = input$pay_wait,
          pay_severity = dist_pay_severity_val
        )
      }
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
    
    tbl_policies() %>% 
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>% 
      ggplot(aes(policy_year)) + 
      geom_bar()
  })
  
  output$plt_claim_counts_by_policy_year <- renderPlot({

    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))

    tbl_claims() %>%
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>%
      group_by(policy_year) %>%
      summarise(n_claims = n()) %>%
      ggplot(aes(policy_year, n_claims)) +
      geom_bar(stat = 'identity')
  })
  
  output$plt_payment_totals_by_policy_year <- renderPlot({
    
    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))

    tbl_claims() %>%
      mutate(policy_year = lubridate::floor_date(policy_effective_date, unit = 'year')) %>%
      group_by(policy_year) %>%
      summarise(claim_amounts = sum(payment_amount)) %>%
      ggplot(aes(policy_year, claim_amounts)) +
      geom_bar(stat = 'identity')
  })
  
})
