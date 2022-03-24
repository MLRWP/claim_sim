library(imaginator)

# panel_payment_frequency <- discrete_picker_ui(
#   
# )

panel_occurrence_wait <- distribution_picker_ui(
  'occurrence_wait', 'Wait time until occurrence', list(min = 1, step = 5, value = 1))
panel_report_wait <- distribution_picker_ui(
  'report_wait', 'Wait time until report', list(min = 1, step = 5, value = 1))
panel_pay_wait <- distribution_picker_ui(
  'pay_wait', 'Wait time between payments', list(min = 1, step = 5, value = 1))
panel_pay_severity <- distribution_picker_ui(
  'pay_severity', 'Claim payment severity', list(min = 1, step = 250, value = 500))

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
      wellPanel(
        panel_occurrence_wait$radio,
        panel_occurrence_wait$fixed,
        panel_occurrence_wait$distribution
      ),
      wellPanel(
        panel_report_wait$radio,
        panel_report_wait$fixed,
        panel_report_wait$distribution
      ),
      wellPanel(
        panel_pay_wait$radio,
        panel_pay_wait$fixed,
        panel_pay_wait$distribution
      ),
      wellPanel(
        panel_pay_severity$radio,
        panel_pay_severity$fixed,
        panel_pay_severity$distribution
      )
    ),
    mainPanel(
      h2("Policy counts by policy year"),
      plotOutput("plt_policies", width = "60%"),
      h2("Claim counts by policy year"),
      plotOutput("plt_claim_counts_by_policy_year", width = "60%"),
      h2('Payment totals by policy year'),
      plotOutput("plt_payment_totals_by_policy_year", width = "60%"),
      h2('Payment totals by accident year'),
      plotOutput("plt_payment_totals_by_accident_year", width = "60%")
    )
  )
)

expr_imaginator <- quote({
  
  tbl_policies <- reactiveVal(NULL)
  tbl_claim_transactions <- reactiveVal(NULL)
  tbl_claims <- reactiveVal(NULL)
  dist_claim_frequency <- reactiveVal(NULL)
  dist_occurrence_wait <- reactiveVal(NULL)
  dist_report_wait <- reactiveVal(NULL)
  dist_pay_wait <- reactiveVal(NULL)
  dist_pay_severity <- reactiveVal(NULL)

  observe({

    tbl_policies(
      policies_simulate(
        n = input$num_starting_policies,
        num_years = input$num_policy_years,
        retention = input$retention,
        growth = input$growth_rate)    
    )

    dist_occurrence_wait(
      if (input$rdo_occurrence_wait_random == 'Fixed') {
        distributions3::Uniform(input$fixed_occurrence_wait, input$fixed_occurrence_wait)
      } else {
        if (input$occurrence_wait_distribution == 'Uniform') {
          distributions3::Uniform(
            input$occurrence_wait_uniform_lower_bound, 
            input$occurrence_wait_uniform_upper_bound
          )
        } else if (input$occurrence_wait_distribution == 'Normal') {
          distributions3::Normal(
            input$occurrence_wait_normal_mu,
            input$occurrence_wait_normal_cv * input$occurrence_wait_normal_mu
          )
        }
      }
    )
    
    dist_report_wait(
      if (input$rdo_report_wait_random == 'Fixed') {
        distributions3::Uniform(input$fixed_report_wait, input$fixed_report_wait)
      } else {
        if (input$report_wait_distribution == 'Uniform') {
          distributions3::Uniform(
            input$report_wait_uniform_lower_bound, 
            input$report_wait_uniform_upper_bound
          )
        } else if (input$report_wait_distribution == 'Normal') {
          distributions3::Normal(
            input$report_wait_normal_mu,
            input$report_wait_normal_cv * input$report_wait_normal_mu
          )
        }
      }
    )
    
    dist_pay_wait(
      if (input$rdo_pay_wait_random == 'Fixed') {
        distributions3::Uniform(input$fixed_pay_wait, input$fixed_pay_wait)
      } else {
        if (input$pay_wait_distribution == 'Uniform') {
          distributions3::Uniform(
            input$pay_wait_uniform_lower_bound, 
            input$pay_wait_uniform_upper_bound
          )
        } else if (input$pay_wait_distribution == 'Normal') {
          distributions3::Normal(
            input$pay_wait_normal_mu,
            input$pay_wait_normal_cv * input$pay_wait_normal_mu
          )
        }
      }
    )
    
    dist_pay_severity(
      if (input$rdo_pay_severity_random == 'Fixed') {
        distributions3::Uniform(input$fixed_pay_severity, input$fixed_pay_severity)
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
      tbl_claim <- claims_by_wait_time(
        tbl_policies(),
        claim_frequency = 1,
        payment_frequency = 1,
        occurrence_wait = dist_occurrence_wait(),
        report_wait = dist_report_wait(),
        pay_wait = dist_pay_wait(),
        pay_severity = dist_pay_severity()
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
  
  output$plt_payment_totals_by_accident_year <- renderPlot({
    
    validate(need(tbl_claim_transactions(), "Waiting on claim transactions"))
    
    tbl_claims() %>%
      mutate(accident_year = lubridate::floor_date(occurrence_date, unit = 'year')) %>%
      group_by(accident_year) %>%
      summarise(claim_amounts = sum(payment_amount)) %>%
      ggplot(aes(accident_year, claim_amounts)) +
      geom_bar(stat = 'identity')
  })
  
})
