
tab_plots <- tabPanel(
  'Visualizations',
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "rdo_plots_which_engine",
        "Which engine?",
        choices = c('SPLICE', 'Imaginator'),
        selected = 'SPLICE'
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.rdo_plots_which_engine == 'SPLICE'",
        plotOutput("plot_n_vector"),
        # plotOutput("plot_claim_size"),
        plotOutput("plot_CL")
        # plotOutput("plot_notidel"),
        # plotOutput("plot_setldel"),
        # plotOutput("hist_no_payments"),
        # plotOutput("hist_size_payments"),
        # plotOutput("plot_claims")
      ),
      conditionalPanel(
        condition = "input.rdo_plots_which_engine == 'Imaginator'",
        fluidRow(
          column(
            3,
            h2("Policy counts by policy year"),
            plotOutput("plt_policies", width = "60%")
          ),
          column(
            3,
            h2("Claim counts by policy year"),
            plotOutput("plt_claim_counts_by_policy_year", width = "60%")
          ),
          column(
            3,
            h2('Payment totals by policy year'),
            plotOutput("plt_payment_totals_by_policy_year", width = "60%")
          ),
          column(
            3,
            h2('Payment totals by accident year'),
            plotOutput("plt_payment_totals_by_accident_year", width = "60%")
          )
        )
      )
    )
  )
)

expr_plots <- quote({

  output$plot_n_vector <- renderPlot({
    plot(
      x = 1:I(), 
      y = n_vector(), 
      type = "l",
      main = paste("Module 1: Claim frequency simulated from the", input$Occurence_selection, "option"),
      xlab = "Occurrence period", 
      ylab = "# Claims"
    )
  })
  
  output$plot_CL <- renderPlot({
    # plot(tmp_cumm[,1])
    ChainLadder::plot(tmp_cumm)
  })
  
  output$plot_claim_size <- renderPlot({
    
    # flag> xlim needs to be flexible
    plot(
      ecdf(unlist(claim_sizes_default())), 
      xlim = c(0, 2000000),
      main = "Module 2: Empirical distribution of simulated claim sizes",
      xlab = "Individual claim size")

    plot(
      ecdf(unlist(claim_sizes())), 
      add = TRUE, 
      col = 2
    )

    legend.text <- c("Default", input$Occurence_size)
    legend("bottomright", legend.text, col = 1:3, lty = 1, bty = "n")
    
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
