library(SynthETIC)
library(SPLICE)

tab_synthetic <- tabPanel(
  'SPLICE',
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h5("Module 0: Configuration"),
        numericInput(
          "rnd_seed",
          "Random seed",
          min = 0,
          max = 100,
          step = 1,
          value = 50
        ),
        numericInput(
          "ref_claim",
          "Reference claim amount",
          min = 10000,
          max = 100000,
          step = 10000,
          value = 200000
        ),
        numericInput(
          "time_unit_month",
          "Report frequency (months)",
          min = 1,
          max = 12,
          step = 1,
          value = 3
        ),
        numericInput(
          "years",
          "Years",
          min = 1,
          max = 40,
          step = 1,
          value = 2
        )
      ),
      # Start wellPanel: Select Module 1 occurance options
      wellPanel(
        selectInput(
          "Occurence_selection",
          "Module 1: Select occurence options",
          choices = c('Constant exposure and frequency'
                      ,'Increasing exposure and constant frequency'
                      , 'Constant exposure and negative binomial frequency'
                      # 'Constant exposure and zero-truncated Poisson frequency'
                      # 'Increasing exposure and zero-truncated Poisson frequency'
                      )
        ),
        # Start: 1. Constant exposure and frequency
        # years =
        # effective annual exposure rates = 
        # claims freuqency =
        
        conditionalPanel(
          condition = "input.Occurence_selection == 'Constant exposure and frequency'",
          numericInput(
            "eff_ann_exp_rate",
            "Effective Annual Exposure Rate",
            min = 1000,
            max = 2000,
            step = 100,
            value = 1200
          ),
          numericInput(
            "claims_freq",
            "Claims Frequency",
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.05
          )
        ),
        # End: 1. Constant exposure and frequency
        
        # Start: 2. Increasing exposure, constant frequency per unit of exposure
        # years =
        # effective annual exposure rates = 
        # claims freuqency =
        conditionalPanel(
          condition = "input.Occurence_selection == 'Increasing exposure and constant frequency'",
          numericInput(
            "eff_ann_exp_rate",
            "Effective Annual Exposure Rate",
            min = 1000,
            max = 2000,
            step = 100,
            value = 1200
          ),
          numericInput(
            "claims_freq",
            "Claims Frequency",
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.05
          )
        ), 
        # End: 2. Increasing exposure, constant frequency per unit of exposure
        
        # Start 3. Negative binomial claim frequency distribution
        # years =
        # Negative binomial size = 
        # Negative binomial mu = 
        conditionalPanel(
          condition = "input.Occurence_selection == 'Constant exposure and negative binomial frequency'",
          numericInput(
            "occurence_neg_bin_size",
            "Negative binomial size",
            min = 90,
            max = 120,
            step = 5,
            value = 100
          ),
          numericInput(
            "occurence_neg_bin_mu",
            "Negative binomial mu",
            min = 90,
            max = 120,
            step = 5,
            value = 100
          )
        )
        # End 3. Negative binomial claim frequency distribution
      ),
      # End wellPanel: Module 1 select occurance options
      
      # Start wellPanel: Module 2 select occurance size options
      wellPanel(
        selectInput(
          "Occurence_size",
          "Module 2: Select size options",
          choices = c('Power normal', 'Weibull')
        ),
        
        # Start: 1. Default
        # conditionalPanel(
        #   condition = "input.Occurence_size == 'Default'",
        #   h5("Default option requires no parameter definition")
        # ),
        # End: 1. Default
        
        # Start: 2. Power normal
        # flag> is left truncation an appropriate parameter to test?
        conditionalPanel(
          condition = "input.Occurence_size == 'Power normal'",
          numericInput(
            "left_trunc",
            "Left truncation (default 30)",
            min = 25,
            max = 35,
            step = 1,
            value = 30
          )
        ),
        # End: 2. Power normal
        
        # Start: 3. Weibull
        # years =
        # effective annual exposure rates = 
        # claims freuqency =
        conditionalPanel(
          condition = "input.Occurence_size == 'Weibull'",
          h5("Weibull option requires no parameter definition")
        )
        # End: 3. Weibull
      ),
      # End wellPanel: Module 2 select occurance size options
      
      # Start wellPanel: Select Module 3 notification delay options
      wellPanel(
        selectInput(
          "Notif_Delay_selection",
          "Module 3: Select notification delay options",
          choices = c(
            'Weibull',
            'Transformed Gamma', 
            'Mixed distribution'
          )
        ),
        # Start: 1. Weibull
        conditionalPanel(
          condition = "input.Notif_Delay_selection == 'Weibull'",
          numericInput(
            "size_weibull_cv",
            "Coefficient of Variation",
            min = 0.10,
            max = 1.00,
            step = 0.10,
            value = 0.70
          )
        ),
        # End: 1. Weibull
        
        # Start: 2. Transformed Gamma distribution
        conditionalPanel(
          condition = "input.Notif_Delay_selection == 'Transformed Gamma'",
          h5("Transformed Gamma option requires no parameter definition")
        ), 
        # End: 2. Transformed Gamma distribution
        
        # Start 3. Mixed distribution
        conditionalPanel(
          condition = "input.Notif_Delay_selection == 'Mixed distribution'",
          h5("Weibull and gamma distributions are used."),
          numericInput(
            "not_delay_3_mix_prob",
            "Probability of Weibull",
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5
          )
        )
        # End 3. Mixed distribution
      ),
      # End wellPanel: Module 3 notification delay options   
      
      # Start wellPanel:Module 4 Closure delay options
      wellPanel(
        selectInput(
          "Cls_delay_selection",
          "Module 4: Closure delay options",
          choices = c('Weibull','Dependence on notification delay')
        ),
        # Start: 1. Weibull
        conditionalPanel(
          condition = "input.Cls_delay_selection == 'Weibull'",
          numericInput(
            "cls_delay_weibull_cv",
            "Coefficient of variation",
            min = 0.1,
            max = 1,
            step = 0.1,
            value = 0.6
          )
        ),
        # End: 1. Weibull
        # Start: 2. Dependence on notification delay
        conditionalPanel(
          condition = "input.Cls_delay_selection == 'Dependence on notification delay'",
          numericInput(
            "cls_delay_nt_dly_cv",
            "Coefficient of variation",
            min = 0.1,
            max = 1,
            step = 0.1,
            value = 0.6
          )
        )
        # End: 2. Dependence on notification delay
      ),
      # End wellPanel: Module 4 select occurance options
      
      
    ),
    # End sidebarPanel
    mainPanel(
      # h2("Claim frequency"),
      plotOutput("plot_n_vector"),
      plotOutput("plot_claim_size"),
      plotOutput("plot_notidel"),
      plotOutput("plot_setldel"),
      plotOutput("hist_no_payments"),
      plotOutput("hist_size_payments"),
      plotOutput("plot_claims")
    )
  )
  # sidebarLayout
)
# tabPanel

expr_synthetic <- quote({
  
  observe({
    # Module 0: Configuration
    set.seed(as.numeric(input$rnd_seed))
    set_parameters(ref_claim = as.numeric(input$ref_claim), 
                   time_unit = as.numeric(input$time_unit_month/12))
    ref_claim <- return_parameters()[1]
    time_unit <- return_parameters()[2]
    years <- as.numeric(input$years)
    I <- years / time_unit
    
    # Module 1: Occurence
    if (input$Occurence_selection == 'Constant exposure and frequency'){

        # Option 1: Constant exposure and frequency
        E <- c(rep(as.numeric(input$eff_ann_exp_rate), I))
        lambda <- c(rep(as.numeric(input$claims_freq), I))
        n_vector <- claim_frequency(I = I, E = E, freq = lambda)
        occurrence_times <- claim_occurrence(frequency_vector = n_vector)

        # Original code
        # E <- c(rep(12e3, I)) # effective annual exposure rates
        # lambda <- c(rep(0.03, I))
        # times <- 10
        # n_vector <- claim_frequency(I, E = E * times, lambda)
        # occurrence_times <- claim_occurrence(n_vector)

    } else if (input$Occurence_selection == 'Increasing exposure and constant frequency'){

      # Option 2: Increasing exposure, constant frequency per unit of exposure
      E <- c(rep(as.numeric(input$eff_ann_exp_rate), I)) + seq(from = 0, by = 100, length = I) # set linearly increasing exposure
      lambda <- c(rep(as.numeric(input$claims_freq), I)) # set constant frequency per unit of exposure
      n_vector <- claim_frequency(I = I, E = E, freq = lambda)
      occurrence_times <- claim_occurrence(frequency_vector = n_vector)

    } else if (input$Occurence_selection == 'Constant exposure and negative binomial frequency'){

      # Option 3: Negative binomial claim frequency distribution
      n_vector <- claim_frequency(I = I,
                                  simfun = rnbinom,
                                  size = as.numeric(input$occurence_neg_bin_size),
                                  mu = as.numeric(input$occurence_neg_bin_mu))
      occurrence_times <- claim_occurrence(frequency_vector = n_vector)
      
    }
    else if (input$Occurence_selection == 'Constant exposure and zero-truncated Poisson frequency'){

      # Option 4: Zero-truncated Poisson claim frequency distribution
      n_vector <- claim_frequency(I = I, simfun = actuar::rztpois, lambda = 90)
      occurrence_times <- claim_occurrence(frequency_vector = n_vector)

    } else if (input$Occurence_selection == 'Increasing exposure and zero-truncated Poisson frequency'){

      # Option 5: Verying frequency across periods
      # Note time_unit not defined

      # set linearly increasing exposure
      E <- c(rep(input$eff_ann_exp_rate, I)) + seq(from = 0, by = 100, length = I)
      # set constant frequency per unit of exposure
      lambda <- c(rep(input$claims_freq, I))
      n_vector <- claim_frequency(I = I, simfun = actuar::rztpois, lambda = time_unit *E* lambda)
      occurrence_times <- claim_occurrence(frequency_vector = n_vector)

    }
    # print(glimpse(I))
    # print(glimpse(n_vector))
    
    # print(glimpse(str(all_claims)))
    output$plot_n_vector <- renderPlot({
      plot(x = 1:I, y = n_vector, type = "l",
           main = paste("Module 1: Claim frequency simulated from the",input$Occurence_selection,"option"),
           xlab = "Occurrence period", ylab = "# Claims")
    })
    
    
    # Module 2: Size
    claim_sizes_default <- claim_size(n_vector)
      
    if (input$Occurence_size == 'Power normal'){

      # claim_sizes <- claim_size(n_vector)

      # Module 2 Option 2: Left truncated power normal distribution, with function
      S_df <- function(s) {
        # truncate
        if (s < input$left_trunc) {
          return(0)
        } else {
          # rescale
          p_trun <- pnorm(s^0.2, 9.5, 3) - pnorm(input$left_trunc^0.2, 9.5, 3)
          p_rescaled <- p_trun/(1 - pnorm(input$left_trunc^0.2, 9.5, 3))
          return(p_rescaled)
        }
      }
      claim_sizes <- claim_size(frequency_vector = n_vector, simfun = S_df, type = "p", range = c(0, 1e24))

      # print(glimpse(claim_sizes))
      
    } else if (input$Occurence_size == 'Weibull'){

      # Module 2 Option 3: Weibull distribution for claim size
      # estimate the weibull parameters to achieve the mean and cv matching that of the built-in test claim dataset
      claim_size_mean <- mean(test_claim_dataset$claim_size)
      claim_size_cv <- cv(test_claim_dataset$claim_size)
      weibull_shape <- get_Weibull_parameters(target_mean = claim_size_mean, target_cv = claim_size_cv)[1]
      weibull_scale <- get_Weibull_parameters(target_mean = claim_size_mean, target_cv = claim_size_cv)[2]

      # simulate claim sizes with the estimated parameters
      claim_sizes <- claim_size(frequency_vector = n_vector,simfun = rweibull, shape = weibull_shape, scale = weibull_scale)

      # print(glimpse(claim_sizes))
    }
    
    output$plot_claim_size <- renderPlot({
      
      # flag> xlim needs to be flexible
      plot(ecdf(unlist(claim_sizes_default)), xlim = c(0, 2000000), 
           main = "Module 2: Empirical distribution of simulated claim sizes",
           xlab = "Individual claim size")
      
      plot(ecdf(unlist(claim_sizes)), add = TRUE, col = 2)
      
      legend.text <- c("Default", input$Occurence_size)
      legend("bottomright", legend.text, col = 1:3, lty = 1, bty = "n")
      
    })
    
    # # Module 3: Notification delay
    notidel_param_default <- function(claim_size, occurrence_period) {
      target_mean <- min(3, max(1, 2-(log(claim_size/(0.50 * ref_claim)))/3))/4 / time_unit
      target_cv <- 0.70
      # convert to Weibull parameters
      shape <- get_Weibull_parameters(target_mean, target_cv)[1]
      scale <- get_Weibull_parameters(target_mean, target_cv)[2]
      c(shape = shape, scale = scale)
    }
    
    notidel_default <- claim_notification(n_vector, claim_sizes, paramfun = notidel_param_default)
    
    
    if (input$Notif_Delay_selection == 'Weibull'){

      # Module 3 Option 1: Weibull
      notidel_param <- function(claim_size, occurrence_period) {
        # NOTE: users may add to, but not remove these two arguments (claim_size,
        # occurrence_period) as they are part of SynthETIC's internal structure

        # specify the target mean and target coefficient of variation
        target_mean <- min(3, max(1, 2-(log(claim_size/(0.50 * ref_claim)))/3))/4 / time_unit
        # target_mean <- 10e3
        target_cv <- as.numeric(input$size_weibull_cv)
        # convert to Weibull parameters
        shape <- get_Weibull_parameters(target_mean, target_cv)[1]
        scale <- get_Weibull_parameters(target_mean, target_cv)[2]

        c(shape = shape, scale = scale)
      }

      notidel <- claim_notification(n_vector, claim_sizes, paramfun = notidel_param)

      # print(glimpse(notidel))
      
    } else if (input$Notif_Delay_selection == 'Transformed Gamma'){

      trgamma_param <- function(claim_size, occurrence_period, rate) {
        c(shape1 = max(1, claim_size / ref_claim),
          shape2 = 1 - occurrence_period / 200,
          rate = rate)
      }

      # simulate notification delays from the transformed gamma
      notidel <- claim_notification(n_vector, claim_sizes, rfun = actuar::rtrgamma, paramfun = trgamma_param, rate = 2)

      # print(glimpse(notidel))
      
    } else if (input$Notif_Delay_selection == 'Mixed distribution') {

      # Part 3: Mixed distribution
      # equal probability of sampling from x (Weibull) or y (transformed gamma)
      rmixed_notidel <- function(n, claim_size) {
        x_selected <- sample(c(T, F),
                             prob=c(as.numeric(input$not_delay_3_mix_prob), 1-as.numeric(input$not_delay_3_mix_prob)),
                             size = n,
                             replace = TRUE)
        x <- rweibull(n, shape = 2, scale = 1)
        y <- actuar::rtrgamma(n, shape1 = min(1, claim_size / ref_claim), shape2 = 0.8, rate = 2)
        result <- length(n)
        result[x_selected] <- x[x_selected]; result[!x_selected] <- y[!x_selected]
        return(result)
      }
      notidel <- claim_notification(n_vector, claim_sizes, rfun = rmixed_notidel)

      # print(glimpse(notidel))
      
    }
    
    output$plot_notidel <- renderPlot({
      plot(ecdf(unlist(notidel_default)), # xlim = c(0, 15),
           main = "Module 3: Empirical distribution of simulated notification delays",
           xlab = "Notification delay (in quarters)")
      plot(ecdf(unlist(notidel)), add = TRUE, col = 2)
      legend.text <- c("Weibull (default)", input$Notif_Delay_selection)
      legend("bottomright", legend.text, col = 1:2, lty = 1, bty = "n")
    })
      
    # Module 4: Closer Delay
    setldel_param <- function(claim_size, occurrence_period) {
      # NOTE: users may add to, but not remove these two arguments (claim_size, 
      # occurrence_period) as they are part of SynthETIC's internal structure
      
      # specify the target Weibull mean
      # if (claim_size < (0.10 * ref_claim) & occurrence_period >= 21) {
      #   a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      # } else {
      #   a <- max(0.85, 1 - 0.0075 * occurrence_period)
      # }
      # mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * ref_claim))))
      target_mean <- 30
      
      # specify the target Weibull coefficient of variation
      target_cv <- 0.60
      
      c(shape = get_Weibull_parameters(target_mean, target_cv)[1, ],
        scale = get_Weibull_parameters(target_mean, target_cv)[2, ])
    }
    
    setldel <- claim_closure(n_vector, claim_sizes, paramfun = setldel_param)

    # FLAG: Check Axis bounds 
    output$plot_setldel <- renderPlot({
      plot(ecdf(unlist(setldel)), # xlim = c(0, 15),
           main = "Module 4: Empirical distribution of simulated settlement delays",
           xlab = "Settlement delay (in quarters)")
    })
    
    # Module 5: Partial payment number
    rmixed_payment_no <- function(n, claim_size, claim_size_benchmark_1, claim_size_benchmark_2) {
      # construct the range indicators
      test_1 <- (claim_size_benchmark_1 < claim_size & claim_size <= claim_size_benchmark_2)
      test_2 <- (claim_size > claim_size_benchmark_2)
      
      # if claim_size <= claim_size_benchmark_1
      no_pmt <- sample(c(1, 2), size = n, replace = T, prob = c(1/2, 1/2))
      # if claim_size is between the two benchmark values
      no_pmt[test_1] <- sample(c(2, 3), size = sum(test_1), replace = T, prob = c(1/3, 2/3))
      # if claim_size > claim_size_benchmark_2
      no_pmt_mean <- pmin(8, 4 + log(claim_size/claim_size_benchmark_2))
      prob <- 1 / (no_pmt_mean - 3)
      no_pmt[test_2] <- stats::rgeom(n = sum(test_2), prob = prob[test_2]) + 4
      
      no_pmt
    }
    
    no_payments <- claim_payment_no(n_vector, claim_sizes, rfun = rmixed_payment_no,
                                    claim_size_benchmark_1 = 0.0375 * ref_claim,
                                    claim_size_benchmark_2 = 0.075 * ref_claim)
    
    # print(glimpse(unlist(no_payments)))
    
    # FLAG: Distribution looks different  
    output$hist_no_payments <- renderPlot({
      hist(unlist(no_payments), 
           main = "Module 5: Histogram of number of partial payments",
           xlab = "Number of partial payments")
    })
    
    # Module 6: Partial payment number
    
    rmixed_payment_size <- function(n, claim_size) {
      # n = number of simulations, here n should be the number of partial payments
      if (n >= 4) {
        # 1) Simulate the "complement" of the proportion of total claim size 
        #    represented by the last two payments
        p_mean <- 1 - min(0.95, 0.75 + 0.04*log(claim_size/(0.10 * ref_claim)))
        p_CV <- 0.20
        p_parameters <- get_Beta_parameters(target_mean = p_mean, target_cv = p_CV)
        last_two_pmts_complement <- stats::rbeta(
          1, shape1 = p_parameters[1], shape2 = p_parameters[2])
        last_two_pmts <- 1 - last_two_pmts_complement
        
        # 2) Simulate the proportion of last_two_pmts paid in the second last payment
        q_mean <- 0.9
        q_CV <- 0.03
        q_parameters <- get_Beta_parameters(target_mean = q_mean, target_cv = q_CV)
        q <- stats::rbeta(1, shape1 = q_parameters[1], shape2 = q_parameters[2])
        
        # 3) Calculate the respective proportions of claim amount paid in the 
        #    last 2 payments
        p_second_last <- q * last_two_pmts
        p_last <- (1-q) * last_two_pmts
        
        # 4) Simulate the "unnormalised" proportions of claim amount paid 
        #    in the first (m - 2) payments
        p_unnorm_mean <- last_two_pmts_complement/(n - 2)
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(
          target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(
          n - 2, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])
        
        # 5) Normalise the proportions simulated in step 4
        amt <- last_two_pmts_complement * (amt/sum(amt))
        # 6) Attach the last 2 proportions, p_second_last and p_last
        amt <- append(amt, c(p_second_last, p_last))
        # 7) Multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt
        
      } else if (n == 2 | n == 3) {
        p_unnorm_mean <- 1/n
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(
          target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(
          n, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])
        # Normalise the proportions and multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt/sum(amt)
        
      } else {
        # when there is a single payment
        amt <- claim_size
      }
      return(amt)
    }
    
    payment_sizes <- claim_payment_size(n_vector, claim_sizes, no_payments, rmixed_payment_size)
    print(glimpse(unlist(payment_sizes)))
    
    # FLAG > Better visual needed
    output$hist_size_payments <- renderPlot({
      hist(unlist(payment_sizes)
           ,main = "Module 6: Histogram of size of partial payments",
           xlab = "Size of partial payments"
      )
    })
    # Module 7: Payment time
    
    param_pmtdel <- function(claim_size, setldel, occurrence_period) {
      # mean settlement delay
      if (claim_size < (0.10 * ref_claim) & occurrence_period >= 21) {
        a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      } else {
        a <- max(0.85, 1 - 0.0075 * occurrence_period)
      }
      mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * ref_claim))))
      target_mean <- mean_quarter / 4 / time_unit
      
      c(claim_size = claim_size,
        setldel = setldel,
        setldel_mean = target_mean)
    }
    
    r_pmtdel <- function(n, claim_size, setldel, setldel_mean) {
      result <- c(rep(NA, n))
      
      # First simulate the unnormalised values of d, sampled from a Weibull distribution
      if (n >= 4) {
        # 1) Simulate the last payment delay
        unnorm_d_mean <- (1 / 4) / time_unit
        unnorm_d_cv <- 0.20
        parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
        result[n] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        
        # 2) Simulate all the other payment delays
        for (i in 1:(n - 1)) {
          unnorm_d_mean <- setldel_mean / n
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }
        
      } else {
        for (i in 1:n) {
          unnorm_d_mean <- setldel_mean / n
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }
      }
      
      # Normalise d such that sum(inter-partial delays) = settlement delay
      # To make sure that the pmtdels add up exactly to setldel, we treat the last one separately
      result[1:n-1] <- (setldel/sum(result)) * result[1:n-1]
      result[n] <- setldel - sum(result[1:n-1])
      
      return(result)
    }
    
    payment_delays <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel,
                                          rfun = r_pmtdel, paramfun = param_pmtdel,
                                          occurrence_period = rep(1:I, times = n_vector))
    
    payment_times <- claim_payment_time(n_vector, occurrence_times, notidel, payment_delays)
    
    payment_periods <- claim_payment_time(n_vector, occurrence_times, notidel, payment_delays,
                                          discrete = TRUE)
    # print(glimpse(cbind(payment_delays[[1]][[1]], payment_times[[1]][[1]], payment_periods[[1]][[1]])))
    
    # Module 8. Inflation    
    demo_rate <- (1 + 0.02)^(1/4) - 1
    base_inflation_past <- rep(demo_rate, times = 40)
    base_inflation_future <- rep(demo_rate, times = 40)
    base_inflation_vector <- c(base_inflation_past, base_inflation_future)
    
    # Superimposed inflation:
    # 1) With respect to occurrence "time" (continuous scale)
    SI_occurrence <- function(occurrence_time, claim_size) {
      if (occurrence_time <= 20 / 4 / time_unit) {1}
      else {1 - 0.4*max(0, 1 - claim_size/(0.25 * ref_claim))}
    }
    # 2) With respect to payment "time" (continuous scale)
    # -> compounding by user-defined time unit
    SI_payment <- function(payment_time, claim_size) {
      period_rate <- (1 + 0.30)^(time_unit) - 1
      beta <- period_rate * max(0, 1 - claim_size/ref_claim)
      (1 + beta)^payment_time
    }
    
    payment_inflated <- claim_payment_inflation(
      n_vector, payment_sizes, payment_times, occurrence_times,
      claim_sizes, base_inflation_vector, SI_occurrence, SI_payment)

    # Part 9: Output
    all_claims <- claims(
      frequency_vector = n_vector,
      occurrence_list = occurrence_times,
      claim_size_list = claim_sizes,
      notification_list = notidel,
      settlement_list = setldel,
      no_payments_list = no_payments,
      payment_size_list = payment_sizes,
      payment_delay_list = payment_delays,
      payment_time_list = payment_times,
      payment_inflated_list = payment_inflated
    )
    
    output$plot_claims <- renderPlot({
      plot(all_claims, adjust = FALSE) +
        ggplot2::labs(subtitle = paste("With x simulations"))
    })
    
    # Module 11. Major Revisions Frequency
    # Option 0: Short script
    major <- claim_majRev_freq(all_claims)
    
    # Module 12: Major Revisions Time
    # Option 0: Short script
    major <- claim_majRev_time(claims = all_claims, majRev_list = major)
    
    # Module 13. Major Revisions Size
    # Option 0: Short script
    major <- claim_majRev_size(major)
    
    # Module 14. Minor Revisions Frequency
    # Option 0: Short script
    minor <- claim_minRev_freq(all_claims)
    
    # Module 15: Minor Revisions Time
    # Option 0: Short script
    minor <- claim_minRev_time(all_claims, minor)
    
    # 16. Minor Revisions Size
    # Option 0: Short script
    minor <- claim_minRev_size(all_claims, major, minor)
    
    # development of case estimates
    test <- claim_history(all_claims, major, minor)
    test_inflated <- claim_history(all_claims, major, minor, base_inflation_vector)
  })
})
