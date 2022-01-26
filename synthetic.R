library(SynthETIC)

tab_synthetic <- tabPanel(
  'SynthETIC',
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "synth_num_policy_years",
        "Choose number of policy years:",
        min = 2,
        max = 20,
        step = 1,
        value = 5
      ),
    ),
  mainPanel(
      h2("This is a plot"),
      plotOutput("plt_synth", width = "60%"),
    )
    
  )
)

expr_synthetic <- quote({
  
  observe({
    set_parameters(ref_claim = 1, time_unit = 1)
    ref_claim <- return_parameters()[1]
    time_unit <- return_parameters()[2]
    
    years <- 10
    I <- years / time_unit
    E <- c(rep(12e3, I)) # effective annual exposure rates
    lambda <- c(rep(0.03, I))
    
    times <- 10
    
    n_vector <- claim_frequency(I, E = E * times, lambda)
    occurrence_times <- claim_occurrence(n_vector)
    claim_sizes <- claim_size(n_vector)
    
    notidel_param <- function(claim_size, occurrence_period) {
      # NOTE: users may add to, but not remove these two arguments (claim_size, 
      # occurrence_period) as they are part of SynthETIC's internal structure
      
      # specify the target mean and target coefficient of variation
      # target_mean <- min(3, max(1, 2-(log(claim_size/(0.50 * ref_claim)))/3))/4 / time_unit
      target_mean <- 10e3
      target_cv <- 0.70
      # convert to Weibull parameters
      shape <- get_Weibull_parameters(target_mean, target_cv)[1]
      scale <- get_Weibull_parameters(target_mean, target_cv)[2]
      
      c(shape = shape, scale = scale)
    }
    
    notidel <- claim_notification(n_vector, claim_sizes, paramfun = notidel_param)
    
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
  })
  
  output$plt_synth <- renderPlot({
    plot(all_claims, adjust = FALSE) +
      ggplot2::labs(subtitle = paste("With", times, "simulations"))
    
  })
  
})
