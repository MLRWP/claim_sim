FIXED_V_RANDOM <- c("Fixed", "Random")
SUPPORTED_DISTRIBUTIONS <- c('Uniform', 'Normal', 'Gamma', 'Exponential')

fixed_ui <- function(input_id_param, fixed_parms) {
  conditionalPanel(
    condition = paste0("input.rdo_", input_id_param, "_random == 'Fixed'"),
    numericInput(
      paste0("fixed_", input_id_param),
      "Enter a single value",
      min = fixed_parms$min,
      step = fixed_parms$step,
      value = fixed_parms$value
    )
  )
}

uniform_distribution_ui <- function(input_id_param, lower_bound_ui_parms, upper_bound_ui_parms) {
  conditionalPanel(
    condition = paste0("input.", input_id_param, "_distribution == 'Uniform'"),
    numericInput(
      paste0(input_id_param, "_uniform_lower_bound"),
      "Lower bound",
      min = lower_bound_ui_parms$min,
      step = lower_bound_ui_parms$step,
      value = lower_bound_ui_parms$value
    ),
    numericInput(
      paste0(input_id_param, "_uniform_upper_bound"),
      "Upper bound",
      min = upper_bound_ui_parms$min,
      step = upper_bound_ui_parms$step,
      value = upper_bound_ui_parms$value
    )
  )
  
}

normal_distribution_ui <- function(input_id_param, mu_ui_parms, cv_ui_parms) {
  conditionalPanel(
    condition = paste0("input.", input_id_param, "_distribution == 'Normal'"),
    numericInput(
      paste0(input_id_param, "_normal_mu"),
      "Mu",
      min = mu_ui_parms$min,
      step = mu_ui_parms$step,
      value = mu_ui_parms$value
    ),
    numericInput(
      paste0(input_id_param, "_normal_cv"),
      "Coefficient of variation",
      min = cv_ui_parms$min,
      step = cv_ui_parms$step,
      value = cv_ui_parms$value
    )
  )
  
}

gamma_distribution_ui <- function(input_id_param, alpha_ui_parms, beta_ui_parms) {
  conditionalPanel(
    condition = paste0("input.", input_id_param, "_distribution == 'Gamma'"),
    numericInput(
      paste0(input_id_param, "_gamma_alpha"),
      "Alpha",
      min = alpha_ui_parms$min,
      step = alpha_ui_parms$step,
      value = alpha_ui_parms$value
    ),
    numericInput(
      paste0(input_id_param, "_gamma_beta"),
      "Beta",
      min = beta_ui_parms$min,
      step = beta_ui_parms$step,
      value = beta_ui_parms$value
    )
  )
  
}

exponential_distribution_ui <- function(input_id_param, alpha_ui_parms) {
  conditionalPanel(
    condition = paste0("input.", input_id_param, "_distribution == 'Exponential'"),
    numericInput(
      paste0(input_id_param, "_exponential_alpha"),
      "Alpha",
      min = alpha_ui_parms$min,
      step = alpha_ui_parms$step,
      value = alpha_ui_parms$value
    )
  )
  
}

distribution_picker_ui <- function(input_id_param, ui_text, fixed_parms) {
  
  alpha_ui_parms <- list(min = .000001, step = 0.2, value = 1)
  list(
    radio = radioButtons(
      paste0("rdo_", input_id_param, "_random"),
      ui_text,
      choices = FIXED_V_RANDOM,
      inline = TRUE
    ),
    fixed = fixed_ui(input_id_param, fixed_parms),
    distribution = 
      conditionalPanel(
        condition = paste0("input.rdo_", input_id_param, "_random == 'Random'"),
        selectInput(
          paste0(input_id_param, "_distribution"),
          "Pick the distribution",
          choices = SUPPORTED_DISTRIBUTIONS
        ),
        uniform_distribution_ui(
          input_id_param, 
          lower_bound_ui_parms = list(min = 1, step = 50, value = 100),
          upper_bound_ui_parms = list(min = 1, step = 50, value = 10e3)
        ),
        normal_distribution_ui(
          input_id_param
          , mu_ui_parms = list(min = 1, step = 500, value = 100)
          , cv_ui_parms = list(min = 0.01, step = 0.01, value = 0.2)
        ),
        gamma_distribution_ui(input_id_param, alpha_ui_parms, alpha_ui_parms),
        exponential_distribution_ui(input_id_param, alpha_ui_parms)
      )
  )
}

tab_main <- tabPanel(
  'Parameters',
  p(
    "These elements will be filled in after the individual tabs have been completed"
  )
  # , numericInput('in_start_year', 'Start year', value = 2000, min = 1900, step = 1)
  # , numericInput('in_stop_year', 'Stop year', value = 2001, min = 1901, step = 1)
  # , numericInput('in_num_policies', 'Number of policies', value = 10e3, min = 1, step = 100)
  # , numericInput('in_claim_frequency', 'Claim frequency', value = 1, min = .0000001)
  # , actionButton('btn_simulate', 'Simulate!')

)
