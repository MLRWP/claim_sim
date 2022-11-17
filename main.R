#' main.R
 
tab_main <- tabPanel(
  'Global parameters',
  numericInput(
    "rnd_seed",
    "Random seed",
    min = 0,
    max = 100,
    step = 1,
    value = 42
  ),
  numericInput(
    "years_exposure",
    "Number of exposure years",
    min = 1,
    max = 40,
    step = 1,
    value = 5
  )
  # , numericInput('in_start_year', 'Start year', value = 2000, min = 1900, step = 1)
  # , numericInput('in_stop_year', 'Stop year', value = 2001, min = 1901, step = 1)
  # , numericInput('in_num_policies', 'Number of policies', value = 10e3, min = 1, step = 100)
  # , numericInput('in_claim_frequency', 'Claim frequency', value = 1, min = .0000001)
  # , actionButton('btn_simulate', 'Simulate!')

)
