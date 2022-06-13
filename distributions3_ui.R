library(shiny)
library(tidyverse)
library(distributions3)

#' distributions3_ui.R
#' 
#' This is a set of reusable components to generate a distributions3 object. 

DISTRIBUTION_UNIFORM <- 'Uniform'
DISTRIBUTION_NORMAL <- 'Normal'
DISTRIBUTION_GAMMA <- 'Gamma'
DISTRIBUTION_EXPONENTIAL <- 'Exponential'

SUPPORTED_DISTRIBUTIONS <- c(
  DISTRIBUTION_UNIFORM,
  DISTRIBUTION_NORMAL, 
  DISTRIBUTION_GAMMA,
  DISTRIBUTION_EXPONENTIAL
)

# options(shiny.reactlog = TRUE) 

distributions3_ui <- function(id){
  ns_this = NS(id)
  tagList(
    selectInput(NS(id, "distribution"), "Pick the distribution", choices = SUPPORTED_DISTRIBUTIONS),
    conditionalPanel(
      condition = paste0("input.distribution !='", DISTRIBUTION_UNIFORM, "'"),
      ns = ns_this,
      numericInput(NS(id, "expected_value"), "Expected value", value = 10, min = 0),
      conditionalPanel(
        condition = paste0("input.distribution !='", DISTRIBUTION_EXPONENTIAL, "'"),
        ns = ns_this,
        numericInput(NS(id, "cv"), "Coefficient of variation", value = 0.1, min = 0, step = .05)
      )
    ),
    conditionalPanel(
      condition = paste0("input.distribution =='", DISTRIBUTION_UNIFORM, "'"),
      ns = ns_this,
      numericInput(NS(id, "lower_bound"), "Lower bound", value = 0, min = 0),
      numericInput(NS(id, "upper_bound"), "Upper bound", value = 10, min = 0),
    )
  )
}

distributions3_server <- function(id){
  moduleServer(id, function(input, output, session){
    reactive(
      if (input$distribution == DISTRIBUTION_UNIFORM) {
        
        distributions3::Uniform(input$lower_bound, input$upper_bound)
        
      } else if (input$distribution == DISTRIBUTION_NORMAL) {
        
        params <- params_norm(input$expected_value, input$cv)
        distributions3::Normal(params$mu, params$sigma)
        
      } else if (input$distribution == DISTRIBUTION_GAMMA) {
        
        params <- params_gamma(input$expected_value, input$cv)
        distributions3::Gamma(params$shape, params$rate)
        
      } else if (input$distribution == DISTRIBUTION_EXPONENTIAL) {
        
        params <- params_exponential(input$expected_value)
        distributions3::Exponential(params$rate)
        
      }
    )
  })
}

params_norm <- function(expected_value, cv){
  list(
    mu = expected_value
    , sigma = cv * expected_value
  )
}

params_gamma <- function(expected_value, cv){
  alpha = 1 / cv^2
  beta <- alpha / expected_value
  list(
    shape = alpha
    , rate = beta
  )
}

params_exponential <- function(expected_value){
  list(
    rate = 1 / expected_value
  )
}

