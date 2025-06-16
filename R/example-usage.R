#' Example Usage of capturetb R6 Class
#'
#' This file demonstrates how to use the capturetb R6 class for model
#' fitting and prediction.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- capturetb$new()
#' model$fit(n.iter = 5000)
#'
#' # Make predictions on new data
#' new_data <- get_data("OP treatment visit")[1:10, ]
#' predictions <- model$predict(new_data)
#'
#' # Custom initialization
#' custom_priors <- capturetb_priors(mu_alpha.mean = 1.0)
#' model2 <- capturetb$new(priors = custom_priors)
#' model2$fit()
#'
#' # Check model status
#' model$is_fitted() # TRUE
#' samples <- model$get_samples()
#' covariates <- model$get_covariates()
#' }
#'
#' @name capturetb-examples
NULL
