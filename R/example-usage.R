#' Package Usage
#'
#' This file demonstrates how to use the fitted CaptureTB models
#' to predict unit costs for new inputs, as well as advanced usage
#' for re-fitting the models.
#'
#' @examples
#' \dontrun{
#' # Standard usage - this is the way 99% of users should interact
#' # with the package. It loads a pre-fitted instance of the MixedEffects
#' # class for use on new data.
#'
#' model <- capturetb_total()
#'
#' # Make predictions
#' new_data <- get_data("OP treatment visit")[1:10, ]
#' predictions <- model$predict(new_data)
#' }
#'
#' @examples
#' \dontrun{
#' # Advanced usage - this functionality allows the user to
#' # fit the model with different covariates, priors or data.
#' # Used for exploration and sensitivity analyses.
#' custom_priors <- capturetb_priors(mu_alpha.mean = 1.0)
#' model2 <- MixedEffects$new(priors = custom_priors)
#' model2$fit()
#' model2$predict(new_data)
#'
#' custom_covariates <- c("primary", "healthcentre")
#' custom_priors2 <- capturetb_priors(beta.mean = c(0, 0),
#'  beta.precision = c(0.01, 0.01)) # coefficient priors for each covariate
#' model3 <- MixedEffects$new(priors = custom_priors2,
#'  covariates = custom_covariates)
#' model3$fit()
#' model3$predict(new_data)
#' }
#'
#' @name capturetb-examples
NULL
