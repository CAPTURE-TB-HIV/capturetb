#' List unique output types in raw data
#'
#' Returns all unique output types found in the ValueTB data.
#'
#' @return A character vector of unique output types.
#' @examples
#' outputs()
#' @export
outputs <- function() {
  td_econ <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  unique(td_econ$output)
}

#' Get raw data filtered by output type
#'
#' Returns the raw ValueTB data filtered to a specified output type.
#'
#' @param output_name Optional character string specifying an output type to
#' filter by. Defaults to NULL (all data).
#'
#' @return A data frame containing only rows matching the specified output type.
#' @examples
#' head(get_data("IP bedday"))
#' @export
#' @importFrom rlang .data
get_data <- function(output_name = NULL) {
  stopifnot(
    "'output_name' must be a string" =
      (is.null(output_name) || is.character(output_name))
  )
  dat <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  if (!is.null(output_name)) {
    dat <- dat |>
      dplyr::filter(.data$output == output_name)
  }

  return(dat)
}


#' Create prior distributions for a model.
#'
#' Constructs a list of prior distributions for model parameters.
#' This function validates the input arguments and returns a list
#' with class `capturetbpriors`. Note that if these priors are
#' used in a [`RandomSlopes`] model, `beta.mean` and
#' `beta.precision` define priors on the `mu_beta` hyper-parameters
#' which in turn define priors for the country-specific coefficients.
#' See `vignette("03_model-comparisons", package = "capturetb")` for details.
#'
#' @param mu_alpha.mean Numeric scalar. Mean of the prior for `mu_alpha`
#' in a [`MixedEffects`] or [`RandomSlopes`] model or `alpha` if the model
#' is an instance of [`FixedEffects`]. Default is 0.
#' @param mu_alpha.precision Numeric scalar. Precision (inverse variance) of
#' the prior for `mu_alpha` in a [`MixedEffects`] or [`RandomSlopes`] model
#' or `alpha` if the model is an instance of [`FixedEffects`]. Default is 0.01.
#' @param sigma.rate Numeric scalar. Rate parameter for prior on `sigma`.
#' Default is 10.
#' @param sigma_alpha.rate Numeric scalar. Rate parameter for prior
#' on `sigma_alpha`if model is an instance of [`MixedEffects`] or
#' [`RandomSlopes`]. Ignored if the model is an instance of [`FixedEffects`].
#' Default is 10.
#' @param beta.mean Numeric vector. Means of the priors for the `beta`
#' coefficients in a [`FixedEffects`] or [`MixedEffects`] model,
#' or the `mu_beta` hyper-parameters in a [`RandomSlopes`] model.
#' Default is `c(0, -0.0142, -0.0412, 0.348, 0.352, -0.29)`.
#' @param beta.precision Numeric vector. Precision of the priors for the
#' `beta` coefficients in a [`FixedEffects`] or [`MixedEffects`] model,
#' or the `mu_beta` hyper-parameters in a [`RandomSlopes`] model.
#' Default is `c(0.01, 22.7, 13.9, 8.08, 5.5, 23.45)`.
#'
#' @return A list of prior parameters with class `capturetbpriors`.
#' @export
capturetb_priors <- function(mu_alpha.mean = 0,
                             mu_alpha.precision = 0.01,
                             sigma.rate = 1,
                             sigma_alpha.rate = 1,
                             beta.mean = c(
                               0, -0.0142, -0.0412,
                               0.348, 0.352, -0.29
                             ),
                             beta.precision = c(
                               0.01, 22.7, 13.9,
                               8.08, 5.5, 23.45
                             )) {
  stopifnot(
    "mu_alpha.mean must be numeric" = is.numeric(mu_alpha.mean),
    "mu_alpha.mean must be scalar" = length(mu_alpha.mean) == 1,
    "mu_alpha.precision must be numeric" = is.numeric(mu_alpha.precision),
    "mu_alpha.precision must be scalar" = length(mu_alpha.precision) == 1,
    "sigma.rate must be numeric" = is.numeric(sigma.rate),
    "sigma.rate must be scalar" = length(sigma.rate) == 1,
    "sigma_alpha.rate must be numeric" = is.numeric(sigma_alpha.rate),
    "sigma_alpha.rate must be scalar" = length(sigma_alpha.rate) == 1,
    "beta.mean must be numeric" = is.numeric(beta.mean),
    "beta.precision must be numeric" = is.numeric(beta.precision),
    "mu_alpha.mean must not be NaN" = !is.nan(mu_alpha.mean),
    "mu_alpha.precision must not be NaN" = !is.nan(mu_alpha.precision),
    "sigma.rate must not be NaN" = !is.nan(sigma.rate),
    "sigma_alpha.rate must not be NaN" = !is.nan(sigma_alpha.rate),
    "beta.mean must not be NaN" = !any(is.nan(beta.mean)),
    "beta.precision must not be NaN" = !any(is.nan(beta.precision)),
    "beta.mean and beta.precision must be the same length" =
      length(beta.precision) == length(beta.mean)
  )
  priors <- list(
    prior.mu_alpha.mean = mu_alpha.mean,
    prior.mu_alpha.precision = mu_alpha.precision,
    prior.sigma.rate = sigma.rate,
    prior.sigma_alpha.rate = sigma_alpha.rate,
    prior.beta.mean = beta.mean,
    prior.beta.precision = beta.precision
  )

  class(priors) <- append("capturetbpriors", class(priors))
  priors
}

#' Get list of covariates used in the final CaptureTB model.
#'
#' Returns a character vector of covariate names.
#'
#' @return A character vector containing the names of covariates:
#'   \itemize{
#'     \item \code{log_USD_p_bldgspace}: Log of USD per building space
#'     \item \code{logVisits}: Log of visits
#'     \item \code{logVisitsPP}: Log of visits per person
#'     \item \code{secondary}: Indicator for secondary variable
#'     \item \code{urban}: Indicator for urban location
#'     \item \code{public}: Indicator for public status
#'   }
#' @keywords internal
capturetb_covariates <- function() {
  c(
    "log_USD_p_bldgspace",
    "logVisits",
    "logVisitsPP",
    "secondary",
    "urban",
    "public"
  )
}
