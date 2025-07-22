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

#' List unique output type groups in raw data
#'
#' Returns all unique output type groups found in the ValueTB data.
#'
#' @return A character vector of unique output groups
#' @examples
#' output_groups()
#' @export
output_groups <- function() {
  td_econ <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  unique(td_econ$outputgroup)
}

#' Get raw data filtered by output type
#'
#' Returns the raw ValueTB data filtered to a specified output type.
#'
#' @param output_name Optional character string specifying an output type to
#' filter by. Defaults to NULL (all data). See [outputs()] for valid options.
#' 
#' @param output_group Optional character string specifying an output group to
#' filter by. Defaults to NULL (all data). See [output_groups()] for valid options.
#'
#' @return A data frame containing only rows matching the specified output type.
#' @examples
#' head(get_data(output_group = "IP"))
#' @export
#' @importFrom rlang .data
#' @seealso outputs, output_groups
get_data <- function(output_name = NULL, output_group = NULL) {
  stopifnot(
    "'output_group' must be a string" =
      (is.null(output_group) || is.character(output_group))
  )
	stopifnot(
    "'output_name' must be a string" =
      (is.null(output_name) || is.character(output_name))
  )
  dat <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  if (!is.null(output_group)) {
    dat <- dat |>
      dplyr::filter(.data$outputgroup == output_group)
  }
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
#' @param alpha.mean Numeric scalar. Mean of the prior for the intercept `alpha`.
#' Default is 0.
#' @param alpha.precision Numeric scalar. Precision (inverse variance) of
#' the prior for the intercept `alpha``. Default is 0.01.
#' @param sigma.scale Numeric scalar. Scale parameter for Half-Cauchy prior
#' on `sigma`. Default is 1.
#' @param sigma_country.scale Numeric scalar. Scale parameter for Half-Cauchy
#' prior on `sigma_country`. Default is 1.
#' @param sigma_fc.scale Numeric scalar. Scale parameter for Half-Cauchy
#' prior on `sigma_fc`. Default is 1.
#' @param sigma_output.scale Numeric scalar. Scale parameter for Half-Cauchy
#' prior on `sigma_output`. Default is 1.
#' @param beta.mean Numeric vector. Means of the priors for the `beta`
#' coefficients in [`MixedEffects`] model,
#' or the `mu_beta` hyper-parameters in a [`RandomSlopes`] model.
#' @param beta.precision Numeric vector. Precision of the priors for the
#' `beta` coefficients in a [`MixedEffects`] model, or the `mu_beta` 
#' hyper-parameters in a [`RandomSlopes`] model.
#'
#' @return A list of prior parameters with class `capturetbpriors`.
#' @export
capturetb_priors <- function(alpha.mean = 0,
                             alpha.precision = 0.01,
                             sigma.scale = 10,
                             sigma_country.scale = 10,
                             sigma_fc.scale = 10,
                             sigma_output.scale = 10,
                             beta.mean = 0,
                             beta.precision = 0.01) {
  stopifnot(
    "alpha.mean must be numeric" = is.numeric(alpha.mean),
    "alpha.mean must be scalar" = length(alpha.mean) == 1,
    "alpha.precision must be numeric" = is.numeric(alpha.precision),
    "alpha.precision must be scalar" = length(alpha.precision) == 1,
    "sigma.scale must be numeric" = is.numeric(sigma.scale),
    "sigma.scale must be scalar" = length(sigma.scale) == 1,
    "sigma_country.scale must be numeric" = is.numeric(sigma_country.scale),
    "sigma_country.scale must be scalar" = length(sigma_country.scale) == 1,
    "sigma_fc.scale must be numeric" = is.numeric(sigma_fc.scale),
    "sigma_fc.scale must be scalar" = length(sigma_fc.scale) == 1,
    "sigma_output.scale must be numeric" = is.numeric(sigma_output.scale),
    "sigma_output.scale must be scalar" = length(sigma_output.scale) == 1,
    "beta.mean must be numeric" = is.numeric(beta.mean),
    "beta.precision must be numeric" = is.numeric(beta.precision),
    "alpha.mean must not be NaN" = !is.nan(alpha.mean),
    "alpha.precision must not be NaN" = !is.nan(alpha.precision),
    "sigma.scale must not be NaN" = !is.nan(sigma.scale),
    "sigma_country.scale must not be NaN" = !is.nan(sigma_country.scale),
    "sigma_fc.scale must not be NaN" = !is.nan(sigma_fc.scale),
    "sigma_output.scale must not be NaN" = !is.nan(sigma_output.scale),
    "beta.mean must not be NaN" = !any(is.nan(beta.mean)),
    "beta.precision must not be NaN" = !any(is.nan(beta.precision)),
    "beta.mean and beta.precision must be the same length" =
      length(beta.precision) == length(beta.mean)
  )
  priors <- list(
    prior.alpha.mean = alpha.mean,
    prior.alpha.precision = alpha.precision,
    prior.sigma.scale = sigma.scale,
    prior.sigma_country.scale = sigma_country.scale,
		prior.sigma_fc.scale = sigma_fc.scale,
    prior.sigma_output.scale = sigma_output.scale,
    prior.beta.mean = beta.mean,
    prior.beta.precision = beta.precision
  )

  class(priors) <- append("capturetbpriors", class(priors))
  priors
}
