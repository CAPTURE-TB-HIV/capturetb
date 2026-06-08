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
#' @param cost_type One of "ECON" or "FIN". If "ECON", model for
#' economic costs is returned. If "FIN", model for financial
#' costs is returned. Default "ECON".
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
get_data <- function(
    cost_type = "ECON",
    output_name = NULL,
    output_group = NULL) {
  stopifnot(
    "'output_group' must be a string" =
      (is.null(output_group) || is.character(output_group))
  )
  stopifnot(
    "'output_name' must be a string" =
      (is.null(output_name) || is.character(output_name))
  )
  stopifnot(
    "cost_type must be one of 'ECON' or 'FIN'" =
      cost_type %in% c("ECON", "FIN")
  )
  if (cost_type == "ECON") {
    dat <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  } else {
    dat <- utils::read.csv(system.file("td_fin.csv", package = "capturetb"))
  }

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
#' with class `capturetbpriors`.
#' See `vignette("03_model-fitting", package = "capturetb")` for details
#' of the model structure.
#'
#' @param alpha.mean Numeric scalar. Mean of the prior for the intercept `alpha`.
#' Default is 0.
#' @param alpha.precision Numeric scalar. Precision (inverse variance) of
#' the prior for the intercept `alpha``. Default is 0.01.
#' @param sigma.scale Numeric scalar. Scale parameter for Half-Cauchy prior
#' on `sigma`. Default is 1.
#' @param sigma_c.scale Numeric scalar. Scale parameter for Half-Cauchy
#' prior on `sigma_c`. Default is 1.
#' @param sigma_f.scale Numeric scalar. Scale parameter for Half-Student-t with
#' 3 degrees of freedom prior on `sigma_f`. Default is 1.
#' @param sigma_v.scale Numeric scalar. Scale parameter for Half-Cauchy
#' prior on `sigma_v`. Default is 1.
#' @param beta.mean Numeric vector. Means of the priors for the `beta`
#' coefficients of fixed effects.
#' @param beta.precision Numeric vector. Precision of the priors for the
#' `beta` coefficients of fixed effects.
#'
#' @return A list of prior parameters with class `capturetbpriors`.
#' @export
capturetb_priors <- function(alpha.mean = 0,
                             alpha.precision = 0.01,
                             sigma.scale = 10,
                             sigma_c.scale = 10,
                             sigma_f.scale = 10,
                             sigma_v.scale = 10,
                             beta.mean = 0,
                             beta.precision = 0.01) {
  stopifnot(
    "alpha.mean must be numeric" = is.numeric(alpha.mean),
    "alpha.mean must be scalar" = length(alpha.mean) == 1,
    "alpha.precision must be numeric" = is.numeric(alpha.precision),
    "alpha.precision must be scalar" = length(alpha.precision) == 1,
    "sigma.scale must be numeric" = is.numeric(sigma.scale),
    "sigma.scale must be scalar" = length(sigma.scale) == 1,
    "sigma_c.scale must be numeric" = is.numeric(sigma_c.scale),
    "sigma_c.scale must be scalar" = length(sigma_c.scale) == 1,
    "sigma_f.scale must be numeric" = is.numeric(sigma_f.scale),
    "sigma_f.scale must be scalar" = length(sigma_f.scale) == 1,
    "sigma_v.scale must be numeric" = is.numeric(sigma_v.scale),
    "sigma_v.scale must be scalar" = length(sigma_v.scale) == 1,
    "beta.mean must be numeric" = is.numeric(beta.mean),
    "beta.precision must be numeric" = is.numeric(beta.precision),
    "alpha.mean must not be NaN" = !is.nan(alpha.mean),
    "alpha.precision must not be NaN" = !is.nan(alpha.precision),
    "sigma.scale must not be NaN" = !is.nan(sigma.scale),
    "sigma_c.scale must not be NaN" = !is.nan(sigma_c.scale),
    "sigma_f.scale must not be NaN" = !is.nan(sigma_f.scale),
    "sigma_v.scale must not be NaN" = !is.nan(sigma_v.scale),
    "beta.mean must not be NaN" = !any(is.nan(beta.mean)),
    "beta.precision must not be NaN" = !any(is.nan(beta.precision)),
    "beta.mean and beta.precision must be the same length" =
      length(beta.precision) == length(beta.mean)
  )
  priors <- list(
    prior.alpha.mean = alpha.mean,
    prior.alpha.precision = alpha.precision,
    prior.sigma.scale = sigma.scale,
    prior.sigma_c.scale = sigma_c.scale,
    prior.sigma_f.scale = sigma_f.scale,
    prior.sigma_v.scale = sigma_v.scale,
    prior.beta.mean = beta.mean,
    prior.beta.precision = beta.precision
  )

  class(priors) <- append("capturetbpriors", class(priors))
  priors
}
