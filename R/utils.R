#' Plot prior distribution from a `capturetbpriors` object
#'
#' Plots the distribution of a specified prior from a `capturetbpriors`` object.
#'
#' @param x Object of class 'capturetbpriors'. See [capturetb_priors()].
#' @param par Character. Name of the parameter to plot.
#' ("alpha", "sigma", "sigma_country", "beta\[1\]", ...).
#' Default is "alpha".
#' @export
#' @method plot capturetbpriors
#' @param \dots Further arguments passed to the method.
#' @importFrom utils read.csv
#' @importFrom stats dexp dnorm qexp rnorm update dt
#' @importFrom ggplot2 .data
#' @examples
#' mod <- unitcost()
#' plot(mod$priors())
plot.capturetbpriors <- function(x, ..., par = "alpha") {
  # Helper to plot normal
  plot_normal <- function(mean, sd, label) {
    vals <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 200)
    dens <- dnorm(vals, mean, sd)
    df <- data.frame(x = vals, y = dens)
    ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste("Normal prior for", label),
        x = label, y = "Density"
      )
  }

  # half Cauchy density
  dhalft <- function(x, sigma) {
    dt(x / sigma, df = 1) / sigma
  }
  # half Cauchy plot helper
  plot_half_cauchy <- function(scale, label) {
    vals <- seq(0, 10, length.out = 1000)
    dens <- dhalft(vals, sigma = scale)
    df <- data.frame(x = vals, y = dens)

    ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste("Half-Cauchy prior for", label),
        x = label, y = "Density"
      )
  }

  # Handle beta priors
  if (grepl("^beta\\[\\d+\\]$", par)) {
    idx <- as.integer(sub("beta\\[(\\d+)\\]", "\\1", par))
    if (idx < 1 || idx > length(x$prior.beta.mean)) {
      stop("beta index out of range")
    }
    mean <- x$prior.beta.mean[idx]
    sd <- 1 / sqrt(x$prior.beta.precision[idx])
    return(plot_normal(mean, sd, paste0("beta[", idx, "]")))
  }

  # Handle other priors
  if (par %in% c(
    "alpha", "sigma_country", "sigma",
    "sigma_fc", "sigma_output"
  )) {
    if (par == "alpha") {
      mean <- x$prior.alpha.mean
      sd <- 1 / sqrt(x$prior.alpha.precision)
      return(plot_normal(mean, sd, "alpha"))
    } else if (par == "sigma") {
      scale <- x$prior.sigma.scale
      return(plot_half_cauchy(scale, "sigma"))
    } else if (par == "sigma_country") {
      scale <- x$prior.sigma_country.scale
      return(plot_half_cauchy(scale, "sigma_country"))
    } else if (par == "sigma_fc") {
      scale <- x$prior.sigma_fc.scale
      return(plot_half_cauchy(scale, "sigma_fc"))
    } else if (par == "sigma_output") {
      scale <- x$prior.sigma_output.scale
      return(plot_half_cauchy(scale, "sigma_output"))
    }
  }

  stop("Unknown parameter name. Use 'alpha', 'sigma',
  'sigma_country', 'sigma_fc', 'sigma_output', or 'beta[1]', etc.")
}

#' Prepare covariates for prediction
#'
#' Centers numeric covariates as required by the model.
#'
#' @param raw List or data frame of raw covariate values.
#' @param model A `capturetb::JAGSModel` object.
#' @export
#' @examples
#' mod <- unitcost()
#' prepare_covariates(
#'   raw = list(
#'     n_services = 5,
#'     public = 1,
#'     urban = 0,
#'     primary = 0,
#'     secondary = 1,
#'     tertiary = 0,
#'     log_ID_p_bldgspace = 1,
#'     logVisits = 6.9,
#'     logVisitsPP_TB = -1.29,
#'     fc_country = "Ethiopia",
#'     output = "op_treatmentvisit"
#'   ),
#'   model = mod
#' )
prepare_covariates <- function(raw, model) {
  stopifnot("raw must be a list or data frame" = is.list(raw))
  stopifnot(
    "model must be a capturetb::JAGSModel object" =
      inherits(model, "JAGSModel")
  )

  covariates <- model$covariates()
  df <- as.data.frame(raw)
  missing_covariates <- setdiff(covariates, names(df))
  if (length(missing_covariates) > 0) {
    stop("Missing covariates: ", paste(missing_covariates, collapse = ","))
  }
  centering_values <- model$centering_values()
  for (cov in covariates) {
    if (is.numeric(df[[cov]]) && !is.null(centering_values[[cov]])) {
      df[[cov]] <- df[[cov]] - centering_values[[cov]]
    }
  }
  df
}
