## TODO: model diagnostics

## TODO: plot predictions

## TODO: perform VOI for new input vector

#' Predict log-costs from fitted CaptureTB model
#'
#' Given a data.frame of new inputs and posterior samples
#' of intercept and coefficients, returns a matrix of
#' predicted log-costs for each simulation and input row.
#'
#' @param dat Data.frame. New input data.
#' @param covariates Character vector. Names of covariate columns.
#' @param countries Character vector. Names of countries in training data.
#' @param samples coda::mcmc.list. Posterior samples from fit_capturetb_model().
#'
#' @return Matrix of predicted log-costs.
#' Rows = simulations, columns = input rows.
#' @export
predict_capturetb_logcost <- function(dat, covariates, countries, samples) {
  smat <- as.matrix(samples)

  # known country intercepts
  alpha_cols <- paste0("alpha[", seq_along(countries), "]")
  alphas <- smat[, alpha_cols, drop = FALSE]

  # if country not known, or not in training data
  # generate intercept using hyper-parameters
  mu <- smat[, "mu_alpha"] # hyper-means
  sig <- smat[, "sigma_alpha"] # hyper-sds

  alpha_new <- rnorm(length(mu), mu, sig)
  alphas <- cbind(alphas, alpha_new)

  beta_cols <- paste0("beta[", seq_along(covariates), "]")
  betas <- smat[, beta_cols, drop = FALSE]

  x <- as.matrix(dat[, covariates, drop = FALSE])
  x_country <- dat[, "fc_country", drop = FALSE]
  x_country_matrix <- as.data.frame(lapply(
    countries,
    function(x) x == x_country
  ))
  x_country_matrix[, ] <- lapply(x_country_matrix[, , drop = FALSE], as.numeric)
  x_country_matrix[, length(countries) + 1] <- 0
  x_country_matrix[
    which(rowSums(x_country_matrix) == 0),
    length(countries) + 1
  ] <- 1

  preds <- alphas %*% t(x_country_matrix) + betas %*% t(x)
  preds
}

#' Fit the CaptureTB Model using rjags
#'
#' Fits the JAGS model defined in \code{inst/jags/model.model}
#' using the provided data and priors.
#'
#' @param dat Data.frame. Data to fit the model to.
#' @param covariates Character. Vector of covariate names to
#' include in the model.
#' @param target Character. Name of the target variable to model.
#' @param priors List of prior parameters, from \code{capturetb_priors()}.
#' @param n.chains Integer. Number of MCMC chains. Default is 3.
#' @param n.iter Integer. Number of total iterations per chain.
#' Default is 100000.
#' @param n.burnin Integer. Number of burn-in iterations. Default is 2000.
#' @param n.adapt Integer. Number of step adaption iterations. Default is 2000.
#' @param n.thin Integer. Thinning interval. Default is 10.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#' @param ... Additional arguments passed to \code{rjags::jags.model()}.
#'
#' @return A \code{coda::mcmc.list} object with posterior samples.
#' @export
#' @importFrom rlang .data
fit_capturetb_model <- function(
    dat = get_data("OP treatment visit"),
    covariates = capturetb_covariates(),
    target = "USD_unitcost_total",
    priors = capturetb_priors(),
    n.chains = 3,
    n.iter = 100000,
    n.burnin = 2000,
    n.adapt = 2000,
    n.thin = 10,
    seed = 123,
    ...) {
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("Package 'rjags' is required but not installed.")
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required but not installed.")
  }

  # 	if there are multiple facilities, take one at random
  dat <- dat |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(covariates),
        ~ !is.na(.) & !is.nan(.) & is.finite(.)
      )
    )

  set.seed(seed)
  model_file <- system.file("jags", "model.model", package = "capturetb")

  x <- dat[, covariates]
  logical_cols <- sapply(x, is.logical)
  if (any(logical_cols)) {
    x[, logical_cols] <- lapply(x[, logical_cols, drop = FALSE], as.numeric)
  }

  jags_data <- list(
    N = nrow(dat),
    K = length(covariates),
    x = x,
    log_cost = log(dat[[target]]),
    NC = length(unique(dat$fc_country)),
    country = as.numeric(as.factor(dat$fc_country))
  )

  jags_data <- c(jags_data, as.list(priors))
  jags_mod <- rjags::jags.model(model_file,
    data = jags_data,
    n.chains = n.chains,
    n.adapt = n.adapt,
    quiet = TRUE,
    ...
  )

  update(jags_mod, n.iter = n.burnin)
  samples <- rjags::coda.samples(jags_mod,
    variable.names = c(
      "mu_alpha",
      "sigma",
      "sigma_alpha",
      "beta",
      "alpha"
    ),
    n.iter = n.iter,
    thin = n.thin
  )
  samples
}

#' Create Prior Distributions for the CaptureTB Model
#'
#' Constructs a list of prior distributions for model parameters.
#' This function validates the input arguments and returns a list
#' with class \code{"capturetbpriors"}.
#'
#' @param mu_alpha.mean Numeric scalar. Mean of the prior for \eqn{\mu_\alpha}.
#' Default is 0.
#' @param mu_alpha.precision Numeric scalar. Precision (inverse variance) of
#' the prior for \eqn{\mu_\alpha}. Default is 0.01.
#' @param sigma.rate Numeric scalar. Rate parameter for prior on  \eqn{\sigma}.
#' Default is 10.
#' @param sigma_alpha.rate Numeric scalar. Rate parameter for prior
#' on \eqn{\sigma_\alpha}. Default is 10.
#' @param beta.mean Numeric vector. Means of the priors for the \eqn{\beta}
#' coefficients. Default is \code{c(0, -0.0142, -0.0412, 0.348, 0.352, -0.29)}.
#' @param beta.precision Numeric vector. Precision of the priors for the
#' \eqn{\beta} coefficients.
#' Default is \code{c(0.01, 22.7, 13.9, 8.08, 5.5, 23.45)}.
#'
#' @return A list of prior parameters with class \code{"capturetbpriors"}.
#'
#' @examples
#' priors <- capturetb_priors()
#'
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
    "beta.precision must not be NaN" = !any(is.nan(beta.precision))
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

#' Plot Prior Distribution from a capturetbpriors Object
#'
#' Plots the distribution of a specified prior from a 'capturetbpriors' object.
#'
#' @param x Object of class 'capturetbpriors'. See \code{capturetb_priors}.
#' @param parameter Character. Name of the parameter prior to plot.
#' ("mu_alpha_mean", "sigma", "sigma_alpha", "beta\[1\]", ...).
#' Default is "mu_alpha_mean".
#' @export
#' @method plot capturetbpriors
#' @param \dots Further arguments passed to the method.
#' @importFrom utils read.csv
#' @importFrom stats dexp dnorm qexp rnorm update
#' @importFrom ggplot2 .data
plot.capturetbpriors <- function(x, ..., parameter = "mu_alpha_mean") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

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

  # Helper to plot exponential
  plot_exp <- function(rate, label) {
    vals <- seq(0, qexp(0.995, rate), length.out = 200)
    dens <- dexp(vals, rate)
    df <- data.frame(x = vals, y = dens)
    ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste("Exponential prior for", label),
        x = label, y = "Density"
      )
  }

  # Map user-friendly names to internal list names
  prior_map <- list(
    mu_alpha_mean = "prior.mu_alpha.mean",
    sigma = "prior.sigma.rate",
    sigma_alpha = "prior.sigma_alpha.rate"
  )

  # Handle beta priors
  if (grepl("^beta\\[\\d+\\]$", parameter)) {
    idx <- as.integer(sub("beta\\[(\\d+)\\]", "\\1", parameter))
    if (idx < 1 || idx > length(x$prior.beta.mean)) {
      stop("beta index out of range")
    }
    mean <- x$prior.beta.mean[idx]
    sd <- 1 / sqrt(x$prior.beta.precision[idx])
    return(plot_normal(mean, sd, paste0("beta[", idx, "]")))
  }

  # Handle mapped priors
  if (parameter %in% names(prior_map)) {
    if (parameter == "mu_alpha_mean") {
      mean <- x$prior.mu_alpha.mean
      sd <- 1 / sqrt(x$prior.mu_alpha.precision)
      return(plot_normal(mean, sd, "mu_alpha"))
    } else if (parameter == "sigma") {
      rate <- x$prior.sigma.rate
      return(plot_exp(rate, "sigma"))
    } else if (prior == "sigma_alpha") {
      rate <- x$prior.sigma_alpha.rate
      return(plot_exp(rate, "sigma_alpha"))
    }
  }

  stop("Unknown prior name. Use 'mu_alpha_mean', 'sigma',
  'sigma_alpha', or 'beta[1]', etc.")
}

#' Get List of Covariate Names for CaptureTB Model
#'
#' Returns a character vector of covariate names used in the CaptureTB model.
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
#' @export
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
