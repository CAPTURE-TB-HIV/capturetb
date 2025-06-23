
#' Plot prior distribution from a `capturetbpriors` object
#'
#' Plots the distribution of a specified prior from a `capturetbpriors`` object.
#'
#' @param x Object of class 'capturetbpriors'. See [capturetb_priors()].
#' @param parameter Character. Name of the parameter to plot.
#' ("mu_alpha_mean", "sigma", "sigma_alpha", "beta\[1\]", ...).
#' Default is "mu_alpha_mean".
#' @export
#' @method plot capturetbpriors
#' @param \dots Further arguments passed to the method.
#' @importFrom utils read.csv
#' @importFrom stats dexp dnorm qexp rnorm update
#' @importFrom ggplot2 .data
#' @examples
#' mod <- unitcost()
#' plot(mod$priors())
plot.capturetbpriors <- function(x, ..., parameter = "mu_alpha") {
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

  # Handle other priors
  if (parameter %in% c("mu_alpha", "sigma_alpha", "sigma")) {
    if (parameter == "mu_alpha") {
      mean <- x$prior.mu_alpha.mean
      sd <- 1 / sqrt(x$prior.mu_alpha.precision)
      return(plot_normal(mean, sd, "mu_alpha"))
    } else if (parameter == "sigma") {
      rate <- x$prior.sigma.rate
      return(plot_exp(rate, "sigma"))
    } else if (parameter == "sigma_alpha") {
      rate <- x$prior.sigma_alpha.rate
      return(plot_exp(rate, "sigma_alpha"))
    }
  }

  stop("Unknown parameter name. Use 'mu_alpha', 'sigma',
  'sigma_alpha', or 'beta[1]', etc.")
}
