#' MixedEffects R6 Class
#'
#' An R6 class for fitting and predicting costs using a mixed effects model.
#'
#' @description
#' This class encapsulates the CaptureTB mixed effects model with
#' country-specific intercepts and fixed covariate effects, providing
#' methods to fit the JAGS model and generate predictions.
#'
#' @examples
#' \dontrun{
#' # Create a new MixedEffects model instance with default covariates and priors
#' model <- capturetb::MixedEffects$new()
#'
#' # Fit the model
#' model$fit(n.iter = 5000)
#'
#' # Make predictions
#' predictions <- model$predict(new_data)
#' }
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang .data
MixedEffects <- R6::R6Class("MixedEffects",
  public = list(
    #' @description
    #' Initialize a new CaptureTB model instance.
    #'
    #' @param dat Data.frame. Training data. Default loads "OP treatment visit"
    #' from the ValueTB dataset installed with this package.
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class 'capturetbpriors'. Should be created using
    #' \code{capturetb_priors}.
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = capturetb_priors()) {
      # Validate inputs
      n_priors <- length(priors$prior.beta.mean)
      n_cov <- length(covariates)
      stopifnot(
        "dat must be a data.frame" = is.data.frame(dat),
        "covariates must be a character vector" = is.character(covariates),
        "target must be a character" = is.character(target),
        "target must be a scalar" = length(target) == 1,
        "priors must be 'capturetbpriors'" = inherits(priors, "capturetbpriors")
      )
      if (n_cov < n_priors) {
        stop(sprintf(
          "%s fixed effect priors provided but only %s covariates",
          n_priors, n_cov
        ))
      }
      if (n_priors < n_cov) {
        stop(sprintf(
          "%s covariates provided but only %s fixed effect priors",
          n_cov, n_priors
        ))
      }

      # Check that required columns exist
      missing_covs <- setdiff(covariates, names(dat))
      if (length(missing_covs) > 0) {
        stop(
          "Missing covariates in data: ",
          paste(missing_covs, collapse = ", ")
        )
      }

      if (!target %in% names(dat)) {
        stop("Target variable '", target, "' not found in data")
      }

      if (!"fc_country" %in% names(dat)) {
        stop("Column 'fc_country' required in data")
      }

      # Store in private variables
      private$.covariates <- covariates

      dat_missing <- dat |>
        dplyr::filter(
          dplyr::if_any(
            dplyr::all_of(private$.covariates),
            ~ is.na(.) | is.nan(.) | !is.finite(.)
          )
        )

      dat <- dplyr::anti_join(dat, dat_missing, by = names(dat))

      if (nrow(dat_missing) > 0) {
        warning(sprintf("Removed %d rows with missing data.", nrow(dat_missing)))
      }

      # if there are multiple facilities, take one at random
      dat_unique <- dat |>
        dplyr::group_by(.data$fc_code) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      n_dupes <- nrow(dat) - nrow(dat_unique)
      if (n_dupes > 0) {
        warning(sprintf("Excluded %d rows with duplicate facility codes.", n_dupes))
      }

      private$.training_data <- dat_unique
      private$.target <- target
      private$.priors <- priors
      private$.countries <- as.factor(unique(dat_unique$fc_country))
      private$.samples <- NULL
    },

    #' @description
    #' Fit the model using JAGS. Requires JAGS and rjags to be installed.
    #'
    #' @param n.chains Integer. Number of MCMC chains. Default is 3.
    #' @param n.iter Integer. Number of total iterations per chain.
    #' Default is 100000.
    #' @param n.burnin Integer. Number of burn-in iterations. Default is 5000.
    #' @param n.adapt Integer. Number of adaptation iterations. Default is 5000.
    #' @param n.thin Integer. Thinning interval. Default is 100.
    #' @param seed Optonal integer. Used to seed both the R and JAGS random
    #' generators for reproducible results.
    #' @param ... Additional arguments passed to rjags::jags.model().
    #'
    #' @return Self (invisibly) for method chaining.
    #' @seealso \code{\link[rjags]{jags.model}}
    fit = function(n.chains = 3,
                   n.iter = 1000000,
                   n.burnin = 5000,
                   n.adapt = 5000,
                   n.thin = 100,
                   seed = NULL,
                   ...) {
      if (!requireNamespace("rjags", quietly = TRUE)) {
        stop("Package 'rjags' is required but not installed.")
      }

      if (!is.null(seed)) {
        set.seed(seed)
      }

      model_file <- system.file("jags", "model.model", package = "capturetb")

      dat <- private$.training_data
      x <- private$.numeric_to_logical(dat[, private$.covariates])

      jags_data <- list(
        N = nrow(dat),
        K = length(private$.covariates),
        x = x,
        log_cost = log(dat[[private$.target]]),
        NC = length(unique(dat$fc_country)),
        country = as.numeric(as.factor(dat$fc_country))
      )

      jags_data <- c(jags_data, as.list(private$.priors))

      if (is.null(seed)) {
        jags_mod <- rjags::jags.model(model_file,
          data = jags_data,
          n.chains = n.chains,
          n.adapt = n.adapt,
          ...
        )
      } else {
        jags_inits <- function(chain) {
          list(
            .RNG.name = "base::Mersenne-Twister",
            .RNG.seed = seed * chain
          )
        }
        jags_mod <- rjags::jags.model(model_file,
          data = jags_data,
          n.chains = n.chains,
          n.adapt = n.adapt,
          inits = jags_inits,
          ...
        )
      }
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

      # Store samples
      private$.samples <- samples

      message(
        "Model fitted successfully with ", n.chains, " chains and ",
        n.iter, " iterations."
      )

      invisible(self)
    },

    #' @description
    #' Generate predictions from the fitted model.
    #'
    #' @param dat Data.frame. New input data for predictions.
    #'
    #' @param scale One of "log" or "natural". Default "log".
    #'
    #' @param summarised Logical. If TRUE, returns mean and 95% CI instead of
    #' full posterior samples. Default FALSE.
    #'
    #' @return If summarised=FALSE, matrix of predicted costs with
    #' rows = simulations, columns = input rows. If summarised=TRUE,
    #' data.frame with mean, lower (2.5%), and upper (97.5%) quantiles.
    predict = function(dat, scale = "log", summarised = FALSE) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      if (is.null(private$.samples)) {
        stop("Model must be fitted before making predictions. Call $fit() first.")
      }

      # Validate prediction data
      stopifnot("dat must be a list or data.frame" = is.list(dat))
      dat <- as.data.frame(dat)
      missing_covs <- setdiff(private$.covariates, names(dat))
      if (length(missing_covs) > 0) {
        stop(
          "Missing covariates in prediction data: ",
          paste(missing_covs, collapse = ", ")
        )
      }

      if (!"fc_country" %in% names(dat)) {
        stop("Column 'fc_country' required in prediction data")
      }

      smat <- do.call(rbind, lapply(private$.samples, as.matrix))

      # known country intercepts
      alpha_cols <- paste0("alpha[", as.numeric(private$.countries), "]")
      alphas <- smat[, alpha_cols, drop = FALSE]

      # if country not known, or not in training data
      # generate intercept using hyper-parameters
      mu_alpha <- smat[, "mu_alpha"] # hyper-means
      sig_alpha <- smat[, "sigma_alpha"] # hyper-sds

      alpha_new <- rnorm(length(mu_alpha), mu_alpha, sig_alpha)
      alphas <- cbind(alphas, alpha_new)

      beta_cols <- paste0("beta[", seq_along(private$.covariates), "]")
      betas <- smat[, beta_cols, drop = FALSE]

      x <- as.matrix(private$.numeric_to_logical(
        dat[, private$.covariates, drop = FALSE]
      ))
      x_country <- dat[, "fc_country", drop = FALSE]
      x_country_matrix <- as.data.frame(lapply(
        private$.countries,
        function(country) as.character(country) == x_country
      ))
      x_country_matrix[, ] <- lapply(
        x_country_matrix[, , drop = FALSE],
        as.numeric
      )
      x_country_matrix[, length(private$.countries) + 1] <- 0
      x_country_matrix[
        which(rowSums(x_country_matrix) == 0),
        length(private$.countries) + 1
      ] <- 1

      sig <- smat[, "sigma"]
      pred_means <- alphas %*% t(x_country_matrix) + betas %*% t(x)

      S <- length(sig)
      N <- ncol(pred_means)
      epsilon <- matrix(rnorm(S * N), nrow = S)
      preds <- pred_means + epsilon * sig

      if (scale == "natural") {
        preds <- exp(preds)
      }

      if (summarised) {
        pred_summary <- data.frame(
          mean = apply(preds, 2, mean),
          lower = apply(preds, 2, quantile, probs = 0.025),
          upper = apply(preds, 2, quantile, probs = 0.975)
        )
        return(pred_summary)
      } else {
        return(preds)
      }
    },

    #' @description
    #' Get the name of the target variable.
    #'
    #' @return data.frame.
    target = function() {
      private$.target
    },

    #' @description
    #' Get the data used to fit the model.
    #'
    #' @return data.frame.
    training_data = function() {
      private$.training_data
    },

    #' @description
    #' Get the fitted MCMC samples.
    #'
    #' @return coda::mcmc.list object or NULL if not fitted.
    samples = function() {
      private$.samples
    },

    #' @description
    #' Get the covariates used in the model.
    #'
    #' @return Character vector of covariate names.
    covariates = function() {
      private$.covariates
    },

    #' @description
    #' Get the countries from the training data.
    #'
    #' @return Character vector of country names.
    countries = function() {
      private$.countries
    },

    #' @description
    #' Get the priors used in the model.
    #'
    #' @return List of prior parameters of class 'capturetbpriors'.
    priors = function() {
      private$.priors
    },

    #' @description
    #' Check if the model has been fitted.
    #'
    #' @return Logical indicating if model is fitted.
    is_fitted = function() {
      !is.null(private$.samples)
    },

    #' @description
    #' Create trace plots for MCMC chains using \code{bayesplot}.
    #'
    #' @param ... Additional arguments passed to \code{bayesplot::mcmc_trace}.
    #'
    #' @return A ggplot object showing trace plots.
    #' @importFrom bayesplot mcmc_trace
    #' @seealso \code{\link[bayesplot]{mcmc_trace}}
    mcmc_trace = function(...) {
      private$.check_fitted()
      samples <- private$.samples
      bayesplot::mcmc_trace(samples, ...)
    },

    #' @description
    #' Compute and plot R-hat convergence diagnostics.
    #'
    #' @return A ggplot object showing R-hat diagnostics.
    #' @importFrom ggplot2 ggplot geom_hline geom_point
    #' labs theme_minimal aes
    #' @importFrom coda gelman.diag
    mcmc_rhat = function() {
      private$.check_fitted()
      samples <- private$.samples
      rhat <- coda::gelman.diag(samples,
        autoburnin = FALSE
      )

      ggplot2::ggplot(
        data.frame(
          Parameter = names(rhat$psrf[, 1]),
          Rhat = rhat$psrf[, "Point est."]
        ),
        ggplot2::aes(x = reorder(Parameter, Rhat), y = Rhat)
      ) +
        ggplot2::geom_hline(yintercept = 1.05, linetype = "dashed") +
        ggplot2::geom_point(col = "red") +
        ggplot2::labs(y = expression(hat(R)), x = NULL) +
        ggplot2::theme_minimal()
    },
    #' @description
    #' Create autocorrelation plots for MCMC chains using \code{bayesplot}.
    #'
    #' @param ... Additional arguments passed to \code{bayesplot::mcmc_acf}.
    #' @seealso \code{\link[bayesplot]{mcmc_acf}}
    #' @importFrom bayesplot mcmc_acf
    #' @return A ggplot object showing autocorrelation plots.
    mcmc_acf = function(...) {
      private$.check_fitted()
      samples <- private$.samples
      bayesplot::mcmc_acf(samples, ...)
    },
    #' @description
    #' Computes the effective sample size of the posterior
    #' samples using the \code{coda::effectiveSize} function.
    #'
    #' @return A named numeric vector.
    #' @importFrom coda effectiveSize
    #' @seealso \code{\link[coda]{effectiveSize}}
    n_eff = function() {
      private$.check_fitted()
      samples <- private$.samples
      coda::effectiveSize(samples)
    },
    #' @description
    #' Plot posterior distributions using \code{bayesplot::mcmc_areas}.
    #'
    #' @param prob Numeric. Density to highlight. Default 0.9.
    #' @param ... Additional arguments passed to \code{bayesplot::mcmc_areas}.
    #' @return A ggplot object showing posterior distributions.
    #' @seealso \code{\link[bayesplot]{mcmc_areas}}
    #' @importFrom bayesplot mcmc_areas
    plot_posteriors = function(prob = 0.9, ...) {
      private$.check_fitted()
      samples <- private$.samples
      bayesplot::mcmc_areas(samples, prob = prob, ...)
    },
    #' @description
    #' Calculate model performance metrics on training data.
    #'
    #' This method evaluates the fitted model's performance by making predictions
    #' on the training data and computing mean absolute error (MAE),
    #' root mean square error (RMSE), correlation between observed
    #' and predicted values, and credible interval coverage.
    #'
    #' @param scale Character. Scale for predictions and performance evaluation.
    #' One of "log" (default) or "natural". When "log", observed values are
    #' log-transformed for comparison with log-scale predictions.
    #'
    #' @return A data.frame with performance metrics:
    #' \itemize{
    #'   \item mae: Mean Absolute Error between observed and predicted values
    #'   \item rmse: Root Mean Square Error between observed and predicted values
    #'   \item correlation: Pearson correlation between observed and predicted values
    #'   \item ci_coverage: Proportion of observations within 95% credible intervals
    #' }
    #'
    #' @examples
    #' \dontrun{
    #' model <- MixedEffects$new()
    #' model$fit()
    #' performance_metrics <- model$performance()
    #' print(performance_metrics)
    #' }
    performance = function(scale = "log") {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      private$.check_fitted()
      dat <- private$.training_data
      predictions <- self$predict(dat,
        scale = scale,
        summarised = TRUE
      )

      # Get observed values in the correct scale
      if (scale == "log") {
        observed_values <- log(dat[[private$.target]])
      } else {
        observed_values <- dat[[private$.target]]
      }

      results_df <- data.frame(
        observed = observed_values,
        country = dat$fc_country,
        predictions
      )

      # Calculate performance metrics
      performance_metrics <- results_df |>
        dplyr::summarise(
          mae = mean(abs(.data$observed - .data$mean)),
          rmse = sqrt(mean((.data$observed - .data$mean)^2)),
          correlation = stats::cor(.data$observed, .data$mean),
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper)
        )

      performance_metrics
    },
    #' @description
    #' Create a residual plot for for diagnosing model fit.
    #'
    #' This method generates a diagnostic plot showing residuals (observed minus
    #' predicted values) against fitted values. Residuals are on the log scale
    #' as the model is fitted on a log scale. The plot includes a reference line
    #' at zero, a LOESS smooth curve to identify patterns, and points colored by
    #' country.
    #'
    #' @param add_smooth Logical. Whether to add a LOESS smooth curve to
    #' identify patterns in residuals. Default TRUE.
    #' @param color_by_country Logical. Whether to color points by country.
    #' Default TRUE.
    #'
    #' @return A ggplot object showing residuals vs fitted values.
    #' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_smooth
    #' labs theme_minimal theme
    plot_residuals = function(add_smooth = TRUE,
                              color_by_country = TRUE) {
      stopifnot(
        "add_smooth must be logical" = is.logical(add_smooth),
        "color_by_country must be logical" = is.logical(color_by_country)
      )

      private$.check_fitted()
      dat <- private$.training_data
      pred <- self$predict(dat, scale = "log", summarised = TRUE)
      observed <- dat[[private$.target]]

      residuals <- log(observed) - pred$mean

      res_df <- data.frame(
        fitted = pred$mean,
        country = dat$fc_country,
        residuals = residuals
      )

      # Create base plot
      p <- ggplot2::ggplot(res_df, ggplot2::aes(
        x = .data$fitted,
        y = .data$residuals
      )) +
        ggplot2::geom_hline(
          yintercept = 0, linetype = "dashed",
          color = "red"
        )

      if (color_by_country) {
        p <- p + ggplot2::geom_point(ggplot2::aes(color = .data$country),
          alpha = 0.7
        )
      } else {
        p <- p + ggplot2::geom_point(alpha = 0.7)
      }

      if (add_smooth) {
        p <- p + ggplot2::geom_smooth(
          method = "loess", se = TRUE,
          color = "grey", alpha = 0.3
        )
      }

      p <- p + ggplot2::labs(
        title = "Residuals vs Fitted Values",
        subtitle = "Points should be randomly scattered around the red line",
        x = "Residuals (Log Observed - Log Predicted)",
        y = "Log Predicted Values",
        color = "Country"
      ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

      return(p)
    },
    #' @description
    #' Create a scatter plot of observed vs predicted values to assess fit.
    #'
    #' This method generates a diagnostic plot showing the relationship between
    #' observed and predicted values on the training data, with a reference line
    #' for perfect predictions and optional confidence intervals.
    #'
    #' @param scale Character. Scale for the plot. One of "log" (default) or
    #' "natural". When "log", both observed and predicted values are shown on
    #' log scale.
    #' @param include_ci Logical. Whether to show prediction intervals as
    #' error bars. Default TRUE.
    #' @param color_by_country Logical. Whether to color points by country.
    #' Default TRUE.
    #' @return A ggplot object showing observed vs predicted values.
    #'
    #' @examples
    #' \dontrun{
    #' model <- MixedEffects$new()
    #' model$fit()
    #' p <- model$plot_fit()
    #' print(p)
    #'
    #' # Natural scale without confidence intervals
    #' p2 <- model$plot_fit(scale = "natural", include_ci = FALSE)
    #' print(p2)
    #' }
    #'
    #' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_errorbar
    #' labs theme_minimal
    plot_fit = function(scale = "log",
                        include_ci = TRUE,
                        color_by_country = TRUE) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural"),
        "include_ci must be logical" = is.logical(include_ci)
      )

      private$.check_fitted()
      dat <- private$.training_data
      pred <- self$predict(dat, scale = scale, summarised = TRUE)
      observed <- dat[[private$.target]]

      if (scale == "log") {
        observed <- log(observed)
        x_lab <- paste("Observed", private$.target, "(log scale)")
        y_lab <- paste("Predicted", private$.target, "(log scale)")
      } else {
        x_lab <- paste("Observed", private$.target)
        y_lab <- paste("Predicted", private$.target)
      }

      results_df <- data.frame(
        observed = observed,
        country = dat$fc_country,
        pred
      )

      plot <- ggplot2::ggplot(results_df, ggplot2::aes(
        x = .data$observed,
        y = .data$mean
      )) +
        ggplot2::geom_abline(
          slope = 1, intercept = 0, linetype = "dashed",
          color = "red"
        ) +
        ggplot2::labs(
          title = "Observed vs Predicted Values",
          subtitle = "Dashed line shows perfect predictions",
          x = x_lab,
          y = y_lab,
          color = "Country"
        ) +
        ggplot2::theme_minimal()

      if (include_ci) {
        plot <- plot + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
          alpha = 0.3, width = 0
        )
      }
      if (color_by_country) {
        plot <- plot + ggplot2::geom_point(ggplot2::aes(color = .data$country),
          alpha = 0.7
        )
      } else {
        plot <- plot + ggplot2::geom_point(alpha = 0.7)
      }
      return(plot)
    },
    #' @description
    #' Perform k-fold cross-validation.
    #'
    #' @param k_folds Integer. Number of folds for cross-validation.
    #' Default is 5.
    #' @param scale Scale to return results in. One of "log" or "natural".
    #' Default "log".
    #' @param seed Integer. Optional random seed for reproducible fold
    #' assignment and model runs. Default NULL.
    #' @param ... Additional arguments passed to the fit() method.
    #'
    #' @return Data.frame with predictions from cross-validation, including
    #' fold assignments and observed values.
    k_fold_cv = function(k_folds = 5, scale = "log",
                         seed = NULL, ...) {
      if (!is.null(seed)) {
        set.seed(seed)
      }

      dat <- private$.training_data
      n_obs <- nrow(dat)

      # Create fold assignments
      fold_ids <- sample(rep(1:k_folds, length.out = n_obs))
      dat$fold <- fold_ids

      cv_predictions <- list()

      for (fold in 1:k_folds) {
        message("Processing fold ", fold, " of ", k_folds)

        # Split data
        train_data <- dat[dat$fold != fold, ]
        test_data <- dat[dat$fold == fold, ]

        # Create temporary model for this fold
        temp_model <- MixedEffects$new(
          dat = train_data,
          covariates = private$.covariates,
          target = private$.target,
          priors = private$.priors
        )

        # Fit model on training fold
        temp_model$fit(seed = seed, ...)

        # Make predictions on test fold
        fold_preds <- temp_model$predict(
          test_data,
          scale = scale,
          summarised = TRUE
        )

        obs <- test_data[[private$.target]]

        if (scale == "log") {
          obs <- log(obs)
        }

        fold_results <- data.frame(
          fold = fold,
          observed = obs,
          fold_preds
        )

        cv_predictions[[fold]] <- fold_results
      }

      # Combine all fold results
      do.call(rbind, cv_predictions)
    }
  ),
  private = list(
    .covariates = NULL,
    .countries = NULL,
    .training_data = NULL,
    .fitted_data = NULL,
    .target = NULL,
    .priors = NULL,
    .samples = NULL,
    .numeric_to_logical = function(x) {
      logical_cols <- sapply(x, is.logical)
      if (any(logical_cols)) {
        x[, logical_cols] <- lapply(x[, logical_cols, drop = FALSE], as.numeric)
      }
      x
    },
    .check_fitted = function() {
      if (!self$is_fitted()) {
        stop(
          "Model must be fitted first. ",
          "Call $fit() first."
        )
      }
    }
  )
)
