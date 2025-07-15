#' JAGSModel R6 Class
#'
#' Base R6 class for fitting and predicting costs using a JAGS model.
#' Contains functionality that is shared across model classes.
#' Should not be instantiated directly, rather one of its children
#' classes should be used:  [`FixedEffects`],
#' [`MixedEffects`],  [`RandomSlopes`]
#'
#' @importFrom R6 R6Class
#' @importFrom rlang .data
#' @seealso [`FixedEffects`]
#' @seealso [`MixedEffects`]
#' @seealso [`RandomSlopes`]
JAGSModel <- R6::R6Class("JAGSModel",
  public = list(
    #' @description
    #' Initialize a new model instance.
    #'
    #' @param dat Data.frame. Training data. Default loads "OP treatment visit"
    #' from the ValueTB dataset installed with this package.
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class "capturetbpriors". Should be created using
    #' [`capturetb_priors`]
    #' @param model Name of the JAGS model file.
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = NULL,
                          model) {
      n_cov <- length(covariates)
      if (is.null(priors)) {
        priors <- capturetb_priors(
          beta.mean = rep(0, n_cov),
          beta.precision = rep(0.01, n_cov)
        )
        warning(sprintf("Priors not provided. Vague priors assumed for each covariate coefficient with mu 0 and precision 0.01."))
      }
      n_priors <- length(priors$prior.beta.mean)
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

      params <- c(
        "alpha",
        "beta",
        "sigma"
      )

      if (model != "fixedeffects.model") {
        params <- c(params, "mu_alpha", "sigma_alpha")
      }
      if (model == "randomslopes.model") {
        params <- c(params, "mu_beta", "sigma_beta")
      }

      private$.params <- params

      private$.training_data <- dat_unique
      private$.target <- target
      private$.priors <- priors
      private$.countries <- as.factor(unique(dat_unique$fc_country))
      private$.samples <- NULL
      private$.model <- model
    },

    #' @description
    #' Fit the model using JAGS. Requires JAGS and rjags to be installed.
    #'
    #' @param n.chains Integer. Number of MCMC chains. Default is 3.
    #' @param n.iter Integer. Number of total iterations per chain.
    #' Default is 1000000.
    #' @param n.burnin Integer. Number of burn-in iterations. Default is 5000.
    #' @param n.adapt Integer. Number of adaptation iterations. Default is 5000.
    #' @param n.thin Integer. Thinning interval. Default is 100.
    #' @param seed Optonal integer. Used to seed both the R and JAGS random
    #' generators for reproducible results.
    #' @param ... Additional arguments passed to [rjags::jags.model].
    #'
    #' @return Self (invisibly) for method chaining.
    #' @seealso [rjags::jags.model].
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

      model_file <- system.file("jags", private$.model, package = "capturetb")

      dat <- private$.training_data
      x <- private$.numeric_to_logical(dat[, private$.covariates])

      jags_data <- list(
        N = nrow(dat),
        K = length(private$.covariates),
        x = x,
        log_cost = log(dat[[private$.target]])
      )

      if (private$.model != "fixedeffects.model") {
        jags_data$NC <- length(unique(dat$fc_country))
        jags_data$country <- as.numeric(as.factor(dat$fc_country))
      }

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
        variable.names = private$.params,
        n.iter = n.iter,
        thin = n.thin
      )

      # Store samples
      private$.samples <- samples

      dic_samples <- rjags::dic.samples(
        model = jags_mod,
        n.iter = n.iter,
        thin = n.thin,
        type = "pD"
      )

      private$.dic_samples <- dic_samples

      rhat <- coda::gelman.diag(samples, autoburnin = FALSE)
      if (any(rhat$psrf > 1.1)) {
        warning(sprintf(
          "Model may not have converged. Max rhat is %s",
          max(rhat$psrf)
        ))
      }

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
      stop("Method not implemented on the base Model class. Use one of FixedEffects, MixedEffects or RandomSlopes")
    },
    #' @description
    #' View distribution of binary characteristics across countries
    #'
    #' @return A data frame with one row per country (`fc_country`), including
    #' the total number of records for that country (`n_total`) and the count of
    #' `TRUE` values for each logical covariate.
    baselines = function() {
      logical_cols <- names(private$.training_data[, private$.covariates] |>
        dplyr::select_if(is.logical))

      private$.training_data |>
        dplyr::group_by(fc_country) |>
        dplyr::summarise(
          n_total = dplyr::n(),
          dplyr::across(
            all_of(logical_cols),
            ~ sum(.x == 1),
            .names = "n_{.col}"
          )
        )
    },
    #' @description
    #' View correlations between covariates in the training data
    #'
    #' @param plot Logical. If TRUE, return a ggplot2
    #' object. If FALSE return a correlation matrix. Default TRUE.
    #' @import ggcorrplot ggcorrplot
    #' @return ggplot2 object or correlation matrix
    covariate_correlation = function(plot = TRUE) {
      stopifnot(
        "plot must be TRUE or FALSE" =
          (length(plot) == 1 && is.logical(plot))
      )
      cor_mat <- cor(private$.training_data[, private$.covariates])
      if (plot) {
        return(ggcorrplot::ggcorrplot(cor_mat))
      } else {
        return(cor_mat)
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
    #' @return [coda::mcmc.list] object or NULL if not fitted.
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
      !is.null(private$.samples) & !is.null(private$.dic_samples) 
    },

    #' @description
    #' Create trace plots for MCMC chains using [bayesplot::mcmc_trace].
    #'
    #' @param ... Additional arguments passed to [bayesplot::mcmc_trace].
    #'
    #' @return A ggplot object showing trace plots.
    #' @importFrom bayesplot mcmc_trace
    #' @seealso [bayesplot::mcmc_trace]
    mcmc_trace = function(...) {
      private$.check_fitted()
      samples <- private$.samples
      bayesplot::mcmc_trace(samples, ...)
    },

    #' @description
    #' Compute and plot R-hat convergence diagnostics.
    #'
    #' @return A [ggplot2::ggplot] object showing R-hat diagnostics.
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
    #' Create autocorrelation plots for MCMC chains using [bayesplot::mcmc_acf].
    #'
    #' @param ... Additional arguments passed to [bayesplot::mcmc_acf].
    #' @seealso [bayesplot::mcmc_acf]
    #' @importFrom bayesplot mcmc_acf
    #' @return A ggplot object showing autocorrelation plots.
    mcmc_acf = function(...) {
      private$.check_fitted()
      samples <- private$.samples
      bayesplot::mcmc_acf(samples, ...)
    },
    #' @description
    #' Computes the effective sample size of the posterior
    #' samples using the [coda::effectiveSize] function.
    #'
    #' @return A named numeric vector.
    #' @importFrom coda effectiveSize
    #' @seealso [coda::effectiveSize]
    n_eff = function() {
      private$.check_fitted()
      samples <- private$.samples
      coda::effectiveSize(samples)
    },
    #' @description
    #' Plot posterior distributions using [bayesplot::mcmc_areas].
    #'
    #' @param prob Numeric. Density to highlight. Default 0.9.
    #' @param ... Additional arguments passed to [bayesplot::mcmc_areas].
    #' @return A ggplot object showing posterior distributions.
    #' @seealso [bayesplot::mcmc_areas]
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
    #' root mean square error (RMSE), Bayesian R2, credible interval coverage and
    #' median credible interval width.
    #'
    #' @param scale One of "log" or "natural". Default "log".
    #'
    #' @param by_country Logical. If TRUE, returns metrics calculated
    #' on country sub-grpups. Default FALSE.
    #' @return A data.frame with performance metrics:
    #' \itemize{
    #'  \item country: Only present if by_country = TRUE
    #'   \item mae: Mean Absolute Error between observed and predicted values
    #'   \item rmse: Root Mean Square Error between observed and predicted values
    #'   \item bayesian_r2: Mean Bayesian r2 estimate.
    #'   \item ci_coverage: Proportion of observations within 95% credible intervals
    #'   \item median_ci: The median width of 95% credible intervals
    #' }
    #'
    #' @examples
    #' \dontrun{
    #' model <- MixedEffects$new()
    #' model$fit()
    #' performance_metrics <- model$performance()
    #' print(performance_metrics)
    #' }
    performance = function(scale = "log", by_country = FALSE) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      private$.check_fitted()
      dat <- private$.training_data
      preds <- self$predict(dat,
        scale = scale,
        summarised = FALSE
      )

      pred_summary <- data.frame(
        mean = apply(preds, 2, mean),
        lower = apply(preds, 2, quantile, probs = 0.025),
        upper = apply(preds, 2, quantile, probs = 0.975)
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
        pred_summary
      )

      if (by_country) {
        results_df <- results_df |> dplyr::group_by(country)
      }

      var_yhat <- apply(preds, 1, var)
      resid_mat <- preds - rep(observed_values, each = nrow(preds))
      var_resid <- apply(resid_mat, 1, var)
      r2_draws <- var_yhat / (var_yhat + var_resid)

      # Calculate performance metrics
      performance_metrics <- results_df |>
        dplyr::summarise(
          mae = mean(abs(.data$observed - .data$mean)),
          rmse = sqrt(mean((.data$observed - .data$mean)^2)),
          bayesian_r2 = mean(r2_draws),
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper),
          median_ci = median(.data$upper - .data$lower),
          .groups = "drop"
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
    #' @return A [ggplot2::ggplot] object showing residuals vs fitted values.
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
    #' @param scale One of "log" or "natural". Default "log".
    #' @param include_ci Logical. Whether to show prediction intervals as
    #' error bars. Default TRUE.
    #' @param color_by_country Logical. Whether to color points by country.
    #' Default TRUE.
    #' @return A [ggplot2::ggplot] object showing observed vs predicted values.
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
        plot <- plot +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
            alpha = 0.2, width = 0
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
    #' @param scale One of "log" or "natural". Default "log".
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
      all_predictions <- list()

      for (fold in 1:k_folds) {
        message("Processing fold ", fold, " of ", k_folds)

        # Split data
        train_data <- dat[dat$fold != fold, ]
        test_data <- dat[dat$fold == fold, ]

        if (private$.model == "fixedeffects.model") {
          model_type <- FixedEffects
        }
        if (private$.model == "mixedeffects.model") {
          model_type <- MixedEffects
        }
        if (private$.model == "randomslopes.model") {
          model_type <- RandomSlopes
        }

        # Create temporary model for this fold
        temp_model <- model_type$new(
          dat = train_data,
          covariates = private$.covariates,
          target = private$.target,
          priors = private$.priors
        )

        # Fit model on training fold
        temp_model$fit(seed = seed, ...)

        # Make full predictions on test fold (not summarised)
        fold_preds_full <- temp_model$predict(
          test_data,
          scale = scale,
          summarised = FALSE
        )

        # Store full predictions for Bayesian R² calculation
        all_predictions[[fold]] <- fold_preds_full

        # Calculate summary statistics for results dataframe
        fold_preds_summary <- data.frame(
          mean = apply(fold_preds_full, 2, mean),
          lower = apply(fold_preds_full, 2, quantile, probs = 0.025),
          upper = apply(fold_preds_full, 2, quantile, probs = 0.975)
        )

        obs <- test_data[[private$.target]]

        if (scale == "log") {
          obs <- log(obs)
        }

        fold_results <- data.frame(
          fold = fold,
          observed = obs,
          fold_preds_summary
        )

        cv_predictions[[fold]] <- fold_results
      }

      # Combine all fold results
      results_df <- do.call(rbind, cv_predictions)

      # Combine all predictions for Bayesian R² calculation
      all_preds_matrix <- do.call(cbind, all_predictions)
      observed_values <- results_df$observed

      # Calculate Bayesian R²
      var_yhat <- apply(all_preds_matrix, 1, var)
      resid_mat <- all_preds_matrix - rep(observed_values, each = nrow(all_preds_matrix))
      var_resid <- apply(resid_mat, 1, var)
      r2_draws <- var_yhat / (var_yhat + var_resid)
      bayesian_r2 <- mean(r2_draws)

      # Calculate performance metrics
      performance_metrics <- results_df |>
        dplyr::summarise(
          mae = mean(abs(.data$observed - .data$mean)),
          rmse = sqrt(mean((.data$observed - .data$mean)^2)),
          bayesian_r2 = bayesian_r2,
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper),
          median_ci = median(.data$upper - .data$lower)
        )

      attr(results_df, "performance") <- performance_metrics
      results_df
    },
    #' @description
    #' Perform leave-one-country-out cross-validation.
    #'
    #' @param scale One of "log" or "natural". Default "log".
    #' @param seed Integer. Optional random seed for reproducible fold
    #' assignment and model runs. Default NULL.
    #' @param ... Additional arguments passed to the fit() method.
    #'
    #' @return Data.frame with predictions from cross-validation, including
    #' country and observed values.
    leave_one_country_out = function(scale = "log", seed = NULL, ...) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      dat <- private$.training_data
      countries <- private$.countries

      cv_predictions <- list()
      all_predictions <- list()
      all_models <- list()

      for (i in seq_along(countries)) {
        train_data <- dat |> dplyr::filter(fc_country != countries[[i]])
        test_data <- dat |> dplyr::filter(fc_country == countries[[i]])

        if (private$.model == "fixedeffects.model") {
          model_type <- FixedEffects
        }
        if (private$.model == "mixedeffects.model") {
          model_type <- MixedEffects
        }
        if (private$.model == "randomslopes.model") {
          model_type <- RandomSlopes
        }

        # Create temporary model for this country
        temp_model <- model_type$new(
          dat = train_data,
          covariates = private$.covariates,
          target = private$.target,
          priors = private$.priors
        )

        all_models[[i]] <- temp_model

        # Fit model on training data
        temp_model$fit(seed = seed, ...)

        # Make full predictions on test country (not summarised)
        fold_preds_full <- temp_model$predict(
          test_data,
          scale = scale,
          summarised = FALSE
        )

        # Store full predictions for Bayesian R² calculation
        all_predictions[[i]] <- fold_preds_full

        # Calculate summary statistics for results dataframe
        fold_preds_summary <- data.frame(
          mean = apply(fold_preds_full, 2, mean),
          lower = apply(fold_preds_full, 2, quantile, probs = 0.025),
          upper = apply(fold_preds_full, 2, quantile, probs = 0.975)
        )

        obs <- test_data[[private$.target]]

        if (scale == "log") {
          obs <- log(obs)
        }

        fold_results <- data.frame(
          country = test_data$fc_country,
          observed = obs,
          fold_preds_summary
        )

        cv_predictions[[i]] <- fold_results
      }

      # Combine all fold results
      results_df <- do.call(rbind, cv_predictions)

      # Combine all predictions for Bayesian R² calculation
      all_preds_matrix <- do.call(cbind, all_predictions)
      observed_values <- results_df$observed

      # Calculate Bayesian R²
      var_yhat <- apply(all_preds_matrix, 1, var)
      resid_mat <- all_preds_matrix - rep(observed_values, each = nrow(all_preds_matrix))
      var_resid <- apply(resid_mat, 1, var)
      r2_draws <- var_yhat / (var_yhat + var_resid)
      bayesian_r2 <- mean(r2_draws)

      # Calculate performance metrics
      performance_metrics <- results_df |>
        dplyr::summarise(
          mae = mean(abs(.data$observed - .data$mean)),
          rmse = sqrt(mean((.data$observed - .data$mean)^2)),
          bayesian_r2 = bayesian_r2,
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper),
          median_ci = median(.data$upper - .data$lower)
        )
      attr(results_df, "models") <- all_models
      attr(results_df, "performance") <- performance_metrics
      results_df
    },
    #' @description
    #' This function computes the Expected Value of Perfect Information (EVPI)
    #' for given facility characteristics and a vector of willingness-to-pay
    #' thresholds (`lambda`).
    #'
    #' @param dat `data.frame` of model inputs (facility characteristics)
    #' @param lambda Numeric vector of willingness-to-pay thresholds
    #' at which to calculate EVPI.
    #' @param n_outputs Numeric scalar. The number of outputs to compare to the
    #' willingness-to-pay thresholds. If the willingness-to-pay is for a single
    #' output then leave as the default of 1. Should either be length 1, for
    #' the same number of outputs at each facility, or should have an entry for
    #' each row in `dat`.
    #'
    #' @return A numeric vector of EVPI values, one for each value
    #' in `lambda`.
    #'
    #' @examples
    #' \dontrun{
    #' dat <- list(x1 = 1, x2 = 2)
    #' lambda <- c(10000, 20000, 30000)
    #' model$evpi(dat, lambda)
    #' }
    #'
    #' @seealso [predict()]
    #' @export
    evpi = function(dat, lambda, n_outputs = 1) {
      stopifnot(
        "dat must be a list or data.frame" =
          (is.list(dat))
      )
      dat <- as.data.frame(dat)
      if (length(n_outputs) == 1) {
        n_outputs <- rep(n_outputs, nrow(dat))
      }
      stopifnot(
        "n_outputs must have length == nrow(dat)" =
          (is.numeric(n_outputs) && length(n_outputs) == nrow(dat))
      )
      stopifnot("lambda must be a numeric vector" = is.numeric(lambda))
      cost_pred <- self$predict(dat,
        scale = "natural",
        summarised = FALSE
      )
      cost_pred_sum <- rowSums(cost_pred * n_outputs)
      evpi <- c()
      for (i in seq_along(lambda)) {
        exp_max <- mean(sapply(
          cost_pred_sum,
          function(c) max(private$.net_benefit(lambda[i], c), 0)
        ))
        max_exp <- max(mean(sapply(
          cost_pred_sum,
          function(c) private$.net_benefit(lambda[i], c)
        )), 0)
        evpi <- c(evpi, exp_max - max_exp)
      }
      evpi
    },
    #' @description
    #' Predict the total cost across multiple facilities with arbitrary numbers
    #' of outputs at each
    #'
    #' @param dat `data.frame` of model inputs (facility characteristics)
    #' @param n_outputs Number of outputs at each facility. Should either be
    #' length 1, for the same number of outputs at each facility, or should
    #' have an entry for each row in `dat`.
    #'
    #' @return Posterior distribution of predicted total cost, as a vector
    #' @export
    predict_total = function(dat, n_outputs) {
      stopifnot(
        "dat must be a list or data.frame" =
          (is.list(dat))
      )
      dat <- as.data.frame(dat)
      if (length(n_outputs) == 1) {
        n_outputs <- rep(n_outputs, nrow(dat))
      }
      stopifnot(
        "n_outputs must have length == nrow(dat)" =
          (is.numeric(n_outputs) && length(n_outputs) == nrow(dat))
      )
      samples <- self$predict(dat, scale = "natural", summarised = FALSE)
      samples_total <- sweep(samples, 2, n_outputs, "*")
      rowSums(samples_total)
    },
    #' @description
    #' Extract fitted model parameters with credible intervals.
    #'
    #' This method summarises fitted parameters using
    #' [bayestestR::describe_posterior]. See [bayestestR::describe_posterior]
    #' for full documentation of available argument.
    #' @param centrality The point-estimates (centrality indices) to compute.
    #' Default "mean".
    #' @param ci Value or vector of probability of the CI (between 0 and 1)
    #' to be estimated. Default `0.95` (`95%`).
    #' @param ci_method The type of index used for Credible Interval.
    #' Default ETI.
    #' @param test The indices of effect existence to compute. Default NULL.
    #' @param ... Other arguments that will be passed to [bayestestR::describe_posterior].
    #' @seealso bayestestR describe_posterior
    #' @return A data.frame of parameter summaries
    #' @examples
    #' model <- unitcost()
    #' params <- model$fitted_parameters()
    #' print(params)
    #'
    #' # 90% credible intervals
    #' params_90 <- model$fitted_parameters(ci = 0.9)
    fitted_parameters = function(centrality = "mean",
                                 ci = 0.95,
                                 ci_method = "eti",
                                 test = NULL, ...) {
      private$.check_fitted()
      bayestestR::describe_posterior(private$.samples,
        centrality = centrality,
        ci = ci,
        ci_method = ci_method,
        test = test, ...
      )
    },
    #' @description Retrieve penalized deviance statistics.
    #'
    #' This method returns cached penalized deviance statistics created
    #' at the time of model fitting using [rjags::dic.samples()].
    #' @param summarised Logical indicating whether to return the full
    #' sample or the summarised DIC. Default TRUE.
    #' @seealso rjags dic.samples
    #' @examples
    #' mod <- unitcost()
    #' mod$mcmc_DIC()
    #' @export
    mcmc_DIC = function(summarised = TRUE) {
      private$.check_fitted()
      if (summarised) {
        sum(private$.dic_samples$deviance) + mean(sum(private$.dic_samples[[2]]))
      } else {
        private$.dic_samples
      }
    }
  ),
  private = list(
    .model = NULL,
    .dic_samples = NULL,
    .params = NULL,
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
    .net_benefit = function(lambda, cost) {
      lambda - cost
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
