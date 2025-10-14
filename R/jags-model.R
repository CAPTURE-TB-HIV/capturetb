#' JAGSModel R6 Class
#'
#' R6 class for fitting and predicting costs using a JAGS model.
#'
#' @description
#' This class allows the user to fit and use a mixed effects model with
#' intercepts that vary by country, facility and output type as well as fixed
#' covariate effects. It is used for the comparison of different model
#' structures as in the `vignette("03_model-comparisons", package = "capturetb")`
#' vignette, and is the class underlying the [`unitcost`], [`unitcost_fixed`]
#' and [`unitcost_ohd`] models.
#'
#' @importFrom R6 R6Class
#' @importFrom rlang .data
JAGSModel <- R6::R6Class("JAGSModel",
  public = list(
    #' @description
    #' Initialize a new model instance.
    #'
    #' @param dat Data.frame. Training data
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class "capturetbpriors". Should be created using
    #' [`capturetb_priors`]. If NULL, non-informative priors will be used.
    initialize = function(dat,
                          covariates,
                          target,
                          priors = NULL) {
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

      dat_missing <- dat[Reduce(`|`, lapply(dat[, private$.covariates, drop = FALSE], function(col) {
        is.na(col) | is.nan(col) | !is.finite(col)
      })), , drop = FALSE]

      dat <- dplyr::anti_join(dat, dat_missing, by = names(dat))

      if (nrow(dat_missing) > 0) {
        warning(sprintf("Removed %d rows with missing data.", nrow(dat_missing)))
      }

      centering_values <- sapply(dat, function(col) {
        attr(col, "scaled:center")
      })
      private$.centering_values <- centering_values[!sapply(centering_values, is.null)]

      params <- c(
        "alpha",
        "beta",
        "sigma",
        "sigma_c",
        "country_effect"
      )
      stopifnot("dat must be a data.frame" = is.data.frame(dat))
      if (length(unique(dat[["output"]])) > 1) {
        message("Multiple outputs detected. Including output-level random effects in model.")
        private$.model <- "outputeffects.model"
        params <- c(
          params,
          "sigma_f",
          "sigma_v",
          "output_effect",
          "fc_effect"
        )
      } else {
        message("Single output type detected. Not including output-level random effects in model.")
        private$.model <- "singleoutput.model"
      }

      private$.params <- params
      dat <- tibble::as_tibble(dat)
      class(dat) <- append("capturetbdata", class(dat))
      private$.training_data <- dat
      private$.target <- target
      private$.priors <- priors
      private$.countries <- as.factor(unique(dat$fc_country))
      private$.outputs <- as.factor(unique(dat$output))
      private$.facilities <- as.factor(unique(dat$fc_code))
      private$.samples <- NULL
    },
    #' @description
    #' Fit the model using JAGS. Requires JAGS and runjags to be installed.
    #'
    #' @param n.chains Integer. Number of MCMC chains. Default is 3.
    #' @param n.iter Integer. Number of total iterations per chain.
    #' Default is 1000000.
    #' @param n.burnin Integer. Number of burn-in iterations. Default is 5000.
    #' @param n.adapt Integer. Number of adaptation iterations. Default is 5000.
    #' @param n.thin Integer. Thinning interval. Default is 100.
    #' @param seed Optonal integer. Used to seed both the R and JAGS random
    #' generators for reproducible results.
    #' @param ... Additional arguments passed to [runjags::run.jags].
    #'
    #' @return Self (invisibly) for method chaining.
    #' @seealso [runjags::run.jags].
    fit = function(n.chains = 3,
                   n.iter = 1000000,
                   n.burnin = 5000,
                   n.adapt = 5000,
                   n.thin = 100,
                   seed = NULL,
                   ...) {
      if (!requireNamespace("runjags", quietly = TRUE)) {
        stop("Package 'runjags' is required but not installed.")
      }
      if (!requireNamespace("rjags", quietly = TRUE)) {
        stop("Package 'rjags' is required but not installed.")
      }

      if (!is.null(seed)) {
        set.seed(seed)
      }

      model_file <- system.file("jags", private$.model, package = "capturetb")

      dat <- private$.training_data
      x <- private$.logical_to_numeric(dat[, private$.covariates, drop = FALSE])

      dat <- dat |>
        dplyr::mutate(
          fc_id = as.numeric(gsub("[^0-9]", "", fc_code))
        )
      jags_data <- list(
        N = nrow(dat),
        K = length(private$.covariates),
        x = as.matrix(x),
        log_cost = log(dat[[private$.target]]),
        NC = length(unique(dat$fc_country)),
        country = as.numeric(as.factor(dat$fc_country))
      )

      jags_data <- c(jags_data, as.list(private$.priors))

      if (private$.model == "outputeffects.model") {
        jags_data <- c(jags_data, list(
          fc = as.numeric(as.factor(dat$fc_code)),
          NFC = length(unique(dat$fc_code)),
          NO = length(unique(dat$output)),
          output = as.numeric(as.factor(dat$output))
        ))
      } else {
        jags_data$prior.sigma_f.scale <- NULL
        jags_data$prior.sigma_v.scale <- NULL
      }

      if (is.null(seed)) {
        seed <- as.integer(Sys.time()) %% 1e7
      }
      jags_inits <- function(chain) {
        list(
          .RNG.name = "base::Mersenne-Twister",
          .RNG.seed = seed * chain
        )
      }
      jags_mod <- runjags::run.jags(
        model = model_file,
        data = jags_data,
        monitor = private$.params,
        n.chains = n.chains,
        adapt = n.adapt,
        burnin = n.burnin,
        sample = n.iter / n.thin,
        thin = n.thin,
        summarise = FALSE,
        inits = jags_inits,
        method = "parallel",
        ...
      )

      samples <- coda::as.mcmc.list(jags_mod)
      private$.samples <- samples
      private$.DIC <- runjags::extract(jags_mod, what = "DIC")

      tryCatch(
        {
          rhat <- coda::gelman.diag(samples, autoburnin = FALSE)
          if (any(rhat$psrf > 1.1)) {
            warning(sprintf(
              "Model may not have converged. Max rhat is %s",
              max(rhat$psrf)
            ))
          }
        },
        error = function(e) {
          warning(sprintf("Model has not converged: %s", e))
        }
      )

      message(
        "Model fitted successfully with ", n.chains, " chains and ",
        n.iter, " iterations."
      )

      invisible(self)
    },
    #' @description
    #' Generate predictions from the fitted model.
    #'
    #' @param dat New input data for predictions. This should be prepared
    #' for the model using [prepare_covariates].
    #' @param scale One of "log" or "natural". Default "log".
    #' @param summarised Logical. If TRUE, summarises predictions using
    #' [bayestestR::describe_posterior]. See [bayestestR::describe_posterior]
    #' for full documentation of available arguments. Default FALSE.
    #' @param centrality The point-estimates (centrality indices) to compute.
    #' Default "mean".
    #' @param ci Value or vector of probability of the CI (between 0 and 1)
    #' to be estimated. Default `0.95` (`95%`).
    #' @param ci_method The type of index used for Credible Interval.
    #' Default ETI.
    #' @param test The indices of effect existence to compute. Default NULL.
    #' See [bayestestR::describe_posterior] for options.
    #' @param ... Other arguments that will be passed to
    #' [bayestestR::describe_posterior].
    #'
    #' @return If summarised=FALSE, matrix of predicted costs with
    #' rows = simulations, columns = input rows. If summarised=TRUE,
    #' data.frame with central point estimate and confidence interval(s).
    #' @seealso bayestestR describe_posterior
    #' @importFrom bayestestR describe_posterior
    predict = function(dat,
                       scale = "log",
                       summarised = FALSE,
                       centrality = "mean",
                       ci = 0.95,
                       test = NULL,
                       ...) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      if (is.null(private$.samples)) {
        stop("Model must be fitted before making predictions. Call $fit() first.")
      }

      private$.validate_data(dat)

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

      preds <- private$.predict(dat)

      if (scale == "natural") {
        preds <- exp(preds)
      }

      if (summarised) {
        pred_summary <- bayestestR::describe_posterior(as.data.frame(preds),
          test = test,
          ci = ci,
          centrality = centrality,
          ...
        )
        names(pred_summary)[[1]] <- "Observation"
        pred_summary$Observation <- sub("V", "", pred_summary$Observation)
        return(pred_summary)
      } else {
        return(preds)
      }
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
    #' @param plot Logical. If TRUE, return a [ggplot2::ggplot]
    #' object. If FALSE return a correlation matrix. Default TRUE.
    #' @return [ggplot2::ggplot] object or correlation matrix
    covariate_correlation = function(plot = TRUE) {
      stopifnot(
        "plot must be TRUE or FALSE" =
          (length(plot) == 1 && is.logical(plot))
      )
      cor_mat <- cor(private$.training_data[, private$.covariates])
      if (plot) {
        ord <- hclust(as.dist(1 - abs(cor_mat)))$order
        cor_mat <- cor_mat[ord, ord]

        corr_long <- as.data.frame(as.table(cor_mat))
        names(corr_long) <- c("x", "y", "r")
        corr_long$x <- factor(corr_long$x, levels = rownames(cor_mat))
        corr_long$y <- factor(corr_long$y, levels = colnames(cor_mat))

        p <- ggplot2::ggplot(corr_long, aes(x, y, fill = r)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.3) +
          ggplot2::scale_fill_gradient2(
            limits = c(-1, 1),
            low = "#B2182B", mid = "white", high = "#2166AC",
            midpoint = 0, name = "Pearson r"
          ) +
          ggplot2::coord_fixed() +
          ggplot2::labs(
            x = NULL,
            y = NULL,
            title = "Correlation heatmap (Pearson)"
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
        return(p)
      } else {
        return(cor_mat)
      }
    },
    #' @description
    #' Get the name of the target variable.
    #'
    #' @return character scalar.
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
    #' Get the outputs used for random effects.
    #'
    #' @return factor.
    outputs = function() {
      private$.outputs
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
    #' Get any centering values used to center covariates in the training data.
    #'
    #' @return List of centering values.
    centering_values = function() {
      private$.centering_values
    },

    #' @description
    #' Check if the model has been fitted.
    #'
    #' @return Logical indicating if model is fitted.
    is_fitted = function() {
      !is.null(private$.samples) # & !is.null(private$.dic_samples)
    },

    #' @description
    #' Create trace plots for MCMC chains using [bayesplot::mcmc_trace].
    #'
    #' @param ... Additional arguments passed to [bayesplot::mcmc_trace].
    #'
    #' @return A [ggplot2::ggplot] object showing trace plots.
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
    #' @param par Optional character vector of parameter names to plot.
    #' @return A [ggplot2::ggplot] object showing R-hat diagnostics.
    #' @importFrom ggplot2 ggplot geom_hline geom_point
    #' labs theme_minimal aes
    #' @importFrom coda gelman.diag
    mcmc_rhat = function(par = NULL) {
      private$.check_fitted()
      samples <- private$.samples
      rhat <- coda::gelman.diag(samples,
        autoburnin = FALSE
      )

      par_df <- data.frame(
        Parameter = names(rhat$psrf[, 1]),
        Rhat = rhat$psrf[, "Point est."]
      )

      if (!is.null(par)) {
				unknown_par <- which(!(par %in% par_df$Parameter))
        if (length(unknown_par) > 0) {
          stop("Parameter '", par[[unknown_par[[1]]]], "' not found in samples.")
        }
        par_df <- par_df[par_df$Parameter %in% par, , drop = FALSE]
      }

      ggplot2::ggplot(
        par_df,
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
    #' @return A [ggplot2::ggplot] object showing autocorrelation plots.
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
    #' @return A [ggplot2::ggplot] object showing posterior distributions.
    #' @seealso [bayesplot::mcmc_areas]
    #' @importFrom bayesplot mcmc_areas
    plot_posteriors = function(prob = 0.9, ...) {
      private$.check_fitted()
      samples <- private$.samples
      vn <- colnames(as.matrix(samples))
      keep <- vn[!grepl("^fc_", vn)]
      samples <- samples[, keep, drop = FALSE]
      bayesplot::mcmc_areas(samples, prob = prob, ...)
    },
    #' @description
    #' Calculate model performance metrics on known data
    #'
    #' This method evaluates the fitted model's performance by comparing
    #' predictions to known costs and computing mean absolute error (MAE),
    #' root mean square error (RMSE), Bayesian R2, credible interval coverage and
    #' median credible interval width. By default the model training
    #' data is used, but a different dataset can also be provided.
    #'
    #' @param scale One of "log" or "natural". Default "log".
    #' @param conditional Logical. If TRUE, returns conditional performance.
    #' If FALSE, returns performance marginalised over facility random effects.
    #' Default FALSE.
    #' @param by_country Logical. If TRUE, returns metrics calculated
    #' on country sub-groups. Default FALSE.
    #' @param dat Optional data prepared using [prepare_covariates()].
    #' If provided, uses this data for performance calculation instead of
    #' the training data.
    #' @return A data.frame with performance metrics:
    #' \itemize{
    #'  \item country: Only present if by_country = TRUE
    #'   \item mae: Mean Absolute Error between observed and predicted values
    #'   \item rmse: Root Mean Square Error between observed and predicted values
    #'   \item bayesian_r2: Mean Bayesian R2 estimate.
    #'   \item ci_coverage: Proportion of observations within 95% credible intervals
    #'   \item median_ci: The median width of 95% credible intervals
    #' }
    #'
    #' @examples
    #' model <- unitcost()
    #' model$performance()
    performance = function(scale = "natural",
                           conditional = FALSE,
                           by_country = FALSE,
                           dat = NULL) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      private$.check_fitted()
      if (is.null(dat)) {
        dat <- private$.training_data
      } else {
        private$.validate_data(dat, include_target = TRUE)
      }
      preds <- private$.predict(dat, conditional = conditional)

      if (scale == "natural") {
        preds <- exp(preds)
      }

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

      resid_mat <- preds - rep(observed_values, each = nrow(preds))

      if (by_country) {
        countries <- private$.countries

        results_df <- results_df |>
          dplyr::group_by(country)

        bayesian_r2 <- c()

        for (i in seq_along(countries)) {
          ctry_indices <- which(dat$fc_country == countries[[i]])
          var_yhat <- apply(preds[, ctry_indices], 1, var)
          var_resid <- apply(resid_mat[, ctry_indices], 1, var)
          bayesian_r2[[i]] <- mean(var_yhat / (var_yhat + var_resid))
        }

        bayesian_r2 <- unlist(bayesian_r2)
      } else {
        var_yhat <- apply(preds, 1, var)
        var_resid <- apply(resid_mat, 1, var)
        bayesian_r2 <- mean(var_yhat / (var_yhat + var_resid))
      }

      # Calculate performance metrics
      performance_metrics <- results_df |>
        dplyr::summarise(
          mae = mean(abs(.data$observed - .data$mean)),
          rmse = sqrt(mean((.data$observed - .data$mean)^2)),
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper),
          median_ci = median(.data$upper - .data$lower),
          .groups = "drop"
        )

      cbind(performance_metrics, bayesian_r2)
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
      preds <- private$.predict(dat, conditional = TRUE)

      pred <- data.frame(
        mean = apply(preds, 2, mean),
        lower = apply(preds, 2, quantile, probs = 0.025),
        upper = apply(preds, 2, quantile, probs = 0.975)
      )

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
    #' @param conditional Logical. If TRUE, shows full conditional fit. If FALSE,
    #' shows marginal fit. Default TRUE.
    #' @param include_ci Logical. Whether to show prediction intervals as
    #' error bars. Default TRUE.
    #' @param color_by_country Logical. Whether to color points by country.
    #' Default TRUE.
    #' @return A [ggplot2::ggplot] object showing observed vs predicted values.
    #'
    #' @examples
    #' \dontrun{
    #' model <- JAGSModel$new()
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
                        conditional = TRUE,
                        include_ci = TRUE,
                        color_by_country = TRUE) {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural"),
        "include_ci must be logical" = is.logical(include_ci),
        "conditional must be logical" = is.logical(conditional),
        "color_by_country must be logical" = is.logical(color_by_country)
      )

      private$.check_fitted()
      dat <- private$.training_data

      preds <- private$.predict(dat, conditional = conditional)

      if (scale == "natural") {
        preds <- exp(preds)
      }

      pred <- data.frame(
        mean = apply(preds, 2, mean),
        lower = apply(preds, 2, quantile, probs = 0.025),
        upper = apply(preds, 2, quantile, probs = 0.975)
      )

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
      n_fc <- length(unique(dat$fc_code))

      # Create fold assignments
      fold_ids <- cbind(
        fc_code = unique(dat$fc_code),
        fold = sample(rep(1:k_folds, length.out = n_fc))
      )

      dat <- dat |> dplyr::left_join(
        as.data.frame(fold_ids),
        by = "fc_code"
      )

      cv_predictions <- list()
      all_predictions <- list()

      for (fold in 1:k_folds) {
        message("Processing fold ", fold, " of ", k_folds)

        # Split data
        train_data <- dat[dat$fold != fold, ]
        test_data <- dat[dat$fold == fold, ]

        # Create temporary model for this fold
        temp_model <- JAGSModel$new(
          dat = train_data,
          covariates = private$.covariates,
          target = private$.target,
          priors = private$.priors
        )

        # Fit model on training fold
        temp_model$fit(seed = seed, ...)

        # Filter test data to modelled output types
        test_data <- test_data |>
          dplyr::filter(output %in% unique(train_data$output))

        # Make full predictions on test fold (not summarised)
        fold_preds_full <- temp_model$predict(
          test_data,
          scale = scale,
          summarised = FALSE
        )

        # Store full predictions for Bayesian R-squared calculation
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

      # Combine all predictions for Bayesian R-squared calculation
      all_preds_matrix <- do.call(cbind, all_predictions)
      observed_values <- results_df$observed

      # Calculate Bayesian R-squared
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

        # Create temporary model for this country
        temp_model <- JAGSModel$new(
          dat = train_data,
          covariates = private$.covariates,
          target = private$.target,
          priors = private$.priors
        )
        # Filter test data to modelled output types
        test_data <- test_data |>
          dplyr::filter(output %in% unique(train_data$output))

        all_models[[i]] <- temp_model

        # Fit model on training data
        temp_model$fit(seed = seed, ...)

        # Make full predictions on test country (not summarised)
        fold_preds_full <- temp_model$predict(
          test_data,
          scale = scale,
          summarised = FALSE
        )

        # Store full predictions for Bayesian R-squared calculation
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

      # Combine all predictions for Bayesian R-squared calculation
      all_preds_matrix <- do.call(cbind, all_predictions)
      observed_values <- results_df$observed

      # Calculate Bayesian R-squared
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
          ci_coverage = mean(.data$observed >= .data$lower &
            .data$observed <= .data$upper),
          median_ci = median(.data$upper - .data$lower),
          bayesian_r2 = bayesian_r2
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
    #' @param dat Model input data prepared using [prepare_covariates()].
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
    #' dat <- prepare_covariates(list(x1 = 1, x2 = 2), model)
    #' lambda <- c(10000, 20000, 30000)
    #' model$evpi(dat, lambda)
    #' }
    #'
    #' @seealso [predict()]
    #' @export
    evpi = function(dat, lambda, n_outputs = 1) {
      private$.validate_data(dat)

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
    #' @param dat Model input data prepared using [prepare_covariates()].
    #' @param n_outputs Number of outputs at each facility. Should either be
    #' length 1, for the same number of outputs at each facility, or should
    #' have an entry for each row in `dat`.
    #'
    #' @return Posterior distribution of predicted total cost, as a vector
    #' @export
    predict_total = function(dat, n_outputs) {
      private$.validate_data(dat)
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
      smat <- as.matrix(private$.samples)
      fc_cols <- which(grepl("^fc_", colnames(smat)))
      if (any(fc_cols)) {
        # Remove facility random effects from parameter summary
        smat <- smat[, -fc_cols]
      }
      bayestestR::describe_posterior(as.data.frame(smat),
        centrality = centrality,
        ci = ci,
        ci_method = ci_method,
        test = test, ...
      )
    },
    #' @description Retrieve penalized deviance statistics.
    #'
    #' This method returns cached penalized deviance statistics created
    #' at the time of model fitting using.
    #' @param summarised Logical. If TRUE (default) return the
    #' total DIC as a single numeric value. If FALSE, return all
    #' DIC samples.
    #' @return If summarised = TRUE, a numeric scalar of the total DIC.
    #' If summarised = FALSE, an object of class "dic"; see [rjags::dic.samples()].
    #' @examples
    #' mod <- unitcost()
    #' mod$mcmc_DIC()
    #' @export
    mcmc_DIC = function(summarised = TRUE) {
      private$.check_fitted()
      dic_samples <- private$.DIC
      if (summarised) {
        return(sum(dic_samples$deviance) + sum(dic_samples$penalty))
      } else {
        return(dic_samples)
      }
    }
  ),
  private = list(
    .model = NULL,
    .DIC = NULL,
    .params = NULL,
    .centering_values = NULL,
    .covariates = NULL,
    .facilities = NULL,
    .countries = NULL,
    .outputs = NULL,
    .training_data = NULL,
    .fitted_data = NULL,
    .target = NULL,
    .priors = NULL,
    .samples = NULL,
    .logical_to_numeric = function(x) {
      logical_cols <- sapply(x, is.logical)
      if (any(logical_cols)) {
        x[, logical_cols] <- lapply(x[, logical_cols, drop = FALSE], as.numeric)
      }
      x
    },
    .net_benefit = function(lambda, cost) {
      lambda - cost
    },
    .validate_data = function(dat, include_target = FALSE) {
      stopifnot(
        "'dat' must be prepared using prepare_covariates" =
          inherits(dat, "capturetbdata")
      )
      required_cols <- c(
        private$.covariates,
        "fc_country"
      )

      if (include_target) {
        required_cols <- c(required_cols, private$.target)
      }
      if (length(unique(private$.outputs)) > 1) {
        required_cols <- c(required_cols, "output")
      }
      missing_cols <- setdiff(required_cols, names(dat))
      if (length(missing_cols) > 0) {
        stop(
          "Missing required columns in data: ",
          paste(missing_cols, collapse = ", ")
        )
      }

      if ("output" %in% names(dat) && any(!dat$output %in% private$.outputs)) {
        warning(sprintf(
          "Unknown output types: %s",
          paste(setdiff(
            unique(dat$output),
            private$.outputs
          ), collapse = ", ")
        ))
      }

      centering_values <- private$.centering_values
      for (cov in private$.covariates) {
        if (is.numeric(dat[[cov]]) && !is.null(centering_values[[cov]])) {
          if (attr(dat[[cov]], "scaled:center") != centering_values[[cov]]) {
            stop(sprintf("'dat' has not been prepared for this model: %s is not centered", cov))
          }
        }
      }
    },
    .check_fitted = function() {
      if (!self$is_fitted()) {
        stop(
          "Model must be fitted first. ",
          "Call $fit() first."
        )
      }
    },
    .predict = function(dat, conditional = FALSE) {
      output_effects <- private$.model == "outputeffects.model"
      smat <- do.call(rbind, lapply(private$.samples, as.matrix))

      # shared intercept
      alpha <- smat[, "alpha"]

      # population standard deviations
      sig <- smat[, "sigma"]
      sig_country <- smat[, "sigma_c"]

      if (output_effects) {
        # facility and output effect standard deviations
        sig_fc <- smat[, "sigma_f"]
        sig_output <- smat[, "sigma_v"]

        # known output effects
        if (length(private$.outputs) == 1) {
          output_cols <- "output_effect"
        } else {
          output_cols <- paste0("output_effect[", as.numeric(private$.outputs), "]")
        }

        outputs <- smat[, output_cols, drop = FALSE]

        # use sig_output to generate output effects for unknown output
        output_new <- rnorm(length(alpha), 0, sig_output)
        outputs <- cbind(outputs, output_new)

        x_output <- dat[, "output", drop = FALSE]
        x_output_matrix <- as.data.frame(lapply(
          private$.outputs,
          function(output) as.character(output) == x_output
        ))
        x_output_matrix[, ] <- lapply(
          x_output_matrix[, , drop = FALSE],
          as.numeric
        )
        x_output_matrix[, length(private$.outputs) + 1] <- 0
        x_output_matrix[
          which(rowSums(x_output_matrix) == 0),
          length(private$.outputs) + 1
        ] <- 1
      }

      # country effects
      country_cols <- paste0("country_effect[", as.numeric(private$.countries), "]")
      countries <- smat[, country_cols, drop = FALSE]

      # use sig_country to generate country effects for unseen countries
      country_new <- rnorm(length(alpha), 0, sig_country)
      countries <- cbind(countries, country_new)

      if (length(private$.covariates) == 1) {
        beta_cols <- "beta"
      } else {
        beta_cols <- paste0("beta[", seq_along(private$.covariates), "]")
      }
      betas <- smat[, beta_cols, drop = FALSE]

      x <- as.matrix(private$.logical_to_numeric(
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

      pred_means <- alpha + betas %*% t(x) +
        countries %*% t(x_country_matrix)

      S <- length(sig)
      N <- ncol(pred_means)

      if (conditional) {
        if (!output_effects) {
          warning("conditional = TRUE has no effect when there is only one output type")
        } else {
          if (!"fc_code" %in% names(dat)) {
            stop("Column 'fc_code' required in data for full conditional predictions.")
          }
          fc_cols <- paste0("fc_effect[", as.numeric(private$.facilities), "]")
          fc <- smat[, fc_cols, drop = FALSE]

          x_fc <- dat[, "fc_code", drop = FALSE]
          x_fc_matrix <- as.data.frame(lapply(
            private$.facilities,
            function(code) as.character(code) == x_fc
          ))
          x_fc_matrix[, ] <- lapply(
            x_fc_matrix[, , drop = FALSE],
            as.numeric
          )
          pred_means <- pred_means + fc %*% t(x_fc_matrix) +
            outputs %*% t(x_output_matrix)
        }
      } else if (output_effects) {
        epsilon_fc <- matrix(rnorm(S * N), nrow = S)
        pred_means <- pred_means + epsilon_fc * sig_fc +
          outputs %*% t(x_output_matrix)
      }

      epsilon <- matrix(rnorm(S * N), nrow = S)

      preds <- pred_means + epsilon * sig
      preds
    }
  )
)
