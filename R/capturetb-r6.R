#' capturetb R6 Class
#'
#' An R6 class for fitting and predicting with the CaptureTB cost model.
#'
#' @description
#' This class encapsulates the CaptureTB model functionality, providing
#' methods to fit the JAGS model and generate predictions. The model uses
#' a hierarchical structure with country-specific intercepts and shared
#' covariate effects.
#'
#' @examples
#' \dontrun{
#' # Create a new CaptureTB model instance
#' model <- capturetb$new()
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
capturetb <- R6::R6Class("capturetb",
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
      stopifnot(
        "dat must be a data.frame" = is.data.frame(dat),
        "covariates must be a character vector" = is.character(covariates),
        "target must be a character" = is.character(target),
        "target must be a scalar" = length(target) == 1,
        "priors must be 'capturetbpriors'" = inherits(priors, "capturetbpriors")
      )

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
      private$covariates <- covariates
      private$training_data <- dat
      private$target <- target
      private$priors <- priors
      private$countries <- as.factor(unique(dat$fc_country))
      private$samples <- NULL
    },

    #' @description
    #' Fit the CaptureTB model using JAGS.
    #'
    #' @param n.chains Integer. Number of MCMC chains. Default is 3.
    #' @param n.iter Integer. Number of total iterations per chain.
    #' Default is 100000.
    #' @param n.burnin Integer. Number of burn-in iterations. Default is 2000.
    #' @param n.adapt Integer. Number of adaptation iterations. Default is 2000.
    #' @param n.thin Integer. Thinning interval. Default is 10.
    #' @param ... Additional arguments passed to rjags::jags.model().
    #'
    #' @return Self (invisibly) for method chaining.
    fit = function(n.chains = 3,
                   n.iter = 100000,
                   n.burnin = 2000,
                   n.adapt = 2000,
                   n.thin = 10,
                   ...) {
      if (!requireNamespace("rjags", quietly = TRUE)) {
        stop("Package 'rjags' is required but not installed.")
      }
      if (!requireNamespace("coda", quietly = TRUE)) {
        stop("Package 'coda' is required but not installed.")
      }

      # if there are multiple facilities, take one at random
      dat <- private$training_data |>
        dplyr::group_by(.data$fc_code) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(private$covariates),
            ~ !is.na(.) & !is.nan(.) & is.finite(.)
          )
        )

      model_file <- system.file("jags", "model.model", package = "capturetb")

      x <- dat[, private$covariates]
      logical_cols <- sapply(x, is.logical)
      if (any(logical_cols)) {
        x[, logical_cols] <- lapply(x[, logical_cols, drop = FALSE], as.numeric)
      }

      jags_data <- list(
        N = nrow(dat),
        K = length(private$covariates),
        x = x,
        log_cost = log(dat[[private$target]]),
        NC = length(unique(dat$fc_country)),
        country = as.numeric(as.factor(dat$fc_country))
      )

      jags_data <- c(jags_data, as.list(private$priors))
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

      # Store samples
      private$samples <- samples
      private$fitted_data <- dat

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
    #' @return Matrix of predicted costs.
    #' Rows = simulations, columns = input rows.
    predict = function(dat, scale = "log") {
      stopifnot(
        "scale must be 'log' or 'natural'" =
          (scale == "log" || scale == "natural")
      )

      if (is.null(private$samples)) {
        stop("Model must be fitted before making predictions. Call $fit() first.")
      }

      # Validate prediction data
      stopifnot("dat must be a data.frame" = is.data.frame(dat))

      missing_covs <- setdiff(private$covariates, names(dat))
      if (length(missing_covs) > 0) {
        stop(
          "Missing covariates in prediction data: ",
          paste(missing_covs, collapse = ", ")
        )
      }

      if (!"fc_country" %in% names(dat)) {
        stop("Column 'fc_country' required in prediction data")
      }

      # Prediction logic (same as original function)
      smat <- as.matrix(private$samples)

      # known country intercepts
      alpha_cols <- paste0("alpha[", seq_along(private$countries), "]")
      alphas <- smat[, alpha_cols, drop = FALSE]

      # if country not known, or not in training data
      # generate intercept using hyper-parameters
      mu <- smat[, "mu_alpha"] # hyper-means
      sig <- smat[, "sigma_alpha"] # hyper-sds

      alpha_new <- rnorm(length(mu), mu, sig)
      alphas <- cbind(alphas, alpha_new)

      beta_cols <- paste0("beta[", seq_along(private$covariates), "]")
      betas <- smat[, beta_cols, drop = FALSE]

      x <- as.matrix(dat[, private$covariates, drop = FALSE])
      x_country <- dat[, "fc_country", drop = FALSE]
      x_country_matrix <- as.data.frame(lapply(
        private$countries,
        function(country) as.character(country) == x_country
      ))
      x_country_matrix[, ] <- lapply(
        x_country_matrix[, , drop = FALSE],
        as.numeric
      )
      x_country_matrix[, length(private$countries) + 1] <- 0
      x_country_matrix[
        which(rowSums(x_country_matrix) == 0),
        length(private$countries) + 1
      ] <- 1

      preds <- alphas %*% t(x_country_matrix) + betas %*% t(x)
      if (scale == "natural") {
        return(exp(preds))
      } else {
        return(preds)
      }
    },

    #' @description
    #' Get the training data.
    #'
    #' @return data.frame.
    get_training_data = function() {
      private$training_data
    },

    #' @description
    #' Get the fitted MCMC samples.
    #'
    #' @return coda::mcmc.list object or NULL if not fitted.
    get_samples = function() {
      private$samples
    },

    #' @description
    #' Get the covariates used in the model.
    #'
    #' @return Character vector of covariate names.
    get_covariates = function() {
      private$covariates
    },

    #' @description
    #' Get the countries from the training data.
    #'
    #' @return Character vector of country names.
    get_countries = function() {
      private$countries
    },

    #' @description
    #' Get the priors used in the model.
    #'
    #' @return List of prior parameters of class 'capturetbpriors'.
    get_priors = function() {
      private$priors
    },

    #' @description
    #' Check if the model has been fitted.
    #'
    #' @return Logical indicating if model is fitted.
    is_fitted = function() {
      !is.null(private$samples)
    }
  ),
  private = list(
    covariates = NULL,
    countries = NULL,
    training_data = NULL,
    fitted_data = NULL,
    target = NULL,
    priors = NULL,
    samples = NULL
  )
)
