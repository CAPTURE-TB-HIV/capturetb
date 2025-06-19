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

      # if there are multiple facilities, take one at random
      dat <- dat |>
        dplyr::group_by(.data$fc_code) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(private$.covariates),
            ~ !is.na(.) & !is.nan(.) & is.finite(.) # and exclude missing data
          )
        )

      private$.training_data <- dat
      private$.target <- target
      private$.priors <- priors
      private$.countries <- as.factor(unique(dat$fc_country))
      private$.samples <- NULL
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
    #' @param seed Integer. Used to seed both the R and JAGS random generators.
    #' @param ... Additional arguments passed to rjags::jags.model().
    #'
    #' @return Self (invisibly) for method chaining.
    fit = function(n.chains = 3,
                   n.iter = 1000000,
                   n.burnin = 5000,
                   n.adapt = 5000,
                   n.thin = 100,
                   seed = 123,
                   ...) {
      if (!requireNamespace("rjags", quietly = TRUE)) {
        stop("Package 'rjags' is required but not installed.")
      }
      if (!requireNamespace("coda", quietly = TRUE)) {
        stop("Package 'coda' is required but not installed.")
      }

      set.seed(seed)

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
      jags_mod <- rjags::jags.model(model_file,
        data = jags_data,
        n.chains = n.chains,
        n.adapt = n.adapt,
        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seed),
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
    #' @return Matrix of predicted costs.
    #' Rows = simulations, columns = input rows.
    predict = function(dat, scale = "log") {
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
        return(exp(preds))
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
    }
  )
)
