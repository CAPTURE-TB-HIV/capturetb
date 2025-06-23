#' RandomSlopes R6 Class
#'
#' An R6 class for fitting and predicting costs using a model with 
#' random slopes.
#'
#' @description
#' This class allows the user to fit and use a random slopes model with
#' coyntry specific intercepts and country specific covariate effects.
#' Its primary purpose is for the comparison of different model structures
#' as per the `vignette("03_model-comparisons", package = "capturetb")`
#' vignette.
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang .data
RandomSlopes <- R6::R6Class("RandomSlopes",
  inherit = JAGSModel,
  public = list(
    #' @description
    #' Initialize a new model instance.
    #'
    #' @param dat Data.frame. Training data. Default loads "OP treatment visit"
    #' from the ValueTB dataset installed with this package.
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class 'capturetbpriors'. Should be created using
    #' [`capturetb_priors()`].
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = capturetb_priors()) {
      super$initialize(dat, covariates, target, priors, "randomslopes.model")
    },
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

      # Extract country-specific beta coefficients
      # beta[k, j] where k is covariate index, j is country index
      n_countries_total <- length(private$.countries) + 1  # +1 for new countries
      beta_arrays <- array(NA, dim = c(nrow(smat), length(private$.covariates), n_countries_total))
      
      # Extract known country beta coefficients
      for (k in seq_along(private$.covariates)) {
        for (j in seq_along(private$.countries)) {
          beta_col <- paste0("beta[", k, ",", j, "]")
          beta_arrays[, k, j] <- smat[, beta_col]
        }
      }
      
      # Generate new beta coefficients for unknown countries using hyper-parameters
      mu_beta <- smat[, paste0("mu_beta[", seq_along(private$.covariates), "]"), drop = FALSE]
      sigma_beta <- smat[, paste0("sigma_beta[", seq_along(private$.covariates), "]"), drop = FALSE]
      
      for (k in seq_along(private$.covariates)) {
        beta_arrays[, k, n_countries_total] <- rnorm(nrow(smat), mu_beta[, k], sigma_beta[, k])
      }
      
      betas <- beta_arrays

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

      # Calculate predictions using country-specific beta coefficients
      S <- nrow(smat)
      N <- nrow(dat)
      pred_means <- matrix(0, nrow = S, ncol = N)

      for (i in 1:N) {
        # Determine which country column to use (known countries + 1 for new)
        country_idx <- which(x_country_matrix[i, ] == 1)

        # Get alpha for this country
        alpha_i <- alphas[, country_idx]

        # Get beta coefficients for this country
        beta_i <- betas[, , country_idx]

        # Calculate prediction: alpha + sum(x * beta)
        x_rep <- matrix(x[i, ], nrow = S, ncol = length(private$.covariates),
                        byrow = TRUE)
        pred_means[, i] <- alpha_i + rowSums(beta_i * x_rep)
      }

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
    }
  )
)
