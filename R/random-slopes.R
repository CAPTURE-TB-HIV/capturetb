#' RandomSlopes R6 Class
#'
#' An R6 class for fitting and predicting costs using a model with
#' random slopes.
#'
#' @description
#' This class allows the user to fit and use a random slopes model with
#' country, facility and output specific intercepts and country specific
#' covariate effects.
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
                          priors = NULL) {
      super$initialize(dat, covariates, target, priors, "randomslopes.model")
    }
  ),
  private = list(
    .predict = function(dat) {
      smat <- do.call(rbind, lapply(private$.samples, as.matrix))

      # shared intercept
      alpha <- smat[, "alpha"]

      # population standard deviations
      sig <- smat[, "sigma"]
      sig_fc <- smat[, "sigma_fc"]
      sig_country <- smat[, "sigma_country"]

      # output intercepts
      output_cols <- paste0("output_effect[", as.numeric(private$.outputs), "]")
      outputs <- smat[, output_cols, drop = FALSE]

      # country effects
      country_cols <- paste0("country_effect[", as.numeric(private$.countries), "]")
      countries <- smat[, country_cols, drop = FALSE]

      # use sig_country to generate country effects for unseen countries
      country_new <- rnorm(length(alpha), 0, sig_country)
      countries <- cbind(countries, country_new)

      # Extract country-specific beta coefficients
      # beta[k, j] where k is covariate index, j is country index
      n_countries_total <- length(private$.countries) + 1 # +1 for new countries
      beta_arrays <- array(NA, dim = c(nrow(smat), length(private$.covariates), n_countries_total))

      # Extract known country beta coefficients
      for (k in seq_along(private$.covariates)) {
        for (j in seq_along(private$.countries)) {
          beta_cols <- paste0("beta[", k, ",", j, "]")
          beta_arrays[, k, j] <- smat[, beta_cols]
        }
      }

      # Generate new beta coefficients for unknown countries using hyper-parameters
      if (length(private$.covariates) == 1) {
        mu_beta <- smat[, "mu_beta", drop = FALSE]
        sigma_beta <- smat[, "sigma_beta", drop = FALSE]
      } else {
        mu_beta <- smat[, paste0("mu_beta[", seq_along(private$.covariates), "]"), drop = FALSE]
        sigma_beta <- smat[, paste0("sigma_beta[", seq_along(private$.covariates), "]"), drop = FALSE]
      }
      for (k in seq_along(private$.covariates)) {
        beta_arrays[, k, n_countries_total] <- rnorm(nrow(smat), mu_beta[, k], sigma_beta[, k])
      }

      betas <- beta_arrays


      S <- nrow(smat)
      N <- nrow(dat)

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

      x_output <- dat[, "output", drop = FALSE]
      x_output_matrix <- as.data.frame(lapply(
        private$.outputs,
        function(output) as.character(output) == x_output
      ))
      x_output_matrix[, ] <- lapply(
        x_output_matrix[, , drop = FALSE],
        as.numeric
      )

      pred_means <- matrix(0, nrow = S, ncol = N)

      # Calculate predictions using country-specific beta coefficients
      for (i in 1:N) {
        # Determine which country column to use (known countries + 1 for new)
        country_idx <- which(x_country_matrix[i, ] == 1)

        # Get beta coefficients for this country
        beta_i <- betas[, , country_idx]

        # Calculate prediction: alpha + sum(x * beta)
        x_rep <- matrix(x[i, ],
          nrow = S, ncol = length(private$.covariates),
          byrow = TRUE
        )
        pred_means[, i] <- alpha + rowSums(beta_i * x_rep)
      }

      pred_means <- pred_means +
        outputs %*% t(x_output_matrix) +
        countries %*% t(x_country_matrix)

      epsilon <- matrix(rnorm(S * N), nrow = S)
      preds <- pred_means + epsilon * sig
      preds
    }
  )
)
