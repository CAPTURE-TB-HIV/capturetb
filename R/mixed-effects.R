#' MixedEffects R6 Class
#'
#' An R6 class for fitting and predicting costs using a mixed effects model.
#'
#' @description
#' This class allows the user to fit and use a mixed effects model with
#' intercepts that vary by country and fixed covariate effects. It is used
#' for the comparison of different model structures as in the
#' `vignette("03_model-comparisons", package = "capturetb")` vignette, and
#' is the class used to generate the [`unitcost`] model.
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang .data
MixedEffects <- R6::R6Class("MixedEffects",
  inherit = JAGSModel,
  public = list(
    #' @description
    #' Initialize a new model instance.
    #'
    #' @param dat Data.frame. Training data. Default loads "OP treatment visit"
    #' from the ValueTB dataset installed with this package.
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class "capturetbpriors". Should be created using
    #' [`capturetb_priors()`]].
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = NULL) {
      super$initialize(dat, covariates, target, priors, "mixedeffects.model")
    }
  ),
  private = list(
    .predict = function(dat) {
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

      if (length(private$.covariates) == 1) {
        beta_cols <- "beta"
      } else {
        beta_cols <- paste0("beta[", seq_along(private$.covariates), "]")
      }
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
      preds
    }
  )
)
