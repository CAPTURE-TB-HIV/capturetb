#' FixedEffects R6 Class
#'
#' An R6 class for fitting and predicting costs using a fixed effects model.
#'
#' @description
#' This class allows the user to fit and use a fixed effects model with
#' single intercept and fixed covariate effects. Its primary purpose is for 
#' the comparison of different model structures; see the 
#' `vignette("03_model-comparisons", package = "capturetb")` vignette.
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang .data
FixedEffects <- R6::R6Class("FixedEffects",
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
    #' [`capturetb_priors()`].
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = NULL) {
      super$initialize(dat, covariates, target, priors, "fixedeffects.model")
    }
  ),
  private = list(
    .predict = function(dat) {
      smat <- do.call(rbind, lapply(private$.samples, as.matrix))
      if (length(private$.covariates) == 1) {
        beta_cols <- "beta"
      } else {
        beta_cols <- paste0("beta[", seq_along(private$.covariates), "]")
      }
			
      betas <- smat[, beta_cols, drop = FALSE]

      alpha <- smat[, "alpha", drop = FALSE]
      sig <- smat[, "sigma"]

      x <- as.matrix(private$.numeric_to_logical(
        dat[, private$.covariates, drop = FALSE]
      ))

      pred_means <- alpha[, rep(1, times = nrow(dat))] + betas %*% t(x)

      S <- length(sig)
      N <- ncol(pred_means)
      epsilon <- matrix(rnorm(S * N), nrow = S)
      preds <- pred_means + epsilon * sig
      preds
    }
  )
)
