#' MixedEffects R6 Class
#'
#' An R6 class for fitting and predicting costs using a fixed effects model.
#'
#' @description
#' This class encapsulates the CaptureTB fixed effects model with
#' single intercept and fixed covariate effects, providing
#' methods to fit the JAGS model and generate predictions.
#'
#' @examples
#' \dontrun{
#' # Create a new FixedEffects model instance with default covariates and priors
#' model <- capturetb::FixedEffects$new()
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
    #' @param priors List of class 'capturetbpriors'. Should be created using
    #' \code{capturetb_priors}.
    initialize = function(dat = get_data("OP treatment visit"),
                          covariates = capturetb_covariates(),
                          target = "USD_unitcost_total",
                          priors = capturetb_priors()) {
      super$initialize(dat, covariates, target, priors, "fixedeffects.model")
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

      smat <- do.call(rbind, lapply(private$.samples, as.matrix))

      beta_cols <- paste0("beta[", seq_along(private$.covariates), "]")
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
