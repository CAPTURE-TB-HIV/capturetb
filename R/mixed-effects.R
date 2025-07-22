#' MixedEffects R6 Class
#'
#' An R6 class for fitting and predicting costs using a mixed effects model.
#'
#' @description
#' This class allows the user to fit and use a mixed effects model with
#' intercepts that vary by country, facility and output type as well as fixed
#' covariate effects. It is used for the comparison of different model
#' structures as in the `vignette("03_model-comparisons", package = "capturetb")`
#' vignette, and is the class underlying the [`unitcost`] model.
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang .data
MixedEffects <- R6::R6Class("MixedEffects",
  inherit = JAGSModel,
  public = list(
    #' @description
    #' Initialize a new model instance.
    #'
    #' @param dat Data.frame. Training data.
    #' @param covariates Character vector. Names of covariate columns.
    #' @param target Character. Name of the target variable.
    #' @param priors List of class "capturetbpriors". Should be created using
    #' [`capturetb_priors()`]].
    initialize = function(dat,
                          covariates,
                          target,
                          priors = NULL) {
      params <- c(
        "alpha",
        "beta",
        "sigma",
        "sigma_country",
        "country_effect"
      )
			stopifnot("dat must be a data.frame" = is.data.frame(dat))
      if (length(unique(dat[["output"]])) > 1) {
        model <- "outputeffects.model"
        params <- c(
          params,
          "sigma_fc",
          "sigma_output",
          "output_effect",
          "fc_effect"
        )
      } else {
        model <- "singleoutput.model"
      }
      super$initialize(
        dat,
        covariates,
        target,
        priors,
        model,
        params
      )
    }
  ),
  private = list(
    .predict = function(dat, conditional = FALSE) {
      output_effects <- private$.model == "outputeffects.model"
      if (output_effects) {
        if (!"output" %in% names(dat)) {
          stop("Column 'output' required in prediction data")
        }
        unknown_outputs <- unique(dat[["output"]][!(dat[["output"]] %in% private$.outputs)])
        if (length(unknown_outputs) > 0) {
          warning(
            "Unknown output types in prediction data: ",
            paste(unknown_outputs, collapse = ", ")
          )
        }
      }

      smat <- do.call(rbind, lapply(private$.samples, as.matrix))

      # shared intercept
      alpha <- smat[, "alpha"]

      # population standard deviations
      sig <- smat[, "sigma"]
      sig_country <- smat[, "sigma_country"]

      if (output_effects) {
        # facility and output effect standard deviations
        sig_fc <- smat[, "sigma_fc"]
        sig_output <- smat[, "sigma_output"]

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
