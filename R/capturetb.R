priors <- function() {
  capturetb_priors(
    beta.mean = rep(0, 5),
    beta.precision = rep(0.01, 5)
  )
}

#' @importFrom stats median
cleaned_data <- function() {
  # where facilities have multiple rows, take the median of the target variable
  get_data("OP treatment visit") |>
    dplyr::filter(is.na(.data$output_pop2) | .data$output_pop2 != "collecting meds") |>
    dplyr::group_by(
      .data$fc_code,
      dplyr::across(dplyr::all_of(capturetb_covariates())), .data$fc_country
    ) |>
    dplyr::summarise(
      USD_unitcost_total = median(.data$USD_unitcost_total, na.rm = TRUE),
      USD_unitcost_variable = median(.data$USD_unitcost_variable, na.rm = TRUE),
      USD_unitcost_fixed = median(.data$USD_unitcost_fixed, na.rm = TRUE),
      USD_unitcost_ohd = median(.data$USD_unitcost_ohd, na.rm = TRUE),
      .groups = "drop"
    )
}

#' CaptureTB outpatient treatment visit cost model
#'
#' This function loads a [`MixedEffects`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the total cost of a single outpatient treatment
#' visit at a given facility or facilities via the predict method.
#'
#' @return An object of class [`MixedEffects`].
#' @examples
#' mod <- unitcost()
#' new_data <- get_data("OP treatment visit")[1, ]
#' mod$predict(new_data, summarised = TRUE)
#' @seealso MixedEffects
#' @export
unitcost <- function() {
  samples <- readRDS(system.file("posterior_samples.rds",
    package = "capturetb"
  ))

  data <- cleaned_data()
  mod <- MixedEffects$new(data, priors = priors())
  mod$.__enclos_env__$private$.samples <- samples
  mod
}

#' CaptureTB outpatient treatment visit overhead costs model
#'
#' This function loads a [`MixedEffects`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the overhead costs per outpatient treatment visit
#' at a given facility or facilities via the predict method.
#'
#' @return An object of class [`MixedEffects`].
#' @examples
#' mod <- unitcost_ohd()
#' new_data <- get_data("OP treatment visit")[1, ]
#' mod$predict(new_data, summarised = TRUE)
#' @seealso MixedEffects
#' @export
unitcost_ohd <- function() {
  samples <- readRDS(system.file("posterior_samples_ohd.rds",
    package = "capturetb"
  ))

  data <- cleaned_data()
  mod <- MixedEffects$new(data, priors = priors(), target = "USD_unitcost_ohd")
  mod$.__enclos_env__$private$.samples <- samples
  mod
}


#' CaptureTB outpatient treatment visit fixed costs model
#'
#' This function loads a [`MixedEffects`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the fixed costs per outpatient treatment visit
#' at a given facility or facilities via the predict method.
#'
#' @return An object of class [`MixedEffects`].
#' @examples
#' mod <- unitcost_ohd()
#' new_data <- get_data("OP treatment visit")[1, ]
#' mod$predict(new_data, summarised = TRUE)
#' @seealso MixedEffects
#' @export
unitcost_fixed <- function() {
  samples <- readRDS(system.file("posterior_samples_fixed.rds",
    package = "capturetb"
  ))

  data <- cleaned_data()
  mod <- MixedEffects$new(data, priors = priors(), target = "USD_unitcost_fixed")
  mod$.__enclos_env__$private$.samples <- samples
  mod
}
