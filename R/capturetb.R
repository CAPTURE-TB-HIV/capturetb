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
  data <- get_data("OP treatment visit") |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(capturetb_covariates()),
        ~ !is.na(.) & !is.nan(.) & is.finite(.)
      )
    ) |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  mod <- MixedEffects$new(data)
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
  data <- get_data("OP treatment visit") |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(capturetb_covariates()),
        ~ !is.na(.) & !is.nan(.) & is.finite(.)
      )
    ) |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  mod <- MixedEffects$new(data, target = "USD_unitcost_ohd")
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
  data <- get_data("OP treatment visit") |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(capturetb_covariates()),
        ~ !is.na(.) & !is.nan(.) & is.finite(.)
      )
    ) |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  mod <- MixedEffects$new(data, target = "USD_unitcost_fixed")
  mod$.__enclos_env__$private$.samples <- samples
  mod
}
