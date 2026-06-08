#' Constructs a list of weakly informative priors for each coefficient.
#' @param n_priors Number of coefficients to create priors for.
#' @keywords internal
uninformative_priors <- function(n_priors) {
  capturetb_priors(
    sigma_c.scale = 0.1,
    beta.mean = c(
      rep(0, n_priors)
    ),
    beta.precision = c(
      rep(0.01, n_priors)
    )
  )
}

#' Load and prepare the outpatient visit data for training
#' the unit cost models.
#' @param cost_type One of "ECON" or "FIN". If "ECON", model for
#' economic costs is returned. If "FIN", model for financial
#' costs is returned. Default "ECON".
#' @keywords internal
opvisit_data <- function(cost_type = "ECON") {
  stopifnot(
    "cost_type must be one of 'ECON' or 'FIN'" =
      cost_type %in% c("ECON", "FIN")
  )
  training_data <- get_data(
    cost_type = cost_type,
    output_group = "OP"
  )

  # Center numeric covariates
  training_data$logVisits <- scale(
    training_data$logVisits,
    center = TRUE, scale = FALSE
  )
  training_data$logVisitsPP_TB <- scale(
    training_data$logVisitsPP_TB,
    center = TRUE, scale = FALSE
  )

  training_data
}

#' CaptureTB outpatient visit cost model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the total cost of a single outpatient visit at
#' a given facility or facilities via the predict method.
#'
#' Note that some covariates are centered. The function
#' [prepare_covariates()] can be used to transform raw variables
#' using the correct centering values.
#' 
#' @return An object of class [`JAGSModel`].
#' @examples
#' mod <- unitcost()
#' new_data <- list(
#'   logVisits = 6.9,
#'   healthcentre = FALSE,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   fc_country = "Ethiopia",
#'   output = "op_treatmentvisit"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @export
unitcost <- function() {
  samples <- readRDS(system.file("econ",
    "posterior_samples.rds",
    package = "capturetb"
  ))
  dic <- readRDS(system.file("econ",
    "posterior_samples_dic.rds",
    package = "capturetb"
  ))

  covariates <- c(
    "public",
    "urban",
    "healthcentre",
    "primary",
    "secondary",
    "tertiary",
    "logVisits"
  )

  data <- opvisit_data()

  mod <- JAGSModel$new(data,
    target = "ID_unitcost_total",
    covariates = covariates,
    priors = uninformative_priors(length(covariates))
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}


#' Extended CaptureTB outpatient visit cost model with more covariates.
#'
#' This function loads a [`JAGSModel`] model object
#' using default covariates and priors. This can be used to
#' predict the total cost of a single outpatient visit at
#' a given facility or facilities via the predict method. This model
#' is not pre-fitted, so requires JAGS to be installed to fit and use.
#'
#' Note that numerical covariates are centered. When making predictions 
#' with the model, the function [prepare_covariates()] can be used 
#' to transform raw variables using the correct centering values.
#'
#' @param cost_type One of "ECON" or "FIN". If "ECON", model for
#' economic costs is returned. If "FIN", model for financial
#' costs is returned. Default "ECON".
#' @return An object of class [`JAGSModel`].
#' @examples
#' \dontrun{
#' mod <- unitcost_extended()
#' mod$fit()
#' }
#' @seealso JAGSModel
#' @export
unitcost_extended <- function(cost_type = "ECON") {
  stopifnot(
    "cost_type must be one of 'ECON' or 'FIN'" =
      cost_type %in% c("ECON", "FIN")
  )

  covariates <- c(
    "public",
    "urban",
    "healthcentre",
    "primary",
    "secondary",
    "tertiary",
    "logVisits",
    "logVisitsPP_TB",
    "log_p_bldgspace"
  )

  data <- opvisit_data(cost_type)

  JAGSModel$new(data,
    target = "ID_unitcost_total",
    covariates = covariates,
    priors = uninformative_priors(length(covariates))
  )
}

#' CaptureTB outpatient visit overhead costs model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the overhead costs per outpatient visit
#' at a given facility or facilities via the predict method.
#'
#' Note that some covariates are centered. The function
#' [prepare_covariates()] can be used to transform raw variables
#' using the correct centering values.
#'
#' @return An object of class [`JAGSModel`].
#' @examples
#' mod <- unitcost_ohd()
#' new_data <- list(
#'   logVisits = 6.9,
#'   healthcentre = FALSE,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   fc_country = "Ethiopia"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @export
unitcost_ohd <- function() {
  samples <- readRDS(system.file("econ",
    "posterior_samples_ohd.rds",
    package = "capturetb"
  ))
  dic <- readRDS(system.file("econ",
    "posterior_samples_dic_ohd.rds",
    package = "capturetb"
  ))

  data <- opvisit_data() |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::filter(!duplicated(.data$ID_unitcost_ohd)) |>
    dplyr::mutate(output = "op_visit") |>
    dplyr::ungroup()

  covariates <- c(
    "public",
    "urban",
    "healthcentre",
    "primary",
    "secondary",
    "tertiary",
    "logVisits"
  )

  mod <- JAGSModel$new(
    data,
    priors = uninformative_priors(length(covariates)),
    covariates = covariates,
    target = "ID_unitcost_ohd"
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}


#' CaptureTB outpatient visit fixed costs model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the fixed costs per outpatient visit at a
#' given facility or facilities via the predict method.
#'
#' Note that some covariates are centered. The function
#' [prepare_covariates()] can be used to transform raw variables
#' using the correct centering values.
#'
#' @return An object of class [`JAGSModel`].
#' @examples
#' mod <- unitcost_ohd()
#' new_data <- list(
#'   logVisits = 6.9,
#'   healthcentre = FALSE,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   fc_country = "Ethiopia"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @importFrom stats median
#' @export
unitcost_fixed <- function() {
  samples <- readRDS(system.file("econ",
    "posterior_samples_fixed.rds",
    package = "capturetb"
  ))
  dic <- readRDS(system.file("econ",
    "posterior_samples_dic_fixed.rds",
    package = "capturetb"
  ))

  data <- opvisit_data("ECON") |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::mutate(ID_unitcost_fixed = median(.data$ID_unitcost_fixed)) |>
    dplyr::filter(!duplicated(.data$ID_unitcost_fixed)) |>
    dplyr::mutate(output = "op_visit") |>
    dplyr::ungroup()

  covariates <- c(
    "public",
    "urban",
    "healthcentre",
    "primary",
    "secondary",
    "tertiary",
    "logVisits"
  )

  mod <- JAGSModel$new(
    data,
    priors = uninformative_priors(length(covariates)),
    covariates = covariates,
    target = "ID_unitcost_fixed"
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}

#' Extended CaptureTB outpatient visit fixed cost model with more covariates.
#'
#' This function loads a [`JAGSModel`] model object
#' using default covariates and priors. This can be used to
#' predict the total cost of a single outpatient visit at
#' a given facility or facilities via the predict method. This model
#' is not pre-fitted, so requires JAGS to be installed to fit and use.
#'
#' Note that numerical covariates are centered. When making predictions 
#' with the model, the function [prepare_covariates()] can be used 
#' to transform raw variables using the correct centering values.
#'
#' @param cost_type One of "ECON" or "FIN". If "ECON", model for
#' economic costs is returned. If "FIN", model for financial
#' costs is returned. Default "ECON".
#' @return An object of class [`JAGSModel`].
#' @examples
#' \dontrun{
#' mod <- unitcost_extended()
#' mod$fit()
#' }
#' @seealso JAGSModel
#' @export
unitcost_fixed_extended <- function(cost_type = "ECON") {
  stopifnot(
    "cost_type must be one of 'ECON' or 'FIN'" =
      cost_type %in% c("ECON", "FIN")
  )

  covariates <- c(
    "public",
    "urban",
    "healthcentre",
    "primary",
    "secondary",
    "tertiary",
    "logVisits",
    "logVisitsPP_TB",
    "log_p_bldgspace"
  )

  data <- opvisit_data(cost_type) |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::mutate(ID_unitcost_fixed = median(.data$ID_unitcost_fixed)) |>
    dplyr::filter(!duplicated(.data$ID_unitcost_fixed)) |>
    dplyr::mutate(output = "op_visit") |>
    dplyr::ungroup()

  JAGSModel$new(data,
    target = "ID_unitcost_total",
    covariates = covariates,
    priors = uninformative_priors(length(covariates))
  )
}
