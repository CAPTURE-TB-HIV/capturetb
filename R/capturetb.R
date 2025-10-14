#' Constructs a power prior for the effect of a covariate
#' based on a historic mean, upper and lower estimates, and a scaling factor.
#' @param mu Mean of the past estimate.
#' @param upper Upper bound of the 95% credible interval for the past estimate.
#' @param lower Lower bound of the 95% credible interval for the past estimate.
#' @param a0 Scaling factor for the power prior.
#' @keywords internal
power_prior <- function(mu, upper, lower, a0) {
  se <- (upper - lower) / (1.96 * 2)
  var_adjusted <- se^2 / a0
  precision <- 1 / var_adjusted
  list(mu = mu, precision = precision)
}

#' Constructs a power prior for the effect of public ownership on
#' outpatient visit costs, based on the WHO-CHOICE estimate.
#' @param a0 Scaling factor for the power prior.
#' @keywords internal
prior_public_ownership <- function(a0 = 0.1) {
  mu_private_old <- 0.0532
  var_private_old <- ((0.102 - 0.00479) / (1.96 * 2))^2

  mu_public_old <- -0.290
  var_pub_old <- ((-0.249 + 0.330) / (1.96 * 2))^2

  # WHO-CHOICE reports that 0.12 of facilities in ther data are
  # private and 0.75 are public.
  w_private <- 0.12 / (1 - 0.75)

  # WHO has coefficients for public and private relative
  # to a baseline of NGO/faith-based facilities.
  # We have very few of the latter so want to estimate
  # the effect of public onwership relative to a baseline of
  # private or NGO/faith-based.
  # We therefore adjust the old coefficient estimate to account
  # for the new baseline
  mu_public_adjusted <- mu_public_old - w_private * mu_private_old
  var_pub_adjusted <- var_pub_old + w_private^2 * var_private_old

  # Now apply power prior adjustment
  var_adjusted <- var_pub_adjusted / a0
  list(mu = mu_public_adjusted, precision = 1 / var_adjusted)
}

#' Constructs a power prior for the effect of being
#' a primary hospital outpatient visit costs, based on the
#' WHO-CHOICE estimate.
#' @param a0 Scaling factor for the power prior.
#' @keywords internal
primary_prior <- function(a0 = 0.1) {
  # WHO has coefficients for health centres with beds (level 2)
  # relative to a baseline of health centres without beds.
  # We have very few of the latter so want to estimate
  # the effect of health system level onwership relative to a
  # baseline of all non-hospital facilities
  # We therefore adjust the old coefficient estimate to account
  # for the new baseline
  mu_level_2_old <- 0.208
  mu_level_3_old <- 0.304

  # WHO reports that 0.15 of facilities in their data are
  # level 2 (health centres with beds), 0.25 are level 3
  # and 0.08 are level 4 or 5.
  w_level_2 <- 0.15 / (1 - 0.25 - 0.08)

  var_level_2 <- ((0.271 - 0.144) / (1.96 * 2))^2
  var_level_3 <- ((0.395 - 0.293) / (1.96 * 2))^2

  mu_level_3 <- mu_level_3_old - w_level_2 * mu_level_2_old

  var_level_3 <- w_level_2^2 * var_level_2 + var_level_3

  # Apply power prior adjustment
  var_level_3_adjusted <- var_level_3 / a0
  list(mu = mu_level_3, precision = 1 / var_level_3_adjusted)
}

#' Constructs a power prior for the effect of being
#' a secondary hospital outpatient visit costs, based on the
#' WHO-CHOICE estimate.
#' @param a0 Scaling factor for the power prior.
#' @keywords internal
secondary_prior <- function(a0 = 0.1) {
  # WHO has coefficients for health centres with beds (level 2)
  # relative to a baseline of health centres without beds.
  # We have very few of the latter so want to estimate
  # the effect of health system level onwership relative to a
  # baseline of all non-hospital facilities
  # We therefore adjust the old coefficient estimate to account
  # for the new baseline
  mu_level_2_old <- 0.208
  mu_level_4_old <- 0.348

  # WHO reports that 0.15 of facilities in their data are
  # level 2 (health centres with beds), 0.25 are level 3
  # and 0.08 are level 4 or 5.
  w_level_2 <- 0.15 / (1 - 0.25 - 0.08)

  var_level_2 <- ((0.271 - 0.144) / (1.96 * 2))^2
  var_level_4 <- ((0.416 - 0.279) / (1.96 * 2))^2

  mu_level_4 <- mu_level_4_old - w_level_2 * mu_level_2_old

  var_level_4 <- w_level_2^2 * var_level_2 + var_level_4

  # Apply power prior adjustment
  var_level_4_adjusted <- var_level_4 / a0
  list(mu = mu_level_4, precision = 1 / var_level_4_adjusted)
}

#' Constructs a list of priors using power priors for urban and public ownership
#' and a weakly informative prior for the other coefficients.
#' @param n_weak_priors Number of additional weakly informative priors to include.
#' @keywords internal
opvisit_priors <- function(n_weak_priors) {
  a0 <- 0.1
  urban_prior <- power_prior(mu = 0.352, lower = 0.268, upper = 0.435, a0 = a0)
  public_prior <- prior_public_ownership(a0 = a0)
  primary <- primary_prior(a0 / 10)
  secondary <- secondary_prior(a0 / 10)
  tertiary <- secondary_prior(a0 / 10) # WHO assumes level 4 and 5 have same effect

  # use a moderately informative prior to regularise the country-level effects
  # as only 5 countries are included in the training data
  capturetb_priors(
    sigma_c.scale = 0.1,
    beta.mean = c(
      public_prior$mu, urban_prior$mu,
      primary$mu, secondary$mu, tertiary$mu,
      rep(0, n_weak_priors)
    ),
    beta.precision = c(
      public_prior$precision, urban_prior$precision,
      primary$precision, secondary$precision, tertiary$precision,
      rep(0.01, n_weak_priors)
    )
  )
}

#' Load and prepare the outpatient visit data for training
#' the unit cost models.
#' @keywords internal
opvisit_data <- function() {
  training_data <- get_data(output_group = "OP")
  n_services <- training_data |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::summarise(n_services = length(unique(.data$output)))

  training_data <- training_data |>
    dplyr::left_join(n_services, by = "fc_code")

  # Center numeric covariates
  training_data$n_services <- scale(
    training_data$n_services,
    center = TRUE, scale = FALSE
  )
  training_data$logVisits <- scale(
    training_data$logVisits,
    center = TRUE, scale = FALSE
  )
  training_data$logVisitsPP_TB <- scale(
    training_data$logVisitsPP_TB,
    center = TRUE, scale = FALSE
  )
  training_data$log_ID_p_bldgspace <- scale(
    training_data$log_ID_p_bldgspace,
    center = TRUE, scale = FALSE
  )
  training_data
}

#' CaptureTB outpatient treatment visit cost model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the total cost of a single outpatient treatment
#' visit at a given facility or facilities via the predict method.
#'
#' Note that some covariates are centered. The function
#' [prepare_covariates()] can be used to transform raw variables
#' using the correct centering values.
#'
#' @return An object of class [`JAGSModel`].
#' @examples
#' mod <- unitcost()
#' new_data <- list(
#'   log_ID_p_bldgspace = 1,
#'   logVisits = 6.9,
#'   logVisitsPP_TB = -1.29,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   n_services = 3,
#'   fc_country = "Ethiopia",
#'   output = "op_treatmentvisit"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @export
unitcost <- function() {
  samples <- readRDS(system.file("posterior_samples.rds",
    package = "capturetb"
  ))
  dic <- readRDS(system.file("posterior_samples_dic.rds",
    package = "capturetb"
  ))

  covariates <- c(
    "public",
    "urban",
    "primary",
    "secondary",
    "tertiary",
    "n_services",
    "log_ID_p_bldgspace",
    "logVisits",
    "logVisitsPP_TB"
  )

  data <- opvisit_data()

  mod <- JAGSModel$new(data,
    target = "ID_unitcost_total",
    covariates = covariates,
    priors = opvisit_priors(n_weak_priors = length(covariates) - 5)
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}

#' CaptureTB outpatient treatment visit overhead costs model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the overhead costs per outpatient treatment visit
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
#'   log_ID_p_bldgspace = 1,
#'   logVisits = 6.9,
#'   logVisitsPP_TB = -1.29,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   n_services = 3,
#'   fc_country = "Ethiopia"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @export
unitcost_ohd <- function() {
  samples <- readRDS(system.file("posterior_samples_ohd.rds",
    package = "capturetb"
  ))
	dic <- readRDS(system.file("posterior_samples_dic_ohd.rds",
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
    "primary",
    "secondary",
    "tertiary",
    "n_services",
    "log_ID_p_bldgspace",
    "logVisits",
    "logVisitsPP_TB"
  )

  mod <- JAGSModel$new(
    data,
    priors = opvisit_priors(n_weak_priors = length(covariates) - 5),
    covariates = covariates,
    target = "ID_unitcost_ohd"
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}


#' CaptureTB outpatient treatment visit fixed costs model
#'
#' This function loads a [`JAGSModel`] model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict the fixed costs per outpatient treatment visit
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
#'   log_ID_p_bldgspace = 1,
#'   logVisits = 6.9,
#'   logVisitsPP_TB = -1.29,
#'   primary = TRUE,
#'   secondary = FALSE,
#'   tertiary = FALSE,
#'   urban = FALSE,
#'   public = TRUE,
#'   n_services = 3,
#'   fc_country = "Ethiopia"
#' )
#' new_covariates <- prepare_covariates(new_data, mod)
#' mod$predict(new_covariates, summarised = TRUE)
#' @seealso JAGSModel
#' @importFrom stats median
#' @export
unitcost_fixed <- function() {
  samples <- readRDS(system.file("posterior_samples_fixed.rds",
    package = "capturetb"
  ))
	dic <- readRDS(system.file("posterior_samples_dic_fixed.rds",
		package = "capturetb"
	))

  data <- opvisit_data() |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::mutate(ID_unitcost_fixed = median(.data$ID_unitcost_fixed)) |>
    dplyr::filter(!duplicated(.data$ID_unitcost_fixed)) |>
    dplyr::mutate(output = "op_visit") |>
    dplyr::ungroup()

  covariates <- c(
    "public",
    "urban",
    "primary",
    "secondary",
    "tertiary",
    "n_services",
    "log_ID_p_bldgspace",
    "logVisits",
    "logVisitsPP_TB"
  )
  mod <- JAGSModel$new(
    data,
    priors = opvisit_priors(n_weak_priors = length(covariates) - 5),
    covariates = covariates,
    target = "ID_unitcost_fixed"
  )
  mod$.__enclos_env__$private$.samples <- samples
  mod$.__enclos_env__$private$.DIC <- dic
  mod
}
