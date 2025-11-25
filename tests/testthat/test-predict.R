test_that("JAGSModel$predict method validation works", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test prediction before fitting
  expect_error(
    model$predict(dat_treatment[1:5, ]),
    "Model must be fitted before making predictions"
  )
})

test_that("can make predictions for known and new countries", {
  covariates <- test_covariates[1:3]
  priors <- capturetb_priors()
  priors$prior.beta.mean <- priors$prior.beta.mean[1:3]
  dat <- unitcost()$training_data() |>
    dplyr::filter(output %in% c("op_monitoringvisit", "op_treatmentvisit")) |>
    dplyr::filter(fc_country %in% c("Kenya", "Ethiopia"))

  model <- JAGSModel$new(
    dat = dat,
    covariates = covariates,
    target = "USD_unitcost_total",
    priors = priors
  )

  n_sim <- 200
  model$.__enclos_env__$private$.samples <- mock_samples(n_sim)

  # Prepare newdata for prediction
  newdata <- prepare_covariates(data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP_TB = c(0.2, 0.3),
    fc_country = c("Kenya", "somewhere new"),
    output = c("op_treatmentvisit", "op_monitoringvisit")
  ), model)

  preds <- model$predict(newdata)

  testthat::expect_true(is.matrix(preds))
  testthat::expect_equal(dim(preds), c(n_sim, nrow(newdata)))

  alpha <- 1
  kenya_intercept <- 0.3
  tmt_intercept <- 2
  monitoring_intercept <- 1
  expected_1 <- alpha + kenya_intercept + tmt_intercept + sum(
    c(0.2, 0.3, 0.4) * as.numeric(newdata[1, covariates])
  )
  expected_2 <- alpha + monitoring_intercept + sum(
    c(0.2, 0.3, 0.4) * as.numeric(newdata[2, covariates])
  )

  # should be exact, since country intercept for Kenya is known
  testthat::expect_equal(preds[1, 1], expected_1)

  # tolerance required as country effect will be generated
  # using rnorm(0, 0.01)
  testthat::expect_equal(preds[1, 2], expected_2, tolerance = 0.1)
})

test_that("returns summarised predictions if summarised=TRUE", {
  covariates <- test_covariates[1:3]
  priors <- capturetb_priors(beta.mean = rep(0, 3), beta.precision = rep(0, 3))
  dat <- unitcost()$training_data() |>
    dplyr::filter(output %in% c("op_treatmentvisit", "op_monitoringvisit")) |>
    dplyr::filter(fc_country %in% c("Kenya", "Georgia"))

  model <- JAGSModel$new(
    dat = dat,
    covariates = covariates,
    target = "USD_unitcost_total",
    priors = priors
  )

  model$.__enclos_env__$private$.samples <- mock_samples(200)

  # Prepare newdata for prediction
  newdata <- prepare_covariates(data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP_TB = c(0.2, 0.3),
    secondary = c(1, 0),
    urban = c(0, 1),
    public = c(1, 1),
    fc_country = c("Kenya", "somewhere new"),
    fc_type = c("Health centre", "Tertiary hospital"),
    output = "op_treatmentvisit",
    n_services = c(5, 10)
  ), model)

  # Test summarised predictions
  preds_summary <- model$predict(newdata,
    summarised = TRUE
  )

  expect_true(is.data.frame(preds_summary))
  expect_true(inherits(preds_summary, "describe_posterior"))
  expect_equal(nrow(preds_summary), nrow(newdata))
  expect_equal(
    names(preds_summary),
    c("Observation", "Mean", "CI", "CI_low", "CI_high")
  )
  expect_true(all(preds_summary$CI_low <= preds_summary$Mean))
  expect_true(all(preds_summary$Mean <= preds_summary$CI_high))

  # Test with natural scale
  preds_natural <- model$predict(newdata,
    scale = "natural",
    summarised = TRUE
  )
  expect_true(all(preds_natural$Mean > 0))
  expect_true(all(preds_natural$CI_low > 0))
  expect_true(all(preds_natural$Ci_high > 0))

  # Compare with non-summarised predictions
  preds_full <- model$predict(newdata, summarised = FALSE)
  expect_true(is.matrix(preds_full))
  expect_equal(ncol(preds_full), nrow(newdata))

  # Check summarised mean
  manual_mean <- apply(preds_full, 2, mean)
  expect_equal(preds_summary$Mean, manual_mean, tolerance = 0.01)
})

test_that("multioutput models require output", {
  multioutput <- unitcost()
  dat <- multioutput$training_data()
  dat$output <- NULL

  expect_error(
    multioutput$predict(dat), "Missing required columns in data: output"
  )
  dat$output <- "unknown"
  expect_warning(
    multioutput$predict(dat), "Unknown output types: unknown"
  )
})

test_that("single output models do not require output", {
  mod <- unitcost_fixed()
  dat <- mod$training_data()
  dat$output <- NULL

  expect_silent(
    mod$predict(dat)
  )
  dat$output <- "unknown"
  expect_warning(
    mod$predict(dat), "Unknown output types: unknown"
  )
})

test_that("conditional predictions require fc_code for multioutput models", {
  mod <- unitcost()
  dat <- mod$training_data()
  dat$fc_code <- NULL

  expect_error(
    mod$performance(dat = dat, conditional = TRUE),
    "Column 'fc_code' required"
  )
})

test_that("conditional flag ignored for single output models", {
  mod <- unitcost_fixed()
  dat <- mod$training_data()
  dat$fc_code <- NULL

  expect_warning(
    mod$performance(dat = dat, conditional = TRUE),
    "conditional = TRUE has no effect when there is only one output type"
  )
})
