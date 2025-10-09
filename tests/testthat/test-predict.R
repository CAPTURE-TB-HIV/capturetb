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
    model$predict(get_data("OP treatment visit")[1:5, ]),
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
  newdata <- data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP_TB = c(0.2, 0.3),
    fc_country = c("Kenya", "somewhere new"),
    output = c("op_treatmentvisit", "op_monitoringvisit")
  )

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
  newdata <- data.frame(
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
  )

  # Test summarised predictions
  preds_summary <- model$predict(newdata, summarised = TRUE)

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

test_that("predict_total expects n_outputs to be scalar or have right length", {
  model <- unitcost()
  dat <- model$training_data()
  expect_error(
    model$predict_total(dat[1, ], 1:5),
    "n_outputs must have length"
  )
  expect_silent(
    model$predict_total(dat[1:2, ], 1:2)
  )
  expect_silent(
    model$predict_total(dat[1:2, ], 1)
  )
})

test_that("predict_total supports scalar n_outputs", {
  model <- unitcost()
  dat <- model$training_data()
  res1 <- model$predict_total(dat[1:2, ], 1)
  res2 <- model$predict_total(dat[1:2, ], 1:1)
  expect_equal(mean(res1), mean(res2), tolerance = 0.01)
})

test_that("predict_total supports list inputs", {
  model <- unitcost()
  dat <- model$training_data()
  res1 <- model$predict_total(as.list(dat[1, ]), 1)
  res2 <- model$predict_total(dat[1, ], 1)
  expect_equal(mean(res1), mean(res2), tolerance = 0.1)
})

test_that("predict_total returns correct dimensions and values", {
  model <- unitcost()
  dat <- model$training_data()

  # Test with single facility
  result_single <- model$predict_total(dat[1, ], 1)
  expect_true(is.numeric(result_single))
  expect_true(length(result_single) > 0)
  expect_true(all(is.finite(result_single)))

  # Test with multiple facilities
  test_data <- dat[c(1, 10, 20), ]
  result_multiple <- model$predict_total(test_data, 1)
  expect_true(is.numeric(result_multiple))
  expect_true(length(result_multiple) > 0)
  expect_true(all(is.finite(result_multiple)))

  # Test with different n_outputs for each facility
  n_outputs_vector <- c(2, 3, 1)
  result_varying <- model$predict_total(test_data, n_outputs_vector)
  expect_true(is.numeric(result_varying))
  expect_true(length(result_varying) == length(result_multiple))

  # Test that outputs scale correctly with n_outputs
  single_pred <- model$predict(dat[1, ], scale = "natural", summarised = FALSE)
  total_pred_1 <- model$predict_total(dat[1, ], 1)
  total_pred_5 <- model$predict_total(dat[1, ], 5)

  # Total with n_outputs=5 should be approximately 5x the total with n_outputs=1
  expect_equal(mean(total_pred_5), 5 * mean(total_pred_1), tolerance = 0.01)
})

test_that("predict_total validates inputs correctly", {
  model <- unitcost()
  dat <- model$training_data()

  # Test non-dataframe/list input
  expect_error(
    model$predict_total("invalid", 1),
    "dat must be a list or data.frame"
  )

  # Test non-numeric n_outputs
  expect_error(
    model$predict_total(dat[1, ], "invalid"),
    "n_outputs must have length"
  )

  # Test n_outputs length mismatch
  expect_error(
    model$predict_total(dat[1:2, ], c(1, 2, 3)),
    "n_outputs must have length == nrow\\(dat\\)"
  )
})

test_that("predict_total handles edge cases", {
  model <- unitcost()
  dat <- model$training_data()

  # Test with n_outputs = 0
  result_zero <- model$predict_total(dat[1, ], 0)
  expect_true(all(result_zero == 0))

  # Test with fractional n_outputs
  result_frac <- model$predict_total(dat[1, ], 0.5)
  expect_true(all(is.finite(result_frac)))
  expect_true(all(result_frac > 0))
})

test_that("predict_total is consistent with predict method", {
  model <- unitcost()
  dat <- model$training_data()
  test_data <- dat[c(1, 10, 20), ]
  n_outputs <- c(3, 2, 4)

  # Get individual predictions
  individual_preds <- model$predict(test_data,
    scale = "natural",
    summarised = FALSE
  )

  # Get total prediction
  total_pred <- model$predict_total(test_data, n_outputs)

  # Manual calculation: multiply each column by n_outputs and sum
  expected_total <- individual_preds[, 1] * 3 +
    individual_preds[, 2] * 2 + individual_preds[, 3] * 4

  expect_equal(mean(total_pred), mean(expected_total), tolerance = 0.1)
  expect_equal(sd(total_pred), sd(expected_total), tolerance = 0.1)
})
