test_that("MixedEffects class initialization works", {
  # test default initialisation options
  model <- suppressWarnings(MixedEffects$new())
  dat <- get_data("OP treatment visit")
  expect_true(R6::is.R6(model))
  expect_false(model$is_fitted())
  expect_equal(model$covariates(), capturetb_covariates())
  expect_true(is.factor(model$countries()))
  expect_equal(levels(model$countries()), unique(dat$fc_country))
  expect_equal(model$priors(), capturetb_priors())

  dat_cleaned <- dat |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(capturetb_covariates()),
        ~ !is.na(.) & !is.nan(.) & is.finite(.)
      )
    ) |>
    dplyr::group_by(.data$fc_code) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  expect_equal(model$training_data(), dat_cleaned)
})

test_that("MixedEffects$new validation works", {
  warnings <- testthat::capture_warnings({
    model <- MixedEffects$new()
  })

  expect_true(length(warnings) == 2)
  expect_true(any(grepl(
    "Removed 1 rows with missing data.",
    warnings
  )))
  expect_true(any(grepl(
    "Excluded 63 rows with duplicate facility codes.",
    warnings
  )))

  # Test invalid data
  expect_error(
    MixedEffects$new(dat = "not a data frame"),
    "dat must be a data.frame"
  )

  # Test missing covariates
  data <- dat_cleaned[1:50, ]
  data$log_USD_p_bldgspace <- NULL
  expect_error(
    MixedEffects$new(dat = data),
    "Missing covariates in data: log_USD_p_bldgspace"
  )

  # Test missing target
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      target = "nonexistent_column"
    ),
    "Target variable 'nonexistent_column' not found in data"
  )

  # Test mismatched priors and covariates
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      covariates = c("one", "two")
    ),
    "6 fixed effect priors provided but only 2 covariates"
  )
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      covariates = c(capturetb_covariates(), "another")
    ),
    "7 covariates provided but only 6 fixed effect priors"
  )
})

test_that("MixedEffects$predict method validation works", {
  model <- MixedEffects$new(dat_cleaned)

  # Test prediction before fitting
  expect_error(
    model$predict(get_data("OP treatment visit")[1:5, ]),
    "Model must be fitted before making predictions"
  )
})

test_that("MixedEffects class methods work with small example", {
  # Use a small dataset for testing with multiple countries
  data <- get_data("OP treatment visit")

  countries_sample <- unique(data$fc_country)[1:2]
  n_countries <- 2
  small_data <- dat_cleaned[dat_cleaned$fc_country %in% countries_sample, ]

  model <- MixedEffects$new(dat = small_data)

  # Test fitting with minimal iterations but sufficient for convergence
  expect_message(
    model$fit(n.chains = 2, n.iter = 500, n.burnin = 50, n.thin = 2),
    "Model fitted successfully"
  )

  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 2)
  expect_equal(dim(samples[[1]]), c(500 / 2, 9 + n_countries))

  # Test prediction
  pred_data <- small_data[1:3, ]
  predictions <- model$predict(pred_data)

  expect_true(is.matrix(predictions))
  expect_equal(ncol(predictions), nrow(pred_data))
  expect_true(nrow(predictions) > 0)
})

test_that("MixedEffects getter methods work", {
  model <- MixedEffects$new(dat_cleaned)

  expect_equal(model$covariates(), capturetb_covariates())
  expect_true(is.factor(model$countries()))
  expect_true(inherits(model$priors(), "capturetbpriors"))
  expect_null(model$samples())
})

test_that("can make predictions for known and new countries", {
  covariates <- capturetb_covariates()[1:3]
  priors <- capturetb_priors()
  priors$prior.beta.mean <- priors$prior.beta.mean[1:3]
  model <- MixedEffects$new(
    dat =
      dat_cleaned[dat_cleaned$fc_country %in% c("Ethiopia", "Kenya"), ],
    covariates = covariates,
    priors = priors
  )

  n_sim <- 200
  model$.__enclos_env__$private$.samples <- mock_samples(n_sim)

  # Prepare newdata for prediction
  newdata <- data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP = c(0.2, 0.3),
    secondary = c(1, 0),
    urban = c(0, 1),
    public = c(1, 1),
    fc_country = c("Kenya", "somewhere new")
  )

  preds <- model$predict(newdata)

  testthat::expect_true(is.matrix(preds))
  testthat::expect_equal(dim(preds), c(n_sim, nrow(newdata)))

  expected_1 <- 3 + sum(
    c(0.2, 0.3, 0.4) * as.numeric(newdata[1, covariates])
  )
  expected_2 <- 1 + sum(
    c(0.2, 0.3, 0.4) * as.numeric(newdata[2, covariates])
  )

  # should be exact, since country intercept for Kenya is known
  testthat::expect_equal(preds[1, 1], expected_1)

  # tolerance required as intercept alpha_new will be generated
  # using rnorm(alpha_mu, sigma_mu)
  testthat::expect_equal(preds[1, 2], expected_2, tolerance = 0.01)
})

test_that("returns summarised predictions if summarised=TRUE", {
  covariates <- capturetb_covariates()[1:3]
  priors <- capturetb_priors()
  priors$prior.beta.mean <- priors$prior.beta.mean[1:3]
  model <- MixedEffects$new(
    dat =
      dat_cleaned[dat_cleaned$fc_country %in% c("Ethiopia", "Kenya"), ],
    covariates = covariates,
    priors = priors
  )

  model$.__enclos_env__$private$.samples <- mock_samples(200)

  # Prepare newdata for prediction
  newdata <- data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP = c(0.2, 0.3),
    secondary = c(1, 0),
    urban = c(0, 1),
    public = c(1, 1),
    fc_country = c("Kenya", "somewhere new")
  )

  # Test summarised predictions
  preds_summary <- model$predict(newdata, summarised = TRUE)

  expect_true(is.data.frame(preds_summary))
  expect_equal(nrow(preds_summary), nrow(newdata))
  expect_equal(names(preds_summary), c("mean", "lower", "upper"))
  expect_true(all(preds_summary$lower <= preds_summary$mean))
  expect_true(all(preds_summary$mean <= preds_summary$upper))

  # Test with natural scale
  preds_natural <- model$predict(newdata,
    scale = "natural",
    summarised = TRUE
  )
  expect_true(all(preds_natural$mean > 0))
  expect_true(all(preds_natural$lower > 0))
  expect_true(all(preds_natural$upper > 0))

  # Compare with non-summarised predictions
  preds_full <- model$predict(newdata, summarised = FALSE)
  expect_true(is.matrix(preds_full))
  expect_equal(ncol(preds_full), nrow(newdata))

  # Check summarised mean
  manual_mean <- apply(preds_full, 2, mean)
  expect_equal(preds_summary$mean, manual_mean, tolerance = 0.001)
})

test_that("k_fold_cv works correctly", {
  # Use small dataset with faster testing
  data <- get_data("OP treatment visit")
  countries_sample <- unique(data$fc_country)[1:3]
  small_data <- dat_cleaned[dat_cleaned$fc_country %in% countries_sample, ]

  # Further reduce data size for testing
  set.seed(123)
  small_data <- small_data[sample(
    nrow(small_data),
    30
  ), ]

  model <- MixedEffects$new(dat = small_data)

  # Test with 3 folds and minimal iterations
  expect_message(
    cv_results <- model$k_fold_cv(
      k_folds = 3,
      scale = "log",
      seed = 123,
      n.chains = 2,
      n.iter = 200,
      n.burnin = 50,
      n.thin = 2
    ),
    "Processing fold"
  )

  # Check output format
  training_data <- model$training_data()
  expect_true(is.data.frame(cv_results))
  expect_equal(nrow(cv_results), nrow(training_data))
  expect_true(all(c("fold", "observed", "mean", "lower", "upper") %in%
    names(cv_results)))

  # Check fold assignments
  expect_true(all(cv_results$fold %in% 1:3))
  expect_equal(length(unique(cv_results$fold)), 3)

  # Check that all original observations are included
  expect_equal(nrow(cv_results), nrow(training_data))

  # Check prediction bounds
  expect_true(all(cv_results$lower <= cv_results$mean))
  expect_true(all(cv_results$mean <= cv_results$upper))

  # Test with natural scale
  cv_results_natural <- model$k_fold_cv(
    k_folds = 2,
    scale = "natural",
    seed = 123,
    n.chains = 2,
    n.iter = 200,
    n.burnin = 50,
    n.thin = 2
  )

  expect_true(all(cv_results_natural$mean > 0))
  expect_true(all(cv_results_natural$observed > 0))
})

test_that("can get n_eff after model fitting", {
  model <- MixedEffects$new(dat_cleaned)

  expect_error(
    model$mcmc_acf(),
    "Model must be fitted"
  )

  model <- unitcost()
  res <- model$n_eff()
  expect_equal(length(res), 14)
  expect_true(is.numeric(res))
})

test_that("performance method validation works", {
  model <- MixedEffects$new(dat_cleaned)

  # Test performance before fitting
  expect_error(
    model$performance(),
    "Model must be fitted first"
  )

  # Test invalid scale parameter
  model$.__enclos_env__$private$.samples <- mock_samples(100)
  expect_error(
    model$performance(scale = "invalid"),
    "scale must be 'log' or 'natural'"
  )
})

test_that("performance method works with log scale", {
  model <- unitcost()
  perf_log <- model$performance(scale = "log")

  expect_true(is.data.frame(perf_log))
  expect_equal(nrow(perf_log), 1)
  expected_names <- c("mae", "rmse", "correlation", "ci_coverage")
  expect_equal(names(perf_log), expected_names)
  expect_true(all(sapply(perf_log, is.numeric)))
  expect_true(perf_log$mae >= 0 & perf_log$mae <= 1)
  expect_true(perf_log$rmse >= 0 & perf_log$rmse <= 1)
  expect_true(perf_log$correlation >= 0.5 && perf_log$correlation <= 1)
  expect_true(perf_log$ci_coverage >= 0.95 && perf_log$ci_coverage <= 1)
})

test_that("performance method works with natural scale", {
  model <- unitcost()
  perf_natural <- model$performance(scale = "natural")

  expect_true(is.data.frame(perf_natural))
  expect_equal(nrow(perf_natural), 1)
  expected_names <- c("mae", "rmse", "correlation", "ci_coverage")
  expect_equal(names(perf_natural), expected_names)
  expect_true(all(sapply(perf_natural, is.numeric)))
  expect_true(perf_natural$mae >= 1)
  expect_true(perf_natural$rmse >= 2)
  expect_true(perf_natural$correlation >= 0.5 && perf_natural$correlation <= 1)
  expect_true(perf_natural$ci_coverage >= 0.95 && perf_natural$ci_coverage <= 1)

  perf_log <- model$performance(scale = "log")
  expect_equal(perf_log$ci_coverage, perf_natural$ci_coverage)
})

test_that("evpi expects a single row of inputs", {
  model <- unitcost()
  expect_error(
    model$evpi(1:10, 1),
    "dat must be a list or data.frame"
  )
  expect_silent(
    model$evpi(dat_cleaned[1, ], 1)
  )
  expect_silent(
    model$evpi(as.list(dat_cleaned[1, ]), 1)
  )
})

test_that("lambda must be a numeric vector", {
  model <- unitcost()
  expect_error(
    model$evpi(dat_cleaned[1, ], "one"),
    "lambda must be a numeric vector"
  )
  expect_silent(
    model$evpi(as.list(dat_cleaned[1, ]), 1:2)
  )
})

test_that("evpi expects n_outputs to be scalar or have right length", {
  model <- unitcost()
  expect_error(
    model$evpi(dat_cleaned[1, ], 1:5, 1:5),
    "n_outputs must have length"
  )
  expect_silent(
    model$evpi(dat_cleaned[1:2, ], 1:2, 5)
  )
  expect_silent(
    model$evpi(dat_cleaned[1:2, ], 1, 5)
  )
})

test_that("evpi supports scalar n_outputs", {
  model <- unitcost()
  res1 <- model$evpi(dat_cleaned[1:2, ], 1)
  res2 <- model$evpi(dat_cleaned[1:2, ], 1:1)
  expect_equal(res1, res2, tolerance = 0.01)
})

test_that("predict_total expects n_outputs to be scalar or have right length", {
  model <- unitcost()
  expect_error(
    model$predict_total(dat_cleaned[1, ], 1:5),
    "n_outputs must have length"
  )
  expect_silent(
    model$predict_total(dat_cleaned[1:2, ], 1:2)
  )
  expect_silent(
    model$predict_total(dat_cleaned[1:2, ], 1)
  )
})

test_that("predict_total supports scalar n_outputs", {
  model <- unitcost()
  res1 <- model$predict_total(dat_cleaned[1:2, ], 1)
  res2 <- model$predict_total(dat_cleaned[1:2, ], 1:1)
  expect_equal(mean(res1), mean(res2), tolerance = 0.01)
})

test_that("predict_total supports list inputs", {
  model <- unitcost()
  res1 <- model$predict_total(as.list(dat_cleaned[1, ]), 1)
  res2 <- model$predict_total(dat_cleaned[1, ], 1)
  expect_equal(mean(res1), mean(res2), tolerance = 0.01)
})
