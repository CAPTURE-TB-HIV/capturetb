test_that("MixedEffects class initialization works", {
  # test default initialisation options
  model <- suppressWarnings(MixedEffects$new())
  dat <- get_data("OP treatment visit")
  expect_true(R6::is.R6(model))
  expect_false(model$is_fitted())
  expect_equal(model$covariates(), capturetb_covariates())
  expect_true(is.factor(model$countries()))
  expect_equal(levels(model$countries()), unique(dat$fc_country))
  expect_equal(
    model$priors(),
    capturetb_priors(beta.mean = rep(0, 5), beta.precision = rep(0.01, 5))
  )

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
  dat <- get_data("OP treatment visit")
  dat[1, "logVisits"] <- NA
  warnings <- testthat::capture_warnings({
    model <- MixedEffects$new(dat)
  })

  expect_true(length(warnings) == 3)
  expect_true(any(grepl(
    "Priors not provided. Vague priors assumed for each covariate coefficient with mu 0 and precision 0.01.",
    warnings
  )))
  expect_true(any(grepl(
    "Removed 1 rows with missing data.",
    warnings
  )))
  expect_true(any(grepl(
    "Excluded 62 rows with duplicate facility codes.",
    warnings
  )))

  # Test invalid data
  expect_error(
    MixedEffects$new(
      dat = "not a data frame",
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      )
    ),
    "dat must be a data.frame"
  )

  # Test missing covariates
  data <- dat_cleaned[1:50, ]
  data$log_USD_p_bldgspace <- NULL
  expect_error(
    MixedEffects$new(
      dat = data,
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      )
    ),
    "Missing covariates in data: log_USD_p_bldgspace"
  )

  # Test missing target
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      ),
      target = "nonexistent_column"
    ),
    "Target variable 'nonexistent_column' not found in data"
  )

  # Test mismatched priors and covariates
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      priors = capturetb_priors(
        beta.mean = rep(0, 6),
        beta.precision = rep(0, 6)
      ),
      covariates = c("one", "two")
    ),
    "6 fixed effect priors provided but only 2 covariates"
  )
  expect_error(
    MixedEffects$new(
      dat = dat_cleaned,
      priors = capturetb_priors(
        beta.mean = rep(0, 4),
        beta.precision = rep(0, 4)
      ),
      covariates = c(capturetb_covariates(), "another")
    ),
    "6 covariates provided but only 4 fixed effect priors"
  )
})

test_that("MixedEffects$predict method validation works", {
  model <- MixedEffects$new(dat_cleaned,
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

test_that("MixedEffects class methods work with small example", {
  # Use a small dataset for testing with multiple countries
  data <- get_data("OP treatment visit")

  countries_sample <- unique(data$fc_country)[1:2]
  n_countries <- 2
  small_data <- dat_cleaned[dat_cleaned$fc_country %in% countries_sample, ]

  model <- MixedEffects$new(
    dat = small_data,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test fitting with minimal iterations but sufficient for convergence
  expect_message(
    model$fit(n.chains = 2, n.iter = 500, n.burnin = 50, n.thin = 2),
    "Model fitted successfully"
  )

  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 2)
  expect_equal(dim(samples[[1]]), c(500 / 2, 8 + n_countries))

  # Test prediction
  pred_data <- small_data[1:3, ]
  predictions <- model$predict(pred_data)

  expect_true(is.matrix(predictions))
  expect_equal(ncol(predictions), nrow(pred_data))
  expect_true(nrow(predictions) > 0)
})

test_that("MixedEffects getter methods work", {
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

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
    logVisitsPP_TB = c(0.2, 0.3),
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
  testthat::expect_equal(preds[1, 2], expected_2, tolerance = 0.1)
})

test_that("returns summarised predictions if summarised=TRUE", {
  covariates <- capturetb_covariates()[1:3]
  priors <- capturetb_priors(beta.mean = rep(0, 3), beta.precision = rep(0, 3))
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
    logVisitsPP_TB = c(0.2, 0.3),
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
  expect_equal(preds_summary$mean, manual_mean, tolerance = 0.01)
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

  model <- MixedEffects$new(
    dat = small_data,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

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
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  expect_error(
    model$mcmc_acf(),
    "Model must be fitted"
  )

  model <- unitcost()
  res <- model$n_eff()
  expect_equal(length(res), 13)
  expect_true(is.numeric(res))
})

test_that("performance method validation works", {
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

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
  expected_names <- c("mae", "rmse", "correlation", "ci_coverage", "median_ci")
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
  expected_names <- c("mae", "rmse", "correlation", "ci_coverage", "median_ci")
  expect_equal(names(perf_natural), expected_names)
  expect_true(all(sapply(perf_natural, is.numeric)))
  expect_true(perf_natural$mae >= 1)
  expect_true(perf_natural$rmse >= 1)
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

test_that("predict_total returns correct dimensions and values", {
  model <- unitcost()

  # Test with single facility
  result_single <- model$predict_total(dat_cleaned[1, ], 1)
  expect_true(is.numeric(result_single))
  expect_true(length(result_single) > 0)
  expect_true(all(is.finite(result_single)))

  # Test with multiple facilities
  test_data <- dat_cleaned[1:3, ]
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
  single_pred <- model$predict(dat_cleaned[1, ], scale = "natural", summarised = FALSE)
  total_pred_1 <- model$predict_total(dat_cleaned[1, ], 1)
  total_pred_5 <- model$predict_total(dat_cleaned[1, ], 5)

  # Total with n_outputs=5 should be approximately 5x the total with n_outputs=1
  expect_equal(mean(total_pred_5), 5 * mean(total_pred_1), tolerance = 0.01)
})

test_that("predict_total validates inputs correctly", {
  model <- unitcost()

  # Test non-dataframe/list input
  expect_error(
    model$predict_total("invalid", 1),
    "dat must be a list or data.frame"
  )

  # Test non-numeric n_outputs
  expect_error(
    model$predict_total(dat_cleaned[1, ], "invalid"),
    "n_outputs must have length"
  )

  # Test n_outputs length mismatch
  expect_error(
    model$predict_total(dat_cleaned[1:2, ], c(1, 2, 3)),
    "n_outputs must have length == nrow\\(dat\\)"
  )
})

test_that("predict_total handles edge cases", {
  model <- unitcost()

  # Test with n_outputs = 0
  result_zero <- model$predict_total(dat_cleaned[1, ], 0)
  expect_true(all(result_zero == 0))

  # Test with fractional n_outputs
  result_frac <- model$predict_total(dat_cleaned[1, ], 0.5)
  expect_true(all(is.finite(result_frac)))
  expect_true(all(result_frac > 0))
})

test_that("predict_total is consistent with predict method", {
  model <- unitcost()
  test_data <- dat_cleaned[1:3, ]
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

test_that("fitted_parameters method works correctly", {
  # Test that method fails before fitting
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  expect_error(
    model$fitted_parameters(),
    "Model must be fitted first"
  )

  # Test with fitted model (use unitcost which already has samples)
  model <- unitcost()
  
  # Test default parameters (95% CI)
  params <- model$fitted_parameters()
  
  expect_true(is.data.frame(params))
  expect_equal(ncol(params), 3)
  expect_equal(names(params), c("mean", "lower", "upper"))
  
  # Check that we have the expected parameters
  expected_params <- c(
    paste0("beta[", 1:5, "]"),  # beta coefficients
    "sigma", "mu_alpha", "sigma_alpha"  # other parameters
  )
  expect_equal(rownames(params), expected_params)
  
  # Check that lower <= mean <= upper for all parameters
  expect_true(all(params$lower <= params$mean))
  expect_true(all(params$mean <= params$upper))
  
  # Check that all values are finite
  expect_true(all(is.finite(params$mean)))
  expect_true(all(is.finite(params$lower)))
  expect_true(all(is.finite(params$upper)))
  
  # Test different probability levels
  params_90 <- model$fitted_parameters(prob = 0.9)
  params_50 <- model$fitted_parameters(prob = 0.5)
  
  expect_equal(dim(params_90), dim(params))
  expect_equal(dim(params_50), dim(params))
  expect_equal(rownames(params_90), rownames(params))
  expect_equal(rownames(params_50), rownames(params))
  
  # Check that narrower CI has smaller intervals
  interval_95 <- params$upper - params$lower
  interval_90 <- params_90$upper - params_90$lower
  interval_50 <- params_50$upper - params_50$lower
  
  expect_true(all(interval_50 <= interval_90))
  expect_true(all(interval_90 <= interval_95))
  
  # Means should be similar across different probability levels
  expect_equal(params$mean, params_90$mean, tolerance = 0.01)
  expect_equal(params$mean, params_50$mean, tolerance = 0.01)
})

test_that("fitted_parameters validates probability parameter", {
  model <- unitcost()
  
  # Test invalid probability values
  expect_error(
    model$fitted_parameters(prob = 1.5),
    "prob.*between 0 and 1|prob.*valid"
  )
  
  expect_error(
    model$fitted_parameters(prob = -0.1),
    "prob.*between 0 and 1|prob.*valid"
  )
  
  expect_error(
    model$fitted_parameters(prob = 0),
    "prob.*between 0 and 1|prob.*valid"
  )
  
  expect_error(
    model$fitted_parameters(prob = 1),
    "prob.*between 0 and 1|prob.*valid"
  )
  
  # Test valid edge cases
  expect_silent(model$fitted_parameters(prob = 0.01))
  expect_silent(model$fitted_parameters(prob = 0.99))
})

test_that("baselines method works correctly", {
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  baselines_result <- model$baselines()

  expect_true(is.data.frame(baselines_result))
  expect_true("fc_country" %in% names(baselines_result))
  expect_true("n_total" %in% names(baselines_result))

  # Check that we have one row per country
  expected_countries <- unique(dat_cleaned$fc_country)
  expect_equal(nrow(baselines_result), length(expected_countries))
  expect_setequal(baselines_result$fc_country, expected_countries)

  # Check that n_total makes sense
  expect_true(all(baselines_result$n_total > 0))
  expect_true(all(is.integer(baselines_result$n_total)))

  # Check that logical covariate columns are present
  logical_covariates <- c("secondary", "urban", "public")
  expected_cols <- paste0("n_", logical_covariates)
  expect_true(all(expected_cols %in% names(baselines_result)))

  # Check that counts are reasonable (between 0 and n_total)
  for (col in expected_cols) {
    expect_true(all(baselines_result[[col]] >= 0))
    expect_true(all(baselines_result[[col]] <= baselines_result$n_total))
    expect_true(all(is.integer(baselines_result[[col]])))
  }

  # Verify counts by manual calculation for one country
  test_country <- expected_countries[1]
  country_data <- dat_cleaned[dat_cleaned$fc_country == test_country, ]
  expected_n_total <- nrow(country_data)
  expected_n_secondary <- sum(country_data$secondary, na.rm = TRUE)

  country_row <- baselines_result[baselines_result$fc_country == test_country, ]
  expect_equal(country_row$n_total, expected_n_total)
  expect_equal(country_row$n_secondary, expected_n_secondary)
})

test_that("covariate_correlation method works correctly", {
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test with plot = FALSE (returns correlation matrix)
  cor_matrix <- model$covariate_correlation(plot = FALSE)

  expect_true(is.matrix(cor_matrix))
  expect_equal(nrow(cor_matrix), length(capturetb_covariates()))
  expect_equal(ncol(cor_matrix), length(capturetb_covariates()))
  expect_equal(rownames(cor_matrix), capturetb_covariates())
  expect_equal(colnames(cor_matrix), capturetb_covariates())

  # Check that it's a proper correlation matrix
  expect_true(all(diag(cor_matrix) == 1))
  expect_true(all(cor_matrix >= -1 & cor_matrix <= 1))
  expect_true(isSymmetric(cor_matrix))

  # Test with plot = TRUE (returns ggplot object)
  skip_if_not_installed("ggcorrplot")
  cor_plot <- model$covariate_correlation(plot = TRUE)

  expect_true(inherits(cor_plot, "ggplot"))
})

test_that("covariate_correlation validates plot parameter", {
  model <- MixedEffects$new(dat_cleaned,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test invalid plot parameter
  expect_error(
    model$covariate_correlation(plot = "invalid"),
    "plot must be TRUE or FALSE"
  )

  expect_error(
    model$covariate_correlation(plot = c(TRUE, FALSE)),
    "plot must be TRUE or FALSE"
  )

  # Test valid parameters
  expect_silent(model$covariate_correlation(plot = TRUE))
  expect_silent(model$covariate_correlation(plot = FALSE))
})
