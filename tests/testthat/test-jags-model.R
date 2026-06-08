test_that("JAGSModel class initialization works", {
  dat <- get_data(output_name = "op_treatmentvisit")
  model <- suppressWarnings(JAGSModel$new(dat,
    covariates = test_covariates,
    target = "USD_unitcost_total",
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  ))
  expect_true(R6::is.R6(model))
  expect_false(model$is_fitted())
  expect_equal(model$covariates(), test_covariates)
  expect_true(is.factor(model$countries()))
  expect_equal(levels(model$countries()), unique(dat$fc_country))
  expect_equal(
    model$priors(),
    capturetb_priors(beta.mean = rep(0, 5), beta.precision = rep(0.01, 5))
  )

  expect_equal(model$training_data(), dat, ignore_attr = TRUE)
  expect_true(inherits(model$training_data(), "capturetbdata"))
})

test_that("JAGSModel$new validation works", {
  dat <- get_data(output_name = "op_treatmentvisit")
  dat[1, "logVisits"] <- NA
  warnings <- testthat::capture_warnings({
    model <- JAGSModel$new(dat,
      covariates = test_covariates,
      target = "USD_unitcost_total"
    )
  })

  expect_true(length(warnings) == 2)
  expect_true(any(grepl(
    "Priors not provided. Vague priors assumed for each covariate coefficient with mu 0 and precision 0.01.",
    warnings
  )))
  expect_true(any(grepl(
    "Removed 1 rows with missing data.",
    warnings
  )))

  # Test invalid data
  expect_error(
    JAGSModel$new(
      dat = "not a data frame",
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      ),
      target = "USD_unitcost_total",
      covariates = test_covariates
    ),
    "dat must be a data.frame"
  )

  # Test missing covariates
  data <- dat[1:50, ]
  data$log_USD_p_bldgspace <- NULL
  expect_error(
    JAGSModel$new(
      dat = data,
      target = "USD_unitcost_total",
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      ),
      covariates = test_covariates
    ),
    "Missing covariates in data: log_USD_p_bldgspace"
  )

  # Test missing target
  expect_error(
    JAGSModel$new(
      dat = dat,
      priors = capturetb_priors(
        beta.mean = rep(0, 5),
        beta.precision = rep(0.01, 5)
      ),
      covariates = test_covariates,
      target = "nonexistent_column"
    ),
    "Target variable 'nonexistent_column' not found in data"
  )

  # Test mismatched priors and covariates
  expect_error(
    JAGSModel$new(
      dat = dat,
      priors = capturetb_priors(
        beta.mean = rep(0, 6),
        beta.precision = rep(0, 6)
      ),
      target = "USD_unitcost_total",
      covariates = c("one", "two")
    ),
    "6 fixed effect priors provided but only 2 covariates"
  )
  expect_error(
    JAGSModel$new(
      dat = dat,
      priors = capturetb_priors(
        beta.mean = rep(0, 4),
        beta.precision = rep(0, 4)
      ),
      target = "USD_unitcost_total",
      covariates = c(test_covariates, "another")
    ),
    "6 covariates provided but only 4 fixed effect priors"
  )
})

test_that("JAGSModel class methods work with small example", {
  # Use a small dataset for testing with multiple countries
  countries_sample <- unique(dat_multioutput$fc_country)[1:2]
  n_countries <- 2
  small_data <- dat_multioutput[dat_multioutput$fc_country %in% countries_sample, ]

  n_outputs <- length(unique(small_data$output))
  n_fc <- length(unique(small_data$fc_code))

  model <- JAGSModel$new(
    dat = small_data,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    ),
    target = "USD_unitcost_total",
    covariates = test_covariates
  )

  # Test fitting with minimal iterations but sufficient for convergence
  expect_message(
    suppressWarnings(model$fit(n.chains = 2, n.iter = 500, n.burnin = 50, n.thin = 2)),
    "Model fitted successfully"
  )

  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 2)
  expect_equal(dim(samples[[1]]), c(500 / 2, 1 + length(test_covariates) +
    n_countries + n_outputs + n_fc + 4))

  # Test prediction
  pred_data <- prepare_covariates(small_data[1:3, ], model)
  predictions <- model$predict(pred_data)

  expect_true(is.matrix(predictions))
  expect_equal(ncol(predictions), nrow(pred_data))
  expect_true(nrow(predictions) > 0)
})

test_that("JAGSModel getter methods work", {
  model <- JAGSModel$new(dat_multioutput,
    target = "USD_unitcost_total",
    covariates = test_covariates,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  expect_equal(model$covariates(), test_covariates)
  expect_true(is.factor(model$countries()))
  expect_true(inherits(model$priors(), "capturetbpriors"))
  expect_null(model$samples())
})

test_that("k_fold_cv works correctly", {
  small_data <- dat_treatment[sample(
    nrow(dat_treatment),
    30
  ), ]

  model <- JAGSModel$new(
    dat = small_data,
    target = "USD_unitcost_total",
    covariates = test_covariates,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test with 3 folds and minimal iterations
  expect_message(
    suppressWarnings(
      cv_results <- model$k_fold_cv(
        k_folds = 3,
        scale = "log",
        seed = 123,
        n.chains = 2,
        n.iter = 200,
        n.burnin = 50,
        n.thin = 2
      )
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
  cv_results_natural <- suppressWarnings(model$k_fold_cv(
    k_folds = 2,
    scale = "natural",
    seed = 123,
    n.chains = 2,
    n.iter = 200,
    n.burnin = 50,
    n.thin = 2
  ))

  expect_true(all(cv_results_natural$mean > 0))
  expect_true(all(cv_results_natural$observed > 0))
})

test_that("can get n_eff after model fitting", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    target = "USD_unitcost_total",
    covariates = test_covariates,
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

  dat <- model$training_data()

  n_outputs <- length(unique(dat$output))
  n_fc <- length(unique(dat$fc_code))
  n_countries <- length(unique(dat$fc_country))

  expect_equal(length(res), 1 + n_outputs + n_fc +
    n_countries + 4 + length(model$covariates()))
  expect_true(is.numeric(res))
})

test_that("performance method validation works", {
  model <- JAGSModel$new(dat_multioutput,
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    ),
    covariates = test_covariates,
    target = "USD_unitcost_total"
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
  expected_names <- c(
    "mae", "rmse", "ci_coverage",
    "median_ci", "bayesian_r2"
  )
  expect_equal(names(perf_log), expected_names)
  expect_true(all(sapply(perf_log, is.numeric)))
  expect_true(perf_log$mae >= 0 & perf_log$mae <= 1)
  expect_true(perf_log$rmse >= 0 & perf_log$rmse <= 1)
  expect_true(perf_log$bayesian_r2 >= 0.3 && perf_log$bayesian_r2 <= 1)
  expect_true(perf_log$ci_coverage >= 0.95 && perf_log$ci_coverage <= 1)
})

test_that("performance method works with natural scale", {
  model <- unitcost()
  perf_natural <- model$performance(scale = "natural")

  expect_true(is.data.frame(perf_natural))
  expect_equal(nrow(perf_natural), 1)
  expected_names <- c(
    "mae", "rmse", "ci_coverage",
    "median_ci", "bayesian_r2"
  )
  expect_equal(names(perf_natural), expected_names)
  expect_true(all(sapply(perf_natural, is.numeric)))
  expect_true(perf_natural$mae >= 1)
  expect_true(perf_natural$rmse >= 1)
  expect_true(perf_natural$bayesian_r2 >= 0.3 && perf_natural$bayesian_r2 <= 1)
  expect_true(perf_natural$ci_coverage >= 0.95 && perf_natural$ci_coverage <= 1)
})

test_that("fitted_parameters method works correctly", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
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
  n_cov <- length(model$covariates())

  # Test default parameters (95% CI)
  params <- model$fitted_parameters()

  expect_true(is.data.frame(params))
  expect_equal(ncol(params), 5)
  expect_equal(names(params), c("Parameter", "Mean", "CI", "CI_low", "CI_high"))

  # Check that we have the expected parameters
  expected_params <- c(
    "alpha",
    paste0("beta[", 1:n_cov, "]"),
    paste0("country_effect[", 1:5, "]"),
    paste0("output_effect[", 1:14, "]"),
    "sigma", "sigma_c", "sigma_f", "sigma_v"
  )
  expect_equal(
    params$Parameter[order(params$Parameter)],
    expected_params[order(expected_params)]
  )

  # Check that lower <= mean <= upper for all parameters
  expect_true(all(params$CI_low <= params$Mean))
  expect_true(all(params$Mean <= params$CI_high))

  # Test different probability levels
  params_90 <- model$fitted_parameters(ci = 0.9)
  params_50 <- model$fitted_parameters(ci = 0.5)

  # Check that narrower CI has smaller intervals
  interval_95 <- params$CI_high - params$CI_low
  interval_90 <- params_90$CI_high - params_90$CI_low
  interval_50 <- params_50$CI_high - params_50$CI_low

  expect_true(all(interval_50 <= interval_90))
  expect_true(all(interval_90 <= interval_95))
})

test_that("baselines method works correctly", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
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
  expected_countries <- unique(dat_multioutput$fc_country)
  expect_equal(nrow(baselines_result), length(expected_countries))
  expect_setequal(baselines_result$fc_country, expected_countries)

  # Check that n_total makes sense
  expect_true(all(baselines_result$n_total > 0))
  expect_true(all(is.numeric(baselines_result$n_total)))

  # Check that logical covariate columns are present
  logical_covariates <- c("secondary", "public")
  expected_cols <- paste0("n_", logical_covariates)
  expect_true(all(expected_cols %in% names(baselines_result)))

  # Check that counts are reasonable (between 0 and n_total)
  for (col in expected_cols) {
    expect_true(all(baselines_result[[col]] >= 0))
    expect_true(all(baselines_result[[col]] <= baselines_result$n_total))
    expect_true(all(is.numeric(baselines_result[[col]])))
  }

  # Verify counts by manual calculation for one country
  test_country <- expected_countries[1]
  country_data <- dat_multioutput[dat_multioutput$fc_country == test_country, ]
  expected_n_total <- nrow(country_data)
  expected_n_secondary <- sum(country_data$secondary, na.rm = TRUE)

  country_row <- baselines_result[baselines_result$fc_country == test_country, ]
  expect_equal(country_row$n_total, expected_n_total)
  expect_equal(country_row$n_secondary, expected_n_secondary)
})

test_that("covariate_correlation method works correctly", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )

  # Test with plot = FALSE (returns correlation matrix)
  cor_matrix <- model$covariate_correlation(plot = FALSE)

  expect_true(is.matrix(cor_matrix))
  expect_equal(nrow(cor_matrix), length(test_covariates))
  expect_equal(ncol(cor_matrix), length(test_covariates))
  expect_equal(rownames(cor_matrix), test_covariates)
  expect_equal(colnames(cor_matrix), test_covariates)

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
  model <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
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

test_that("can perform loco validation", {
  model <- unitcost()
  res <- suppressWarnings(model$leave_one_country_out(n.iter = 500))
  dat <- model$training_data()

  # for this we only include outputs that are found in more than 1 country
  output_counts <- table(dat$output, dat$fc_country)
  outputs_multiple_countries <- rownames(output_counts)[rowSums(output_counts > 0) > 1]
  dat <- dat[dat$output %in% outputs_multiple_countries, , drop = FALSE]

  expect_equal(nrow(res), nrow(dat))
  expect_equal(names(res), c("country", "observed", "mean", "lower", "upper"))

  perf <- attr(res, "performance")
  expect_equal(
    names(perf),
    c(
      "mae", "rmse", "ci_coverage",
      "median_ci", "bayesian_r2"
    )
  )
  expect_true(perf$bayesian_r2 > 0.4)
  expect_true(perf$bayesian_r2 < 0.5)
  expect_true(perf$mae < 1)
  mods <- attr(res, "models")
  expect_true(inherits(mods[[1]], "JAGSModel"))
  expect_equal(length(mods), 5)
})

test_that("can get loco validation results on natural scale", {
  model <- unitcost()
  res <- suppressWarnings(model$leave_one_country_out(n.iter = 500, scale = "natural"))

  dat <- model$training_data()

  # for this we only include outputs that are found in more than 1 country
  output_counts <- table(dat$output, dat$fc_country)
  outputs_multiple_countries <- rownames(output_counts)[rowSums(output_counts > 0) > 1]
  dat <- dat[dat$output %in% outputs_multiple_countries, , drop = FALSE]
  expect_equal(nrow(res), nrow(dat))
  expect_equal(names(res), c("country", "observed", "mean", "lower", "upper"))

  perf <- attr(res, "performance")
  expect_equal(
    names(perf),
    c("mae", "rmse", "ci_coverage", "median_ci", "bayesian_r2")
  )
  expect_true(perf$bayesian_r2 > 0.3)
  expect_true(perf$bayesian_r2 < 0.6)
  expect_true(perf$mae > 1)
  mods <- attr(res, "models")
  expect_true(inherits(mods[[1]], "JAGSModel"))
  expect_equal(length(mods), 5)
})

test_that("can get DIC", {
  mod <- JAGSModel$new(
    dat = dat_multioutput,
    covariates = test_covariates,
    target = "USD_unitcost_total",
    priors = capturetb_priors(
      beta.mean = rep(0, 5),
      beta.precision = rep(0.01, 5)
    )
  )
  expect_error(mod$mcmc_DIC(), "Model must be fitted first.")

  suppressWarnings(mod$fit(n.chains = 2, n.iter = 500, n.burnin = 50, n.thin = 2))

  DIC <- mod$mcmc_DIC(summarised = FALSE)
  expect_equal(names(DIC), c("deviance", "penalty", "type"))
})
