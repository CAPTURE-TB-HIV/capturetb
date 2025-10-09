test_that("JAGSModel model can be initialised, fitted, and used for predictions", {
  model <- JAGSModel$new(
    dat = dat_treatment,
    target = "USD_unitcost_total",
    covariates = c("logVisits", "logVisitsPP_TB"),
    priors = capturetb_priors(beta.mean = c(0, 0), beta.precision = c(1, 1))
  )
  fit <- suppressWarnings(model$fit(n.iter = 500))
  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  preds <- model$predict(prepare_covariates(dat_treatment[1, ], model))

  expect_true(is.matrix(preds))
  expect_equal(ncol(preds), 1)
  expect_true(nrow(preds) > 0)
})

test_that("JAGSModel model works for a single covariate", {
  model <- JAGSModel$new(
    dat = dat_treatment,
    target = "USD_unitcost_total",
    covariates = c("logVisits"),
    priors = capturetb_priors()
  )
  fit <- suppressWarnings(model$fit(n.iter = 500))
  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  preds <- model$predict(prepare_covariates(dat_treatment[1, ], model))

  expect_true(is.matrix(preds))
  expect_equal(ncol(preds), 1)
  expect_true(nrow(preds) > 0)
})

test_that("can perform loco validation", {
  model <- unitcost_fixed()
  res <- suppressWarnings(model$leave_one_country_out(n.iter = 500))
  dat <- model$training_data()

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
  expect_true(perf$bayesian_r2 > 0.1)
  expect_true(perf$bayesian_r2 < 0.9)
  expect_true(perf$mae < 1)
  mods <- attr(res, "models")
  expect_true(inherits(mods[[1]], "JAGSModel"))
  expect_equal(length(mods), 5)
})
