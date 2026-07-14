test_that("multi output model works without country effects", {
  model <- JAGSModel$new(
    dat = dat_multioutput,
    target = "USD_unitcost_total",
    covariates = c("logVisits", "logVisitsPP_TB"),
    country_random_effects = FALSE,
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

test_that("single output model works without country effects", {
  model <- JAGSModel$new(
    dat = dat_treatment,
    target = "USD_unitcost_total",
    covariates = c("logVisits", "logVisitsPP_TB"),
    country_random_effects = FALSE,
    priors = capturetb_priors(beta.mean = c(0, 0), beta.precision = c(1, 1))
  )
  expect_true(length(model$outputs()) == 1)
  fit <- suppressWarnings(model$fit(n.iter = 500))
  expect_true(model$is_fitted())
  samples <- model$samples()
  expect_true(inherits(samples, "mcmc.list"))
  preds <- model$predict(prepare_covariates(dat_treatment[1, ], model))

  expect_true(is.matrix(preds))
  expect_equal(ncol(preds), 1)
  expect_true(nrow(preds) > 0)
})
