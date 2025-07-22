test_that("can load total unit cost model", {
  mod <- unitcost()
  samples <- mod$samples()
  expect_true(mod$is_fitted())
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 3)
  n_iter_expected <- 1000000
  n_thin_expected <- 100
  n_outputs <- length(mod$outputs())
  n_fc <- length(unique(mod$training_data()$fc_code))
  expect_equal(
    dim(samples[[1]]),
    c((n_iter_expected) / n_thin_expected, 1 + length(mod$covariates()) +
      5 + n_outputs + n_fc + 4)
  )
  expect_equal(mod$priors()$prior.sigma_country.scale, 0.1)
  expect_equal(
    mod$covariates(),
    c(
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
  )
  expect_equal(mod$target(), "ID_unitcost_total")
  n_eff <- mod$n_eff()
  n_eff <- n_eff[!sapply(names(n_eff), grepl, pattern = "(^fc)")]
  expect_true(all(n_eff > 20000))
})

test_that("can load fixed unitcost model", {
  mod <- unitcost_fixed()
  samples <- mod$samples()
  expect_true(mod$is_fitted())
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 3)
  n_iter_expected <- 1000000
  n_thin_expected <- 100

  n_outputs <- length(mod$outputs())
  n_fc <- length(unique(mod$training_data()$fc_code))
  expect_equal(
    dim(samples[[1]]),
    c((n_iter_expected) / n_thin_expected, 1 + length(mod$covariates()) +
      5 + 2)
  )
  expect_equal(mod$priors()$prior.sigma_country.scale, 0.1)
  expect_equal(
    mod$covariates(),
    c(
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
  )
  expect_equal(mod$target(), "ID_unitcost_fixed")
  n_eff <- mod$n_eff()
  n_eff <- n_eff[!sapply(names(n_eff), grepl, pattern = "(^fc)")]

  expect_true(all(n_eff > 20000))
})

test_that("can load ohd unitcost model", {
  mod <- unitcost_ohd()
  samples <- mod$samples()
  expect_true(mod$is_fitted())
  expect_true(inherits(samples, "mcmc.list"))
  expect_equal(length(samples), 3)
  n_iter_expected <- 1000000
  n_thin_expected <- 100
  n_outputs <- length(mod$outputs())
  expect_equal(
    dim(samples[[1]]),
    c((n_iter_expected) / n_thin_expected, 1 + length(mod$covariates()) +
      5 + 2)
  )
  expect_equal(mod$priors()$prior.sigma_country.scale, 0.1)
  expect_equal(
    mod$covariates(),
    c(
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
  )
  expect_equal(mod$target(), "ID_unitcost_ohd")
  expect_true(all(mod$n_eff() > 20000))
})

test_that("can get DIC", {
  expect_true(inherits(unitcost()$mcmc_DIC(summarised = FALSE), "dic"))
  expect_true(inherits(unitcost_fixed()$mcmc_DIC(summarised = FALSE), "dic"))
  expect_true(inherits(unitcost_ohd()$mcmc_DIC(summarised = FALSE), "dic"))

  expect_true(inherits(unitcost()$mcmc_DIC(), "numeric"))
  expect_true(inherits(unitcost_fixed()$mcmc_DIC(), "numeric"))
  expect_true(inherits(unitcost_ohd()$mcmc_DIC(), "numeric"))
})
