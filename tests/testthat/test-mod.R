test_that("run_capturetb_model runs and returns coda::mcmc.list", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("coda")
  skip_if_not_installed("capturetb")

  data <- capturetb::get_data(output = "OP treatment visit")
  expect_s3_class(data, "data.frame")

  samples <- fit_capturetb_model(
    dat = data,
    n.chains = 2,
    n.iter = 2000,
    n.burnin = 500,
    n.adapt = 500,
    n.thin = 2,
    seed = 123
  )

  expect_s3_class(samples, "mcmc.list")
  expect_equal(length(samples), 2)
  expect_equal(dim(samples[[1]]), c(2000 / 2, 14))
})

testthat::test_that("predict_capturetb_logcost returns correct dimensions and values", {
  data <- data.frame(
    log_USD_p_bldgspace = c(1, 2),
    logVisits = c(0.5, 1.5),
    logVisitsPP = c(0.2, 0.3),
    secondary = c(1, 0),
    urban = c(0, 1),
    public = c(1, 1),
    fc_country = c("Kenya", "somewhere new")
  )
  covariates <- c("logVisits", "logVisitsPP", "secondary")

  # mock posterior samples
  n_sim <- 500
  smat <- matrix(
    c(
      rep(1, n_sim), # mu_alpha
      rep(0.01, n_sim), # sigma_alpha
      rep(0.2, n_sim), # beta[1]
      rep(0.3, n_sim), # beta[2]
      rep(0.4, n_sim), # beta[3]
      rep(2, n_sim), # alpha[1]
      rep(3, n_sim) # alpha[2]
    ),
    nrow = n_sim,
    ncol = 7,
    byrow = FALSE
  )
  colnames(smat) <- c(
    "mu_alpha", "sigma_alpha",
    paste0("beta[", 1:3, "]"),
    paste0("alpha[", 1:2, "]")
  )
  samples <- coda::as.mcmc(smat)
  samples <- coda::mcmc.list(samples)

  countries <- c("Ethiopia", "Kenya")
  preds <- predict_capturetb_logcost(data, covariates, countries, samples)

  testthat::expect_true(is.matrix(preds))
  testthat::expect_equal(dim(preds), c(n_sim, nrow(data)))

  expected_1 <- 3 + sum(
    c(0.2, 0.3, 0.4) * as.numeric(data[1, covariates])
  )
  expected_2 <- 1 + sum(
    c(0.2, 0.3, 0.4) * as.numeric(data[2, covariates])
  )

  # should be exact, since country intercept for Kenya is known
  testthat::expect_equal(preds[1, 1], expected_1)

  # tolerance required as intercept alpha_new will be generated
  # using rnorm(alpha_mu, sigma_mu)
  testthat::expect_equal(preds[1, 2], expected_2, tolerance = 0.01)
})

testthat::test_that("predict_capturetb_logcost works with fitted model", {
  data <- get_data(output_name = "OP treatment visit")
  samples <- fit_capturetb_model(
		dat = data,
    n.chains = 2,
    n.iter = 2000,
    n.burnin = 500,
    n.adapt = 500,
    n.thin = 2,
    seed = 123
  )

  countries <- unique(data$fc_country)

  preds <- predict_capturetb_logcost(data, covariates = capturetb_covariates(), countries, samples)

  testthat::expect_true(is.matrix(preds))
  testthat::expect_equal(dim(preds), c(2000, nrow(data)))
})


# priors <- capturetb_priors(sigma_alpha.rate = 0.1)
# plot(priors, "sigma_alpha")
# samples <- fit_capturetb_model(priors = priors)

# pred <- predict_capturetb_logcost(
#   data = get_data("OP treatment visit"),
#   covariates = capturetb_covariates(),
#   samples = samples
# )

# plot(apply(pred, 2, mean),
# 	log(get_data("OP treatment visit")$USD_unitcost_total)
# )

# bayesplot::mcmc_areas(samples,
#   prob = 0.9, pars = c("mu_alpha", "sigma", "sigma_alpha")
# )

# bayesplot::mcmc_areas(samples,
#   prob = 0.9, pars = c("mu_alpha", "sigma", "sigma_alpha")
# )

# coda::gelman.plot(samples, autoburnin = FALSE)
