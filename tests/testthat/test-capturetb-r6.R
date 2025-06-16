test_that("capturetb R6 class initialization works", {
  # Test default initialization
  model <- capturetb$new()

  dat <- get_data("OP treatment visit")
  expect_true(R6::is.R6(model))
  expect_false(model$is_fitted())
  expect_equal(model$get_covariates(), capturetb_covariates())
  expect_true(is.factor(model$get_countries()))
  expect_equal(model$get_training_data(), dat)
  expect_equal(levels(model$get_countries()), unique(dat$fc_country))
  expect_equal(model$get_priors(), capturetb_priors())
})

test_that("capturetb R6 class validation works", {
  # Test invalid data
  expect_error(
    capturetb$new(dat = "not a data frame"),
    "dat must be a data.frame"
  )

  # Test missing covariates
  data <- get_data("OP treatment visit")
  data$log_USD_p_bldgspace <- NULL
  expect_error(
    capturetb$new(dat = data),
    "Missing covariates in data"
  )

  # Test missing target
  expect_error(
    capturetb$new(target = "nonexistent_column"),
    "Target variable.*not found"
  )
})

test_that("capturetb R6 predict method validation works", {
  model <- capturetb$new()

  # Test prediction before fitting
  expect_error(
    model$predict(get_data("OP treatment visit")[1:5, ]),
    "Model must be fitted before making predictions"
  )
})

test_that("capturetb R6 class methods work with small example", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("coda")

  # Use a small dataset for testing with multiple countries
  data <- get_data("OP treatment visit")

  countries_sample <- unique(data$fc_country)[1:2]
  n_countries <- 2
  small_data <- data[data$fc_country %in% countries_sample, ]

  model <- capturetb$new(dat = small_data)

  # Test fitting with minimal iterations but sufficient for convergence
  expect_message(
    model$fit(n.chains = 2, n.iter = 500, n.burnin = 50, n.thin = 2),
    "Model fitted successfully"
  )

  expect_true(model$is_fitted())
  samples <- model$get_samples()
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

test_that("capturetb R6 getter methods work", {
  model <- capturetb$new()

  expect_equal(model$get_covariates(), capturetb_covariates())
  expect_true(is.factor(model$get_countries()))
  expect_true(inherits(model$get_priors(), "capturetbpriors"))
  expect_null(model$get_samples())
})

test_that("can make predictions for new countries", {
  data <- get_data("OP treatment visit")
  covariates <- capturetb_covariates()[1:3]
  model <- capturetb$new(
    dat =
      data[data$fc_country %in% c("Ethiopia", "Kenya"), ],
    covariates = covariates
  )

  # Mock private$samples
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
  fake_samples <- coda::as.mcmc(smat)
  fake_samples <- coda::mcmc.list(fake_samples)
  model$.__enclos_env__$private$samples <- fake_samples

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
