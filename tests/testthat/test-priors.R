test_that("capturetb_priors returns correct class and structure", {
  priors <- capturetb_priors()
  expect_s3_class(priors, "capturetbpriors")
  expect_named(priors, c(
    "prior.mu_alpha.mean",
    "prior.mu_alpha.precision",
    "prior.sigma.rate",
    "prior.sigma_alpha.rate",
    "prior.beta.mean",
    "prior.beta.precision"
  ))
})

test_that("capturetb_priors accepts custom values", {
  custom <- capturetb_priors(
    mu_alpha.mean = 1,
    mu_alpha.precision = 2,
    sigma.rate = 3,
    sigma_alpha.rate = 4,
    beta.mean = c(1, 2, 3, 4, 5, 6),
    beta.precision = c(10, 20, 30, 40, 50, 60)
  )
  expect_equal(custom$prior.mu_alpha.mean, 1)
  expect_equal(custom$prior.mu_alpha.precision, 2)
  expect_equal(custom$prior.sigma.rate, 3)
  expect_equal(custom$prior.sigma_alpha.rate, 4)
  expect_equal(custom$prior.beta.mean, c(1, 2, 3, 4, 5, 6))
  expect_equal(custom$prior.beta.precision, c(10, 20, 30, 40, 50, 60))
})

test_that("capturetb_priors errors on invalid input", {
  expect_error(
    capturetb_priors(mu_alpha.mean = "a"),
    "mu_alpha.mean must be numeric"
  )
  expect_error(
    capturetb_priors(mu_alpha.precision = c(1, 2)),
    "mu_alpha.precision must be scalar"
  )
  expect_error(
    capturetb_priors(sigma.rate = "b"),
    "sigma.rate must be numeric"
  )
  expect_error(
    capturetb_priors(sigma_alpha.rate = NA),
    "sigma_alpha.rate must be numeric"
  )
  expect_error(
    capturetb_priors(beta.mean = "foo"),
    "beta.mean must be numeric"
  )
  expect_error(
    capturetb_priors(beta.precision = list(1, 2)),
    "beta.precision must be numeric"
  )
  expect_error(
    capturetb_priors(beta.precision = c(NaN, 2)),
    "beta.precision must not be NaN"
  )
  expect_error(
    capturetb_priors(sigma.rate = NaN),
    "sigma.rate must not be NaN"
  )
})
