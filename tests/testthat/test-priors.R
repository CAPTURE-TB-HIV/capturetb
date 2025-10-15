test_that("capturetb_priors returns correct class and structure", {
  priors <- capturetb_priors()
  expect_s3_class(priors, "capturetbpriors")
  expect_named(priors, c(
    "prior.alpha.mean",
    "prior.alpha.precision",
    "prior.sigma.scale",
    "prior.sigma_c.scale",
    "prior.sigma_f.scale",
    "prior.sigma_v.scale",
    "prior.beta.mean",
    "prior.beta.precision"
  ))
})

test_that("capturetb_priors accepts custom values", {
  custom <- capturetb_priors(
    alpha.mean = 1,
    alpha.precision = 2,
    sigma.scale = 3,
    sigma_c.scale = 4,
    beta.mean = c(1, 2, 3, 4, 5, 6),
    beta.precision = c(10, 20, 30, 40, 50, 60)
  )
  expect_equal(custom$prior.alpha.mean, 1)
  expect_equal(custom$prior.alpha.precision, 2)
  expect_equal(custom$prior.sigma.scale, 3)
  expect_equal(custom$prior.sigma_c.scale, 4)
  expect_equal(custom$prior.beta.mean, c(1, 2, 3, 4, 5, 6))
  expect_equal(custom$prior.beta.precision, c(10, 20, 30, 40, 50, 60))
})

test_that("capturetb_priors errors on invalid input", {
  expect_error(
    capturetb_priors(alpha.mean = "a"),
    "alpha.mean must be numeric"
  )
  expect_error(
    capturetb_priors(alpha.precision = c(1, 2)),
    "alpha.precision must be scalar"
  )
  expect_error(
    capturetb_priors(sigma.scale = "b"),
    "sigma.scale must be numeric"
  )
  expect_error(
    capturetb_priors(sigma_c.scale = NA),
    "sigma_c.scale must be numeric"
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
    capturetb_priors(sigma.scale = NaN),
    "sigma.scale must not be NaN"
  )
})
