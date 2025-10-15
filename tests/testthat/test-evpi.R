test_that("evpi expects prepared data", {
  model <- unitcost()
  dat <- model$training_data()
  expect_error(
    model$evpi(1:10, 1),
    "'dat' must be prepared using prepare_covariates"
  )
  expect_silent(
    model$evpi(dat[1, ], 1)
  )
  expect_silent(
    model$evpi(dat, 1)
  )
  new_data <- prepare_covariates(list(
    log_ID_p_bldgspace = 1,
    logVisits = 6.9,
    logVisitsPP_TB = -1.29,
    primary = TRUE,
    secondary = FALSE,
    tertiary = FALSE,
    urban = FALSE,
    public = TRUE,
    n_services = 3,
    output = "op_treatmentvisit",
    fc_country = "Ethiopia"
  ), model)
  expect_silent(
    model$evpi(new_data, 1)
  )
})

test_that("lambda must be a numeric vector", {
  model <- unitcost()
  dat <- model$training_data()
  expect_error(
    model$evpi(dat[1, ], "one"),
    "lambda must be a numeric vector"
  )
  expect_silent(
    model$evpi(dat[1, ], 1:2)
  )
})

test_that("evpi expects n_outputs to be scalar or have right length", {
  model <- unitcost()
  dat <- model$training_data()
  expect_error(
    model$evpi(dat[1, ], 1:5, 1:5),
    "n_outputs must have length"
  )
  expect_silent(
    model$evpi(dat[1:2, ], 1:2, 5)
  )
  expect_silent(
    model$evpi(dat[1:2, ], 1, 5)
  )
})

test_that("evpi supports scalar n_outputs", {
  model <- unitcost()
  dat <- model$training_data()
  res1 <- model$evpi(dat[1:2, ], 1)
  res2 <- model$evpi(dat[1:2, ], 1:1)
  expect_equal(res1, res2, tolerance = 0.01)
})
