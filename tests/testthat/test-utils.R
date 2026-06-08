test_that("can center covariates", {
  model <- unitcost()
  raw <- list(
    logVisits = 6.9,
    healthcentre = FALSE,
    primary = FALSE,
    secondary = FALSE,
    tertiary = FALSE,
    urban = FALSE,
    public = TRUE,
    fc_country = "Ethiopia",
    output = "op_treatmentvisit"
  )
  cv <- prepare_covariates(
    raw = raw,
    mod = model
  )

  expect_equal(
    cv$logVisits,
    6.9 - model$centering_values()$logVisits,
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cv$logVisits, "scaled:center"),
    model$centering_values()$logVisits
  )
  expect_true(all(names(raw) %in% names(cv)))
})

test_that("centered covariates are converted to numeric", {
  model <- unitcost()
  raw <- list(
    logVisits = "6.9",
    healthcentre = FALSE,
    primary = FALSE,
    secondary = FALSE,
    tertiary = FALSE,
    urban = FALSE,
    public = TRUE,
    fc_country = "Ethiopia",
    output = "op_treatmentvisit"
  )
  cv <- prepare_covariates(
    raw = raw,
    mod = model
  )
  expect_equal(
    cv$logVisits,
    6.9 - model$centering_values()$logVisits,
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cv$logVisits, "scaled:center"),
    model$centering_values()$logVisits
  )
})
