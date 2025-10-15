test_that("can center covariates", {
  model <- unitcost()
  raw <- list(
    log_ID_p_bldgspace = 1,
    logVisits = 6.9,
    logVisitsPP_TB = -1.29,
    n_services = 2,
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
    cv$log_ID_p_bldgspace,
    1 - model$centering_values()$log_ID_p_bldgspace,
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cv$log_ID_p_bldgspace, "scaled:center"),
    model$centering_values()$log_ID_p_bldgspace
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
  expect_equal(
    cv$logVisitsPP_TB,
    -1.29 - model$centering_values()$logVisitsPP_TB,
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cv$logVisitsPP_TB, "scaled:center"),
    model$centering_values()$logVisitsPP_TB
  )
  expect_equal(
    cv$n_services,
    2 - model$centering_values()$n_services,
    ignore_attr = TRUE
  )
  expect_equal(
    attr(cv$n_services, "scaled:center"),
    model$centering_values()$n_services
  )
  expect_true(all(names(raw) %in% names(cv)))
})
