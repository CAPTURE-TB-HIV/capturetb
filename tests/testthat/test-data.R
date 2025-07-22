test_that("can get output types", {
  out <- outputs()
  expect_true(is.character(out))
  expect_true("op_treatmentvisit" %in% out)
})

test_that("can get all data as a data.frame", {
  dat <- get_data()
  expect_s3_class(dat, "data.frame")
  expect_equal(unique(dat$output), outputs())
  expect_gt(nrow(dat), 0)
})

test_that("can get data filtered to output type", {
  dat <- get_data(output_name = "op_treatmentvisit")
  expect_s3_class(dat, "data.frame")
  expect_true(all(dat$output == "op_treatmentvisit"))
  expect_gt(nrow(dat), 0)
})

test_that("can get data filtered to output group", {
  dat <- get_data(output_group = "OP")
  expect_s3_class(dat, "data.frame")
  expect_true(all(dat$outputgroup == "OP"))
  expect_gt(nrow(dat), 0)
})

test_that("output name must be a string", {
  expect_error(
    get_data(output_name = 123),
    "'output_name' must be a string"
  )
})

test_that("output group must be a string", {
  expect_error(
    get_data(output_group = 123),
    "'output_group' must be a string"
  )
})
