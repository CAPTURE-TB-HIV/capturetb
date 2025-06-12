test_that("can get output types", {
  out <- outputs()
  expect_true(is.character(out))
  expect_true("OP treatment visit" %in% out)
})

test_that("can get all data as a data.frame", {
  dat <- get_data()
  expect_s3_class(dat, "data.frame")
  expect_equal(unique(dat$output), outputs())
  expect_gt(nrow(dat), 0)
})

test_that("can get data filtered to output type", {
  dat <- get_data(output_name = "OP treatment visit")
  expect_s3_class(dat, "data.frame")
  expect_true(all(dat$output == "OP treatment visit"))
  expect_gt(nrow(dat), 0)
})

test_that("output name must be a string", {
  expect_error(
    get_data(output_name = 123),
    "'output_name' must be a string"
  )
})
