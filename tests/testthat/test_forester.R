context("Tests for forester function")

source("objects_for_tests.R")

test_that("Running basic function", {
  model <- forester(apartments, "m2.price", "regression")
  expect_s3_class(model, "forester_model")
})

test_that("Running basic function", {
  model <- forester(apartments, "m2.price", "regression", data_test = apartmentsTest)
  expect_s3_class(model, "forester_model")
})


