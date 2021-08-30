context("Tests for compare function")

source("objects_for_tests.R")

test_that("Running function without metric", {
  model_regr <- compare_models(models_regr, apartments, "m2.price")
  model_classif <- compare_models(models_classif, iris_bin, "Species")
  expect_s3_class(model_regr, "forester_model")
  expect_s3_class(model_classif, "forester_model")
})

test_that("Running function with wrong metric", {
  expect_error(model_regr <- compare_models(models_regr, apartments, "m2.price", "auc"))
  expect_error(model_classif <- compare_models(models_classif, iris_bin, "Species", "rmse"))
})

test_that("Wrong type of object in model list ", {
  models1 <- list(ranger_regr, 3)
  models2<- list(3, ranger_regr)
  expect_error(model_regr <- compare_models(models1, apartments, "m2.price"))
  expect_error(model_classif <- compare_models(models2, iris_bin, "Species"))
})

test_that("Mixed type of tasks ", {
  models <- list(ranger_regr, xgboost_classif)
  expect_error(model_regr <- compare_models(models, apartments, "m2.price"))
})

test_that("Empty list or no list ", {
  models <- list()
  expect_error(model_regr <- compare_models(models, apartments, "m2.price"), 
               "List of models is empty.")
  expect_error(model_regr <- compare_models(ranger_regr, apartments, "m2.price"), 
               "Models should be passed as a list")
})


