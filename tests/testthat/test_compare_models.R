context("Tests for compare function")

source("objects_for_tests.R")

test_that("Running function without metric", {
  model_regr <- evaluate(ranger_regr, xgboost_regr,
                               data_test = apartments,
                               target = "m2.price")
  model_classif <- evaluate(ranger_classif, xgboost_classif,
                                  data_test = iris_bin,
                                  target = "Species")
  expect_type(model_regr, "list")
  expect_s3_class(model_regr$best_model, "forester_model")
  expect_type(model_classif, "list")
  expect_s3_class(model_classif$best_model, "forester_model")
})

test_that("Running function with wrong metric", {
  expect_error(model_regr <- evaluate(ranger_regr, xgboost_regr,
                                            data_test = apartments,
                                            target = "m2.price",
                                            metric = "auc"))
  expect_error(model_classif <- evaluate(ranger_classif, xgboost_classif, 
                                               data_test = iris_bin,
                                               target = "Species", 
                                               metric = "rmse"))
})

test_that("Wrong type of object in model list ", {
  expect_error(model_regr <- evaluate(ranger_regr, 3, 
                                            data_test = apartments,
                                            target = "m2.price"))
  expect_error(model_classif <- evaluate(3, ranger_regr, 
                                               data_test = iris_bin,
                                               target = "Species"))
})

test_that("Mixed type of tasks ", {

  expect_error(model_regr <- evaluate(ranger_regr, xgboost_classif,
                                            data_test = apartments, 
                                            target = "m2.price"))
})

test_that("Empty list or no list ", {

  expect_error(model_regr <- evaluate(data_test = apartments, 
                                            target = "m2.price"), 
               "List of models is empty.")
})


