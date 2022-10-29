test_that('test-train_models_bayesopt', {
  data(iris)
  iris_bin          <- iris[1:100, ]
  type              <- guess_type(iris_bin, 'Species')
  preprocessed_data <- preprocessing(iris_bin, 'Species')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       'Species',
                       type = type,
                       balance = FALSE)
  train_data <-
    prepare_data(split_data$train,
                 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))

  test_data <-
    prepare_data(split_data$test,
                 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <- train_models_bayesopt(train_data,
                                   'Species',
                                   test_data,
                                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                   type = type,
                                   iters.n = 1,
                                   return_params = FALSE,
                                   verbose = FALSE)
  )

  # Iris dataset for classification.
  expect_true(length(model) == 5)
  expect_true(class(model$ranger_bayes) == 'ranger')
  expect_true(class(model$xgboost_bayes) == 'xgb.Booster')
  expect_true(identical(class(model$decision_tree_bayes), c('constparty', 'party')))
  expect_true(class(model$lightgbm_bayes)[1] == 'lgb.Booster')
  expect_true(class(model$catboost_bayes) == 'catboost.Model')

  model <- train_models_bayesopt(train_data,
                                 'Species',
                                 test_data,
                                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                 type = type,
                                 iters.n = 1,
                                 return_params = TRUE,
                                 verbose = FALSE)
  expect_true(length(model) == 6)


  # Compas dataset for classification.
  type              <- guess_type(compas, 'Two_yr_Recidivism')
  preprocessed_data <- preprocessing(compas, 'Two_yr_Recidivism')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Two_yr_Recidivism',
                       type = type,
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'Two_yr_Recidivism',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')))
  test_data <-
    prepare_data(split_data$test,
                 'Two_yr_Recidivism',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <- train_models_bayesopt(train_data,
                                   'Two_yr_Recidivism',
                                   test_data,
                                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                   type = type,
                                   iters.n = 1,
                                   verbose = FALSE))

  expect_true(length(model) == 5)
  expect_true(class(model$ranger_bayes) == 'ranger')
  expect_true(class(model$xgboost_bayes) == 'xgb.Booster')
  expect_true(identical(class(model$decision_tree_bayes), c('constparty', 'party')))
  expect_true(class(model$lightgbm_bayes)[1] == 'lgb.Booster')
  expect_true(class(model$catboost_bayes) == 'catboost.Model')


  # Lisbon dataset for regression.
  type                <- guess_type(lisbon, 'Price')

  suppressWarnings(
    preprocessed_data <- preprocessing(lisbon, 'Price')
  )
  preprocessed_data   <- preprocessed_data$data

  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Price',
                       type = type,
                       balance = FALSE)
  suppressWarnings(
    train_data <- prepare_data(split_data$train,
                               y = 'Price',
                               engine = c('ranger', 'xgboost', 'decision_tree',
                                          'lightgbm', 'catboost')))

  test_data <-
    prepare_data(split_data$test,
                 'Price',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <- train_models_bayesopt(train_data,
                                   'Price',
                                   test_data,
                                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                   type = type,
                                   iters.n = 1,
                                   verbose = FALSE))

  expect_true(length(model) == 5)
  expect_true(class(model$ranger_bayes) == 'ranger')
  expect_true(class(model$xgboost_bayes) == 'xgb.Booster')
  expect_true(identical(class(model$decision_tree_bayes), c('constparty', 'party')))
  expect_true(class(model$lightgbm_bayes)[1] == 'lgb.Booster')
  expect_true(class(model$catboost_bayes) == 'catboost.Model')
})
