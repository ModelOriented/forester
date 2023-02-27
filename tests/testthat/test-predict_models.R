test_that('test-predict_models', {
  # Iris dataset for classification.
  iris_bin          <- iris[1:100, ]
  iris_bin$Species  <- factor(iris_bin$Species)
  type              <- guess_type(iris_bin, 'Species')
  preprocessed_data <- preprocessing(iris_bin, 'Species', type = type)
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Species',
                       balance = FALSE)
  train_data <-
    prepare_data(split_data$train,
                 y = 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  test_data <-
    prepare_data(split_data$test,
                 y = 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 split_data$train)

  suppressWarnings(
    model <-
      train_models(train_data,
                   'Species',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)
  )
  predictions <-
    predict_models(model,
                   test_data,
                   y = 'Species',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)


  expect_true(length(predictions) == 5)
  expect_true(length(predictions$ranger_preds) == 20)
  expect_true(length(predictions$xgboost_preds) == 20)
  expect_true(length(predictions$decision_tree_preds) == 20)
  expect_true(length(predictions$lightgbm_preds) == 20)
  expect_true(length(predictions$catboost_preds) == 20)


  # Compas dataset for classification.
  type              <- guess_type(compas, 'Two_yr_Recidivism')
  preprocessed_data <- preprocessing(compas, 'Two_yr_Recidivism', type = type)
  preprocessed_data <- preprocessed_data$data
  set.seed(123)
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Two_yr_Recidivism',
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'Two_yr_Recidivism',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 y = 'Two_yr_Recidivism',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y = 'Two_yr_Recidivism',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type)
  )
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y = 'Two_yr_Recidivism',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type)
  )

  expect_true(length(predictions) == 5)
  expect_true(length(predictions$ranger_preds) == 1235)
  expect_true(length(predictions$xgboost_preds) == 1235)
  expect_true(length(predictions$decision_tree_preds) == 1235)
  expect_true(length(predictions$lightgbm_preds) == 1235)
  expect_true(length(predictions$catboost_preds) == 1235)


  # Tests for lisbon dataset.
  type                <- guess_type(lisbon, 'Price')
  suppressWarnings(
    preprocessed_data <- preprocessing(lisbon, 'Price', type = type)
  )
  preprocessed_data   <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Price',
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'Price',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 y = 'Price',
                 c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y = 'Price',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type)
  )
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y = 'Price',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type)
  )

  expect_true(length(predictions) == 5)
  expect_true(length(predictions$ranger_preds) == 50)
  expect_true(length(predictions$xgboost_preds) == 50)
  expect_true(length(predictions$decision_tree_preds) == 50)
  expect_true(length(predictions$lightgbm_preds) == 50)
  expect_true(length(predictions$catboost_preds) == 50)


  # Tests for regression.
  type              <- guess_type(testing_data, 'y')
  preprocessed_data <- preprocessing(testing_data, 'y', type = type)
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'y',
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'y',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 y = 'y',
                 c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y = 'y',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type)
  )
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y = 'y',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type)
  )

  expect_true(length(predictions) == 5)
  expect_true(length(predictions$ranger_preds) == 200)
  expect_true(length(predictions$xgboost_preds) == 200)
  expect_true(length(predictions$decision_tree_preds) == 200)
  expect_true(length(predictions$lightgbm_preds) == 200)
  expect_true(length(predictions$catboost_preds) == 200)


  # Tests for regression.
  type              <- guess_type(adult[1:100, ], 'salary')
  preprocessed_data <- preprocessing(adult[1:100, ], 'salary', type = type)
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'salary',
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'salary',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 y = 'salary',
                 c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y = 'salary',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type)
  )
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y = 'salary',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type)
  )

  expect_true(length(predictions) == 5)
  expect_true(length(predictions$ranger_preds) == 20)
  expect_true(length(predictions$xgboost_preds) == 20)
  expect_true(length(predictions$decision_tree_preds) == 20)
  expect_true(length(predictions$lightgbm_preds) == 20)
  expect_true(length(predictions$catboost_preds) == 20)
})
