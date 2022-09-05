test_that("predict_models_all", {
  data(compas)
  data(lisbon)
  data(iris)

  # iris - classification
  iris_bin <- iris[1:100, ]
  type <- guess_type(iris_bin, 'Species')
  preprocessed_data <- preprocessing(iris_bin, 'Species')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Species',
                       type = type,
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
    predict_models_all(model,
                   test_data,
                   y = 'Species',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)


  expect_true(length(predictions) == 5)
  expect_true(all(lapply(predictions, length) == 20))
  expect_true(range(predictions)[1] >= 0)
  expect_true(range(predictions)[2] <= 1)

  # compas - classification
  type <- guess_type(compas, 'Two_yr_Recidivism')
  preprocessed_data <- preprocessing(compas, 'Two_yr_Recidivism')
  preprocessed_data <- preprocessed_data$data
  set.seed(123)
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Two_yr_Recidivism',
                       type = type,
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
                   type = type)
  )
  suppressWarnings(
    predictions <-
      predict_models_all(model,
                     test_data,
                     y = 'Two_yr_Recidivism',
                     engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                     type = type)
  )

  expect_true(length(predictions) == 5)
  expect_true(all(lapply(predictions, length) == 1235))
  expect_true(range(predictions)[1] >= 0)
  # expect_true(range(predictions)[2] <= 1)

  # lisbon

  type <- guess_type(lisbon,'Price')
  suppressWarnings(preprocessed_data <- preprocessing(lisbon, 'Price'))
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Price',
                       type = type,
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'Price',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 y = 'Price',c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y = 'Price',
                   engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                   type = type)
  )
  suppressWarnings(
    predictions <-
      predict_models_all(model,
                     test_data,
                     y = 'Price',
                     engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                     type = type)
  )


  expect_true(length(predictions)==5)
  expect_true(length(predictions[[1]]) == 50)
  expect_true(length(predictions[[2]]) == 50)
  expect_true(length(predictions[[3]]) == 50)
  expect_true(length(predictions[[4]]) == 50)
  expect_true(length(predictions[[5]]) == 50)
})
