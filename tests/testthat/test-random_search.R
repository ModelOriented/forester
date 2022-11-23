test_that('test-random_search', {
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
    model <-
      train_models(train_data,
                   'Species',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)
  )

  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     'Species',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type = type)
  )

  score <- score_models(model, predictions, test_data$ranger_data$Species, type)

  suppressWarnings(
    random_best <- random_search(train_data,
                                 test_data,
                                 y = 'Species',
                                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                 type = type,
                                 max_evals = 4,
                                 nr_return_models = 'all')
  )
  expect_true(length(random_best$engine) == 20)
  expect_true(length(random_best$models) == 20)
})
