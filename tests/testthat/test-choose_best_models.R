test_that('choose_best_models', {
  data(iris)
  iris_bin          <- iris[1:100, ]
  iris_bin$Species  <- factor(iris_bin$Species)
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
  predictions <-
    predict_models(model,
                   test_data,
                   'Species',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)
  score <-
    score_models(model,
                 predictions,
                 observed = split_data$test$Species,
                 type = type,
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  best_models <-
    choose_best_models(model,
                       score = score,
                       engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                       number = 3)

  expect_equal(length(best_models$models), 3)
  expect_equal(length(best_models$engine), 3)
})
