test_that('binary scoring works', {
  iris_bin <- iris[1:100, ]
  iris_bin$Species <- factor(iris_bin$Species)
  type <- guess_type(iris_bin, 'Species')
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
                 type = type)
  expect_true(all(dim(score) == c(5, 5)))
})
test_that('regresion scoring works', {
  type <- guess_type(iris, 'Petal.Width')
  preprocessed_data <- preprocessing(iris, 'Petal.Width')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       'Petal.Width',
                       type = type,
                       balance = FALSE)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   'Petal.Width',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  test_data <-
    prepare_data(split_data$test,
                 'Petal.Width',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)
  suppressWarnings(
    model <-
      train_models(train_data,
                   'Petal.Width',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type = type)
  )
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     'Petal.Width',
                     engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                     type = type)
  )
  score <-
    score_models(model,
                 predictions,
                 observed = split_data$test$Petal.Width,
                 type = type)

  expect_true(all(dim(score) == c(5, 5)))
})
