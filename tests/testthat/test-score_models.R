test_that('test-score_models', {
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
                 type = type)
  expect_true(all(dim(score) == c(5, 5)))
})


test_that('regresion scoring works', {
  type              <- guess_type(iris, 'Petal.Width')
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
                 type = type,
                 metrics = 'all',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 tuning = rep('test', 5))

  expect_true(all(dim(score) == c(5, 9)))
})


test_that('regresion scoring compas', {
  type              <- guess_type(compas, 'Two_yr_Recidivism')
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
                         type = type)
  )

  score_all      <- score_models(models = model,
                                 predictions = predictions,
                                 observed = test_data$ranger_data$Two_yr_Recidivism,
                                 type = type,
                                 metrics = 'all',
                                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                 tuning = rep('test', 5))
  score_auto     <- score_models(models = model,
                                 predictions = predictions,
                                 observed = test_data$ranger_data$Two_yr_Recidivism,
                                 type = type,
                                 sort_by = 'f1',
                                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                 tuning = rep('test', 5))
  score_warnings <- suppressWarnings(
   score_models(models = model,
                predictions = predictions,
                observed = test_data$ranger_data$Two_yr_Recidivism,
                type = type,
                metrics = c('auc', 'f1', 'wrong'),
                sort_by = 'wrong')
  )

  expect_true(all(dim(score_all) == c(5, 9)))
  expect_true(all(colnames(score_all) == c('no.', 'name', 'engine', 'tuning', 'auc', 'f1', 'recall', 'precision', 'accuracy')))
  expect_true(all(score_all$auc == score_all$auc[order(score_all$auc, decreasing = TRUE)]))

  expect_true(all(dim(score_auto) == c(5, 7)))
  expect_true(all(colnames(score_auto) == c('no.', 'name', 'engine', 'tuning', 'auc', 'f1', 'accuracy')))
  expect_true(all(score_auto$f1 == score_auto$f1[order(score_auto$f1, decreasing = TRUE)]))

  expect_warning(
    expect_warning(
      score_models(models = model, predictions = predictions, observed = test_data$ranger_data$Two_yr_Recidivism, type = type, metrics = c('auc', 'f1', 'wrong'), sort_by = 'wrong'),
      'Not valid metrics ommited:  wrong'
      ),
    'sort_by need to by one of binary classification metrics. Default metric applied : auc.'
    )
  expect_true(all(dim(score_warnings) == c(5, 4)))
  expect_true(all(colnames(score_warnings) == c('no.', 'name', 'auc', 'f1')))
  expect_true(all(score_warnings$auc == score_warnings$auc[order(score_warnings$auc, decreasing = TRUE)]))



  good_fun      <- function(predicted, observed) {sum(abs(predicted - observed))}
  warning_fun   <- function(predicted, observed) {warning('warning_fun')}
  error_fun     <- function(prdicted, observed) {stop('error_fun') }

  score_good    <- score_models(models = model,
                                predictions = predictions,
                                observed = test_data$ranger_data$Two_yr_Recidivism,
                                type = type,
                                metrics = 'all',
                                metric_function = good_fun,
                                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                                tuning = rep('test', 5))
  score_warinig <- suppressWarnings(
    score_models(models = model,
                 predictions = predictions,
                 observed = test_data$ranger_data$Two_yr_Recidivism,
                 type = type,
                 metrics = 'auto',
                 metric_function = warning_fun,
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 tuning = rep('test', 5))
    )
  score_error   <- suppressWarnings(
    score_models(models = model,
                 predictions = predictions,
                 observed = test_data$ranger_data$Two_yr_Recidivism,
                 type = type,
                 metrics = 'all',
                 metric_function = error_fun,
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 tuning = rep('test', 5))
    )
  score_name    <- score_models(models = model,
                               predictions = predictions,
                               observed = test_data$ranger_data$Two_yr_Recidivism,
                               type = type,
                               metrics = c('f1', 'metric_function'),
                               metric_function = good_fun,
                               metric_function_name = 'works',
                               metric_function_decreasing = FALSE,
                               engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                               tuning = rep('test', 5))

  expect_true(all(dim(score_good) == c(5, 10)))
  expect_true(all(colnames(score_good) == c('no.', 'name', 'engine', 'tuning', 'metric_function', 'auc', 'f1', 'recall', 'precision', 'accuracy')))
  expect_true((all(is.numeric(score_good$metric_function))))
  expect_true(all(score_good$metric_function == score_good$metric_function[order(score_good$metric_function, decreasing = TRUE)]))

  expect_true(all(dim(score_warinig) == c(5, 8)))
  expect_true(all(colnames(score_warinig) == c('no.', 'name', 'engine', 'tuning', 'metric_function', 'auc', 'f1', 'accuracy')))
  expect_true((all(is.na(score_warinig$metric_function))))

  expect_true(all(dim(score_error) == c(5, 10)))
  expect_true(all(colnames(score_error) == c('no.', 'name', 'engine', 'tuning', 'metric_function', 'auc', 'f1', 'recall', 'precision', 'accuracy')))
  expect_true((all(is.na(score_error$metric_function))))

  expect_true(all(dim(score_name) == c(5, 6)))
  expect_true(all(colnames(score_name) == c('no.', 'name', 'engine', 'tuning', 'f1', 'works')))
  expect_true((all(is.numeric(score_name$works))))
  expect_true(all(score_name$works == score_name$works[order(score_name$works, decreasing = FALSE)]))
})
