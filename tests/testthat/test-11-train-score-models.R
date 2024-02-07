test_that('test-train-score-models', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_prepared_data.RData', '/testing_data_prepared_data.RData',
               '/iris_prepared_data.RData',   '/compas_prepared_data.RData',
               '/lisbon_models.RData',        '/testing_data_models.RData',
               '/iris_models.RData',          '/compas_models.RData',
               '/lisbon_predictions.RData',   '/testing_data_predictions.RData',
               '/iris_predictions.RData',     '/compas_predictions.RData',
               '/tuning.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  # No catboost due to the error with loading the learn_pool.
  engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  prepared_data <- list(lisbon_prepared_data, testing_data_prepared_data, iris_prepared_data, compas_prepared_data)
  models        <- list(lisbon_models, testing_data_models, iris_models, compas_models)
  predictions   <- list(lisbon_predictions, testing_data_predictions, iris_predictions, compas_predictions)

  metrics <- c('auto', 'all')

  for (i in 1:length(prepared_data)) {
    for (metric in metrics) {
      score_train <- score_models(models      = models[[i]]$models_all,
                                  predictions = predictions[[i]]$predict_train,
                                  observed    = prepared_data[[i]]$train_data$ranger_data[[targets[i]]],
                                  data        = prepared_data[[i]]$train_data,
                                  type        = types[i],
                                  metrics     = metric,
                                  sort_by     = 'auto',
                                  engine      = models[[i]]$engine_all,
                                  tuning      = tuning)

      score_test  <- score_models(models      = models[[i]]$models_all,
                                  predictions = predictions[[i]]$predict_test,
                                  observed    = prepared_data[[i]]$test_data$ranger_data[[targets[i]]],
                                  data        = prepared_data[[i]]$test_data,
                                  type        = types[i],
                                  metrics     = metric,
                                  sort_by     = 'auto',
                                  engine      = models[[i]]$engine_all,
                                  tuning      = tuning)

      score_valid <- score_models(models      = models[[i]]$models_all,
                                  predictions = predictions[[i]]$predict_valid,
                                  observed    = prepared_data[[i]]$valid_data$ranger_data[[targets[i]]],
                                  data        = prepared_data[[i]]$valid_data,
                                  type        = types[i],
                                  metrics     = metric,
                                  sort_by     = 'auto',
                                  engine      = models[[i]]$engine_all,
                                  tuning      = tuning)

      # Checking if every model gets evaluated.
      expect_equal(nrow(score_train), length(models[[i]]$models_all))
      expect_equal(nrow(score_test),  length(models[[i]]$models_all))
      expect_equal(nrow(score_valid), length(models[[i]]$models_all))

      # Checking if ranked list is properly sorted.
      if (types[i] == 'regression') {
        decreasing <- FALSE
      } else {
        decreasing <- TRUE
      }
      expect_identical(score_train[, 5][sort.list(score_train[, 5], decreasing = decreasing)], score_train[, 5])
      expect_identical(score_test[, 5][sort.list(score_test[, 5],  decreasing = decreasing)], score_test[, 5])
      expect_identical(score_valid[, 5][sort.list(score_valid[, 5], decreasing = decreasing)], score_valid[, 5])

      # Checking if both metric types work well.
      if (metric == 'auto') {
        expect_true(ncol(score_train) %in% c(8, 7))
        expect_true(ncol(score_test)  %in% c(8, 7))
        expect_true(ncol(score_valid) %in% c(8, 7))
      } else {
        expect_true(ncol(score_train) %in% c(9, 12, 14))
        expect_true(ncol(score_test)  %in% c(9, 12, 14))
        expect_true(ncol(score_valid) %in% c(9, 12, 14))
      }
    }
  }


})
