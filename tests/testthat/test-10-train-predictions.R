test_that('test-train-predictions', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_prepared_data.RData', '/testing_data_prepared_data.RData',
               '/iris_prepared_data.RData',   '/compas_prepared_data.RData',
               '/lisbon_models.RData', '/testing_data_models.RData',
               '/iris_models.RData',   '/compas_models.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  # No catboost due to the error with loading the learn_pool.
  engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  prepared_data <- list(lisbon_prepared_data, testing_data_prepared_data, iris_prepared_data, compas_prepared_data)
  models        <- list(lisbon_models, testing_data_models, iris_models, compas_models)
  predictions   <- list()
  for (i in 1:length(prepared_data)) {
    predict_train    <- predict_models_all(models = models[[i]]$models_all,
                                           data   = prepared_data[[i]]$raw_train,
                                           y      = targets[i],
                                           type   = types[i])
    predict_test     <- predict_models_all(models = models[[i]]$models_all,
                                           data   = prepared_data[[i]]$test_data,
                                           y      = targets[i],
                                           type   = types[i])
    predict_valid    <- predict_models_all(models = models[[i]]$models_all,
                                           data   = prepared_data[[i]]$valid_data,
                                           y      = targets[i],
                                           type   = types[i])

    predictions[[i]] <- list(predict_train = predict_train,
                             predict_test  = predict_test,
                             predict_valid = predict_valid)

    expect_equal(length(predict_train), length(predict_test))
    expect_equal(length(predict_train), length(predict_valid))

    for (j in 1:length(predict_train)) {
      # If there are any nulls.
      expect_true(sum(is.null(predict_train[[j]])) == 0)
      expect_true(sum(is.null(predict_test[[j]]))  == 0)
      expect_true(sum(is.null(predict_valid[[j]])) == 0)

      # If every observation got a prediction.
      expect_true(length(predict_train[[j]]) == nrow(prepared_data[[i]]$raw_train$ranger_data))
      expect_true(length(predict_test[[j]])  == nrow(prepared_data[[i]]$test_data$ranger_data))
      expect_true(length(predict_valid[[j]]) == nrow(prepared_data[[i]]$valid_data$ranger_data))

      # If all outputs are numeric.
      expect_true(is.numeric(predict_train[[j]]))
      expect_true(is.numeric(predict_test[[j]]))
      expect_true(is.numeric(predict_valid[[j]]))
    }
  }

  for (j in 1:length(predict_train)) {
    # If we have correct data types for each task.
    # Multiclass.
    expect_true(length(unique(predictions[[3]]$predict_train[[j]])) <= 3)
    expect_true(length(unique(predictions[[3]]$predict_test[[j]]))  <= 3)
    expect_true(length(unique(predictions[[3]]$predict_valid[[j]])) <= 3)
    # Binary.
    expect_true(sum(predictions[[4]]$predict_train[[j]] > 1) == 0)
    expect_true(sum(predictions[[4]]$predict_train[[j]] < 0) == 0)
    expect_true(sum(predictions[[4]]$predict_test[[j]]  > 1) == 0)
    expect_true(sum(predictions[[4]]$predict_test[[j]]  < 0) == 0)
    expect_true(sum(predictions[[4]]$predict_valid[[j]] > 1) == 0)
    expect_true(sum(predictions[[4]]$predict_valid[[j]] < 0) == 0)
  }

  lisbon_predictions       <- predictions[[1]]
  testing_data_predictions <- predictions[[2]]
  iris_predictions         <- predictions[[3]]
  compas_predictions       <- predictions[[4]]

  save(lisbon_predictions, file = capture.output(cat(folder, '/lisbon_predictions.RData', sep='')))
  save(testing_data_predictions, file = capture.output(cat(folder, '/testing_data_predictions.RData', sep='')))
  save(iris_predictions, file = capture.output(cat(folder, '/iris_predictions.RData', sep='')))
  save(compas_predictions, file = capture.output(cat(folder, '/compas_predictions.RData', sep='')))
})
