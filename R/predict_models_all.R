#' Predictions for a list of models with multiple occurrences of the same types of models
#'
#' @param models A list of models trained by `train_models()` function.
#' @param data A test data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`,`decision_tree`, `lightgbm`, `catboost`.
#' @param type A string that determines if Machine Learning task is the
#' `classification` or `regression` task.
#'
#' @return A list of predictions for every engine without names.
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin          <- iris[1:100, ]
#' type              <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species')
#' preprocessed_data <- preprocessed_data$data
#' split_data <-
#'   train_test_balance(preprocessed_data,
#'                      'Species',
#'                      type = type,
#'                      balance = FALSE)
#' train_data <-
#'   prepare_data(split_data$train,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
#' test_data <-
#'   prepare_data(split_data$test,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                predict = TRUE,
#'                train = split_data$train)
#'
#' model <-
#'   train_models(train_data,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                type = type)
#' predictions <-
#'   predict_models_all(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
predict_models_all <- function(models, data, y, engine, type) {
  predictions <- list(1:length(models))

  engine_all <- c()
  for (i in 1:length(models)) {
    m_class <- class(models[[i]])
    if ('ranger' %in% m_class) {
      eng <- 'ranger'
    } else if ('xgb.Booster' %in% m_class) {
      eng <- 'xgboost'
    } else if ('constparty' %in% m_class) {
      eng <- 'decision_tree'
    } else if ('lgb.Booster' %in% m_class) {
      eng <- 'lightgbm'
    } else if ('catboost.Model' %in% m_class) {
      eng <- 'catboost'
    }
    engine_all <- c(engine_all, eng)
  }
  engine <- engine_all

  if (type == 'regression') {
    for (i in 1:length(models)) {
      if (engine[i] == 'ranger') {
        predictions[i] <-
          list(ranger::predictions(predict(models[[i]], data$ranger_data)))

      } else if (engine[i] == 'xgboost') {
        predictions[i] <-
          list(predict(models[[i]], data$xgboost_data))

      } else if (engine[i] == 'decision_tree') {
        predictions[i] <-
          list(unname(predict(
            models[[i]], data$decision_tree_data
          )))

      } else if (engine[i] == 'lightgbm') {
        predictions[i] <- list(predict(models[[i]], data$lightgbm_data))

      } else if (engine[i] == 'catboost') {
        predictions[i] <- list(
          catboost::catboost.predict(models[[i]],
                                     data$catboost_data,
                                     prediction_type = 'RawFormulaVal')
        )
      }
    }
    return(predictions)

  } else if (type == 'binary_clf') {
    for (i in 1:length(models)) {
      if (engine[i] == 'ranger') { # There was issue of 1 column entry from the prediction made beforehand in train pipeline.
        preds <- ranger::predictions(predict(models[[i]], data$ranger_data))
        if (is.null(ncol(preds))) {
          predictions[i] <- list(preds - 1)
        } else {
          predictions[i] <- list(preds[, 2])
        }
      } else if (engine[i] == 'xgboost') {
        predictions[i] <- list(predict(models[[i]], data$xgboost_data))

      } else if (engine[i] == 'decision_tree') {
        predictions[i] <-
          list(unname(
            predict(models[[i]], data$decision_tree_data, type = 'prob')[, 2]
          ))

      } else if (engine[i] == 'lightgbm') {
        predictions[i] <- list(predict(models[[i]], data$lightgbm_data))

      } else if (engine[i] == 'catboost') {
        predictions[i] <- list(
          catboost::catboost.predict(models[[i]],
                                     data$catboost_data,
                                     prediction_type = 'Probability')
        )
      }
    }
    return(predictions)
  }
}
