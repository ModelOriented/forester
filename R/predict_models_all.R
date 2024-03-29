#' Predictions for a list of models with multiple occurrences of the same types of models
#'
#' @param models A list of models trained by `train_models()` function.
#' @param data A test data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param type A string that determines if Machine Learning task is the
#' `binary_clf`, `regression`, `survival`, or `multiclass` task.
#'
#' @return A list of predictions for every engine without names.
#' @export
#' @importFrom stats as.formula predict
predict_models_all <- function(models, data, y, type) {
  predictions <- list(1:length(models))
  engine_all  <- c()
  to_rm       <- c()
  # Elimination of null models (artifacts of not selected engines).
  for (i in 1:length(models)) {
    if (is.null(models[[i]])) {
      to_rm <- c(to_rm, i)
    }
  }
  if (!is.null(to_rm)) {
    models <- models[-to_rm]
  }

  if (type == 'survival') {
    for (i in 1:length(models)) {
      predictions[i] <- list(randomForestSRC::predict.rfsrc(models[[i]], data$ranger_data))
    }
    names(predictions) <- names(models)
    return(predictions)
  } else {
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
      names(predictions) <- names(models)
      return(predictions)

    } else if (type == 'binary_clf') {
      for (i in 1:length(models)) {
        # There was issue of 1 column entry from the prediction made beforehand in train pipeline.
        if (engine[i] == 'ranger') {
          preds <- ranger::predictions(predict(models[[i]], data$ranger_data))
          if (is.null(ncol(preds))) {
            predictions[i] <- list(preds - 1)
          } else {
            predictions[i] <- list(preds[, 2])
          }
        } else if (engine[i] == 'xgboost') {
          predictions[i] <- list(predict(models[[i]], data$xgboost_data))

        } else if (engine[i] == 'decision_tree') {
          predictions[i] <- list(unname(predict(models[[i]], data$decision_tree_data, type = 'prob')[, 2]))

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
      names(predictions) <- names(models)
      return(predictions)
    } else if (type == 'multiclass') {
      for (i in 1:length(models)) {
        if (engine[i] == 'ranger') {
          predicts <- ranger::predictions(predict(models[[i]], data$ranger_data))

        } else if (engine[i] == 'xgboost') {
          predicts <- predict(models[[i]], data$xgboost_data)
          predicts <- matrix(predicts, nrow = nrow(data$xgboost_data), ncol = length(unique(data$ranger_data[[y]])), byrow = TRUE)

        } else if (engine[i] == 'decision_tree') {
          predicts <- unname(predict(models[[i]], data$decision_tree_data, type = 'prob'))

        } else if (engine[i] == 'lightgbm') {
          predicts <- predict(models[[i]], data$lightgbm_data)

        } else if (engine[i] == 'catboost') {
          predicts <- catboost::catboost.predict(models[[i]], data$catboost_data,
                                                 prediction_type = 'Probability')
        }
        preds <- c()
        for (j in 1:nrow(predicts)) {
          preds <- c(preds, which.max(unname(predicts[j, ])))
        }
        predictions[i] <- list(preds)
      }
      names(predictions) <- names(models)
      return(predictions)
    }
  }
}
