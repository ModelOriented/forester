#' Predicts models depending on the engine
#'
#' As all machine learning models have different predicting pipelines, we have to
#' provide a useful tool for normalization of making predictions.
#'
#' @param models A list of models trained by `train_models` function.
#' @param data A test data for models created by `prepare_data`.
#' @param y A string which indicates a target column name.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`, `decision tree`, `lightgbm`, `catboost`.
#' @param type A string which determines if machine learning task is the
#' `classification` or `regression`.
#' @param probability A logical value which determines whether the output for
#' classification task should be 0/1 or described by probability.
#'
#' @return A list of predictions for every engine.
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin <- iris[1:100,]
#' type <- guess_type(iris_bin, 'Species')
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
#'   predict_models(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#'
#'@importFrom stats as.formula predict
predict_models <- function(models,
                           data,
                           y,
                           engine,
                           type,
                           probability = FALSE) {
  ranger_preds        <- NULL
  xgboost_preds       <- NULL
  decision_tree_preds <- NULL
  lightgbm_preds      <- NULL
  catboost_preds      <- NULL

  if (type == 'regression') {
    for (i in 1:length(engine)) {
      if (engine[i] == 'ranger') {
        ranger_preds <- (predict(models[[i]], data$ranger_data))$predictions

      }
      if (engine[i] == 'xgboost') {
        xgboost_preds <- (predict(models[[i]], data$xgboost_data))

      }
      if (engine[i] == 'decision_tree') {
        decision_tree_preds <-
          unname(predict(models[[i]], data$decision_tree_data))

      }
      if (engine[i] == 'lightgbm') {
        lightgbm_preds <- (predict(models[[i]], data$lightgbm_data))

      }
      if (engine[i] == 'catboost') {
        catboost_preds <- (
          catboost::catboost.predict(models[[i]],
                                     data$catboost_data,
                                     prediction_type = 'RawFormulaVal')
        )
      }
    }
  } else if (type == 'binary_clf') {
    for (i in 1:length(engine)) {
      if (engine[i] == 'ranger') {
        ranger_preds <-
          (ranger::predictions(predict(models[[i]], data$ranger_data))[, 2])

      }
      if (engine[i] == 'xgboost') {
        xgboost_preds <- (predict(models[[i]], data$xgboost_data))

      }
      if (engine[i] == 'decision_tree') {
        decision_tree_preds <-
          (unname(
            predict(models[[i]], data$decision_tree_data, type = 'prob')[, 2]
          ))

      }
      if (engine[i] == 'lightgbm') {
        lightgbm_preds <- (predict(models[[i]], data$lightgbm_data))

      }
      if (engine[i] == 'catboost') {
        catboost_preds <- (
          catboost::catboost.predict(models[[i]],
                                     data$catboost_data,
                                     prediction_type = 'RawFormulaVal')
        )
      }
    }
  }

  if (type == 'binary_clf' && probability == FALSE) {
    treshold <- 0.5
    ranger_preds        <- (ranger_preds >= treshold) + 1
    decision_tree_preds <- (decision_tree_preds >= treshold) + 1
    catboost_preds      <- (catboost_preds >= treshold) + 1
    xgboost_preds       <- (xgboost_preds >= treshold) + 1
    lightgbm_preds      <- (lightgbm_preds >= treshold) + 1
  }

  return(
    list(
      ranger_preds        = ranger_preds,
      xgboost_preds       = xgboost_preds,
      decision_tree_preds = decision_tree_preds,
      lightgbm_preds      = lightgbm_preds,
      catboost_preds      = catboost_preds
    )
  )

}
