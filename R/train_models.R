#' Train models from given engines
#'
#' @param data A training data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`,`decision_tree`, `lightgbm`, `catboost`.
#' @param type A string that determines if Machine Learning task is the
#' `classification` or `regression`.
#'
#' @return A list of models for every engine.
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin          <- iris[1:100, ]
#' type              <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species')
#' preprocessed_data <- preprocessed_data$data
#' split_data <-
#'   train_test_balance(preprocessed_data, 'Species', type = type, balance = FALSE)
#' train_data <-
#'   prepare_data(split_data$train,
#'                'Species',
#'                c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
#' model <-
#'   train_models(train_data,
#'                'Species',
#'                c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                type)
train_models <- function(data, y, engine, type) {
  ranger_model        <- NULL
  xgboost_model       <- NULL
  decision_tree_model <- NULL
  lightgbm_model      <- NULL
  catboost_model      <- NULL

  if (type == 'multi_clf') {
    stop('Multilabel classification is not supported currently!')
  }

  for (i in 1:length(engine)) {
    if (engine[i] == 'ranger') {
      if (type == 'binary_clf') {
        ranger_model <-
          ranger::ranger(dependent.variable.name = y,
                         data = data$ranger_data,
                         classification = TRUE,
                         probability = TRUE)
      } else if (type == 'regression') {
        ranger_model <-
          ranger::ranger(dependent.variable.name = y,
                         data = data$ranger_data)
      }

    } else if (engine[i] == 'xgboost') {
      if (type == 'binary_clf') {
        if (any(data$ranger_data[[y]] == 2)) {
          data$ranger_data[[y]] = data$ranger_data[[y]] - 1
        }
      xgboost_model <-
        xgboost::xgboost(data$xgboost_data,
                         as.vector(data$ranger_data[[y]]),
                         objective = 'binary:logistic',
                         nrounds = 20,
                         verbose = 0,
                         eval_metric = 'auc')
      } else if (type == 'regression'){
        xgboost_model <-
          xgboost::xgboost(data$xgboost_data,
                           as.vector(data$ranger_data[[y]]),
                           nrounds = 20,
                           verbose = 0)
      }

    } else if (engine[i] == 'decision_tree') {
      form = as.formula(paste0(y, ' ~.'))
      decision_tree_model <- partykit::ctree(form, data = data$decision_tree_data)

    } else if (engine[i] == 'lightgbm') {
      # For each objective type, we need another set of params
      # setting up the parameters.
      if (type == 'binary_clf') {
        obj = 'binary'
        params <- list(objective = obj)
      } else if (type == 'multi_clf') {
        obj = 'multiclass'
        params <- list(objective = obj)
      } else if (type == 'regression') {
        obj = 'regression'
        params <- list(objective = obj)
      }

      lightgbm_model <- lightgbm::lgb.train(params = params,
                                            data = data$lightgbm_data,
                                            verbose = -1)

    } else if (engine[i] == 'catboost') {
      if (type == 'binary_clf') {
        obj = 'Logloss'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'multi_clf') {
        obj = 'MultiClass'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'regression') {
        obj = 'MAE'
        params <- list(loss_function = obj, logging_level = 'Silent')
      }
      capture.output(catboost_model <- catboost::catboost.train(data$catboost_data, params = params))
    }
  }
  return(
    list(
      ranger_model        = ranger_model,
      xgboost_model       = xgboost_model,
      decision_tree_model = decision_tree_model,
      lightgbm_model      = lightgbm_model,
      catboost_model      = catboost_model
    )
  )
}
