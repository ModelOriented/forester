#' Explain forester model
#'
#' The `explain()` is a wrapper for `DALEX` methods for model explanations.
#' If possible it will use methods for tree based models.
#'
#' @param model A single model created with the `train()` function or a list of
#' up to 5 unique models (unique = of different engine).
#' @param data A test dataset returned from `train()` function.
#' @param y A target variable. It can be either
#' (1) a vector of the same number of observations as `data` or
#' (2) a character name of variable in the `data` that contains
#' the target variable.
#' @param engine A vector of tree-based models that shall be testes.
#' Possible values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#' All models from this vector will be trained and the best one will be returned.
#' @param verbose A logical value determining whether explainer creation messages
#' should be printed or not.
#'
#' @return A list of DALEX explainers for 5 models of different engines.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DALEX)
#' library(magrittr)
#' titanic_imputed %>%
#'     train('survived') %>%
#'     forester::explain()
#' }
explain <- function(model, data, y, engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'), verbose = FALSE) {

  ranger_explainer        <- NULL
  xgboost_explainer       <- NULL
  decision_tree_explainer <- NULL
  lightgbm_explainer      <- NULL
  catboost_explainer      <- NULL
  type                    <- guess_type(data$ranger_data, y)

  single_model <- FALSE

  if (class(model)[1] %in% c('ranger', 'xgb.Booster', 'lgb.Booster', 'constparty', 'catboost.Model')) {
    single_model <- TRUE
  }

  if ('ranger' %in% engine) {
    exp_data   <- data$ranger_data
    target     <- data$ranger_data[[y]]

    if (single_model) {
      ranger_explainer <- DALEX::explain(model = model,
                                         data = exp_data,
                                         y = as.vector(target),
                                         verbose = verbose)
    } else {
      ranger_explainer <- DALEX::explain(model = model$ranger_model,
                                         data = exp_data,
                                         y = as.vector(target),
                                         verbose = verbose)
    }


  }
  if ('xgboost' %in% engine) {
    exp_data <- data$xgboost_data
    target   <- data$ranger_data[[y]]

    if (type == 'binary_clf') {
      target <- as.integer(target)
    }

    if (single_model) {
      xgboost_explainer <- DALEX::explain(model = model,
                                          data = exp_data,
                                          y = as.vector(target),
                                          verbose = verbose)
    } else {
      xgboost_explainer <- DALEX::explain(model = model$xgboost_model,
                                          data = exp_data,
                                          y = as.vector(target),
                                          verbose = verbose)
    }
  }
  if ('decision_tree' %in% engine) {
    exp_data <- data$decision_tree_data
    target   <- data$ranger_data[[y]]

    if (single_model) {
      decision_tree_explainer <- DALEX::explain(model = model,
                                                data = exp_data,
                                                y = as.vector(target),
                                                verbose = verbose)
    } else {
      decision_tree_explainer <- DALEX::explain(model = model$decision_tree_model,
                                                data = exp_data,
                                                y = as.vector(target),
                                                verbose = verbose)
    }
  }
  if ('lightgbm' %in% engine) {
    exp_data <- data$lightgbm_data
    target   <- data$ranger_data[[y]]

    if (type == 'binary_clf') {
      target <- as.integer(target) - 1
    }

    if (single_model) {
      lightgbm_explainer <- DALEX::explain(model = model,
                                           data = exp_data,
                                           y = as.vector(target),
                                           verbose = verbose)
    } else {
      lightgbm_explainer <- DALEX::explain(model = model$lightgbm_model,
                                           data = exp_data,
                                           y = as.vector(target),
                                           verbose = verbose)
    }
  }
  if ('catboost' %in% engine) {
    exp_data <- data$catboost_data
    target   <- data$ranger_data[[y]]

    if (type == 'binary_clf') {
      if (max(target) > 1) {
        target <- target - 1
      }
      pred_catboost <- function(model, data) {
        as.numeric(catboost::catboost.predict(model,
                                              data,
                                              prediction_type = 'Probability'))
      }
    } else {
      pred_catboost <- function(model, data) {
        as.numeric(catboost::catboost.predict(model,
                                              data,
                                              prediction_type = 'RawFormulaVal'))
      }
    }

    if (single_model) {
      catboost_explainer <- DALEX::explain(model = model,
                                           data = exp_data,
                                           y = as.vector(target),
                                           predict_function = pred_catboost,
                                           verbose = verbose)
    } else {
      catboost_explainer <- DALEX::explain(model = model$catboost_model,
                                           data = exp_data,
                                           y = as.vector(target),
                                           predict_function = pred_catboost,
                                           verbose = verbose)
    }

  }
  return(
    list(
      ranger_explainer        = ranger_explainer,
      xgboost_explainer       = xgboost_explainer,
      decision_tree_explainer = decision_tree_explainer,
      lightgbm_explainer      = lightgbm_explainer,
      catboost_explainer      = catboost_explainer
    )
  )
}
