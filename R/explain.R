#' Explain forester model
#'
#' The `explain()` function is a wrapper for `DALEX` methods for model explanations.
#' If possible it will use methods for tree-based models.
#'
#' @param models A single model created with the `train()` function or a list of
#' models.
#' @param test_data A test dataset returned from `train()` function.
#' @param y A target variable. It can be either
#' (1) a vector of the same number of observations as `data` or
#' (2) a character name of variable in the `data` that contains
#' the target variable.
#' @param verbose A logical value determining whether explainer creation messages
#' should be printed or not.
#'
#' @return A list of DALEX explainers for 5 models of different engines.
#' @export
#'
#' @examples
#' data(lisbon)
#' train_output   <- train(lisbon, 'Price', verbose = FALSE, random_evals = 2, bayes_iter = 1)
#' best_explainer <- explain(train_output$best_models[[1]], train_output$data, train_output$y)
explain <- function(models, test_data, y, verbose = FALSE) {
  type <- guess_type(test_data$ranger_data, y)

  single_model <- FALSE
  if (class(models)[1] %in% c('ranger', 'xgb.Booster', 'lgb.Booster', 'constparty', 'catboost.Model')) {
    single_model <- TRUE
    verbose_cat('Explaining a single object', verbose = verbose)
    n <- 1
  } else {
    n <- length(models)
  }
  explainers <- list()

  for (i in 1:n) {
    if (single_model) {
      m_class <- class(models)[1]
    } else {
      m_class <- class(models[[i]])
    }

    if ('ranger' %in% m_class) {
      engine <- 'ranger'
    } else if ('xgb.Booster' %in% m_class) {
      engine <- 'xgboost'
    } else if ('constparty' %in% m_class) {
      engine <- 'decision_tree'
    } else if ('lgb.Booster' %in% m_class) {
      engine <- 'lightgbm'
    } else if ('catboost.Model' %in% m_class) {
      engine <- 'catboost'
    }

    if ('ranger' == engine) {
      exp_data   <- test_data$ranger_data
      target     <- as.numeric(test_data$ranger_data[[y]])
      test_data$ranger_data[[y]] <- as.numeric(test_data$ranger_data[[y]])

      if (single_model) {
        explainer <- DALEX::explain(model = models,
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      } else {
        explainer <- DALEX::explain(model = models[[i]],
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      }
    }
    if ('xgboost' == engine) {
      exp_data <- test_data$xgboost_data
      target   <- as.numeric(test_data$ranger_data[[y]])
      test_data$ranger_data[[y]] <- as.numeric(test_data$ranger_data[[y]])

      if (type == 'binary_clf') {
        target <- as.integer(target)
      }

      if (single_model) {
        explainer <- DALEX::explain(model = models,
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      } else {
        explainer <- DALEX::explain(model = models[[i]],
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      }
    }
    if ('decision_tree' == engine) {
      exp_data <- test_data$decision_tree_data
      target   <- as.numeric(test_data$ranger_data[[y]])
      test_data$ranger_data[[y]] <- as.numeric(test_data$ranger_data[[y]])

      if (single_model) {
        explainer <- DALEX::explain(model = models,
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      } else {
        explainer <- DALEX::explain(model = models[[i]],
                                                  data = exp_data,
                                                  y = as.vector(target),
                                                  verbose = verbose)
      }
    }
    if ('lightgbm' == engine) {
      exp_data <- test_data$lightgbm_data
      target   <- as.numeric(test_data$ranger_data[[y]])
      test_data$ranger_data[[y]] <- as.numeric(test_data$ranger_data[[y]])

      if (type == 'binary_clf') {
        target <- as.integer(target) - 1
      }

      if (single_model) {
        explainer <- DALEX::explain(model = models,
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      } else {
        explainer <- DALEX::explain(model = models[[i]],
                                    data = exp_data,
                                    y = as.vector(target),
                                    verbose = verbose)
      }
    }
    if ('catboost' == engine) {
      exp_data <- test_data$catboost_data
      target   <- as.numeric(test_data$ranger_data[[y]])
      test_data$ranger_data[[y]] <- as.numeric(test_data$ranger_data[[y]])

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
        explainer <- DALEX::explain(model = models,
                                    data = exp_data,
                                    y = as.vector(target),
                                    predict_function = pred_catboost,
                                    verbose = verbose)
      } else {
        explainer <- DALEX::explain(model = models[[i]],
                                    data = exp_data,
                                    y = as.vector(target),
                                    predict_function = pred_catboost,
                                    verbose = verbose)
      }
    }
    if (single_model) {
      explainers <- explainer
    }
    else {
      explainers[[names(models)[[i]]]] <- explainer
    }
  }

  return(
    explainers
  )
}
