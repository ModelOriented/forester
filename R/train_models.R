#' Train models from given engines
#'
#' @param data A training data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`,`decision_tree`, `lightgbm`, `catboost`. Doesn't
#' matter for survival analysis.
#' @param type A string that determines if Machine Learning task is the
#' `binary_clf`, `regression`, or `survival`.
#'
#' @return A list of models for every engine.
#' @export
train_models <- function(data, y = NULL, time = NULL, status = NULL, engine, type) {
  if (is.null(y)) {
    rfsrc_model <- randomForestSRC::rfsrc(as.formula(paste0('Surv(',time,',', status,') ~ .')),
                                          data      = data$ranger_data,
                                          na.action = 'na.omit',
                                          ntree     = 500,
                                          nodesize  = 15,
                                          nsplit    = 10,
                                          splitrule = 'logrankscore')
    return_list  <- list(
      rfsrc_model = rfsrc_model
    )
  } else {
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
            data$ranger_data[[y]] <- as.numeric(data$ranger_data[[y]]) - 1
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
        form                <- as.formula(paste0(y, ' ~.'))
        decision_tree_model <- partykit::ctree(form, data = data$decision_tree_data)

      } else if (engine[i] == 'lightgbm') {
        # For each objective type, we need another set of params
        # setting up the parameters.
        if (type == 'binary_clf') {
          obj    <- 'binary'
          params <- list(objective = obj)
        } else if (type == 'multi_clf') {
          obj    <- 'multiclass'
          params <- list(objective = obj)
        } else if (type == 'regression') {
          obj    <- 'regression'
          params <- list(objective = obj)
        }

        lightgbm_model <- lightgbm::lgb.train(params = params,
                                              data = data$lightgbm_data,
                                              verbose = -1)

      } else if (engine[i] == 'catboost') {
        if (type == 'binary_clf') {
          obj    <- 'Logloss'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'multi_clf') {
          obj    <- 'MultiClass'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'regression') {
          obj    <- 'MAE'
          params <- list(loss_function = obj, logging_level = 'Silent')
        }
        capture.output(catboost_model <- catboost::catboost.train(data$catboost_data, params = params))
      }
    }

    # To remove models that are NULL.
    return_list <- list(
      ranger_model        = ranger_model,
      xgboost_model       = xgboost_model,
      decision_tree_model = decision_tree_model,
      lightgbm_model      = lightgbm_model,
      catboost_model      = catboost_model
    )

    to_rm <- c()
    for (i in 1:length(return_list)) {
      if (is.null(return_list[[i]])) {
        to_rm <- c(to_rm, i)
      }
    }
    if (!is.null(to_rm)) {
      return_list <- return_list[-to_rm]
    }
  }
  return(return_list)
}
