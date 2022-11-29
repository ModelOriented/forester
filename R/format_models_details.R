#' Format info about models
#'
#' @param models The result of `choose_best_models()` function or just a list of models
#' from engine.
#'
#' @return Prints formatted text.
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
#' test_data <- prepare_data(split_data$test,
#'             'Species',
#'              engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'              predict = TRUE,
#'              train = split_data$train)
#' models <-  train_models(train_data,
#'        'Species',
#'        engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'        type = type)
#' predictions <-
#'   predict_models(models,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#' score <-
#'     score_models(models,
#'                  predictions,
#'                  observed = split_data$test$Species,
#'                  type = type)
#' best_models <- choose_best_models(models,
#'               score = score,
#'               number = 3)
#' format_models_details(best_models)
format_models_details <- function(models) {
  for (i in 1:length(models)) {
    if (grepl('ranger', names(models[i]))) {
      cat(paste0('------------ Ranger model ------------\n\n',
                 'Parameters\n',
                 '  num.trees: ', models[[i]]$num.trees, '\n',
                 '  ntry: ', models[[i]]$mtry, '\n',
                 '  num.samples: ', models[[i]]$num.samples, '\n',
                 '  min.node.size: ', models[[i]]$min.node.size,
                 '\n\n'))
    }
    if (grepl('xgboost', names(models[i]))) {
      # Functions for getting params of xgboost model are not implementented!
      eval_log <- '    iter : train_rmse\n'
      for (j in 1:length(models[[i]]$evaluation_log$iter)) {
        eval_log <- paste(eval_log, '    ', models[[i]]$evaluation_log$iter[j], ': ', models[[i]]$evaluation_log$train_rmse[j], '\n')
      }
      cat(paste0('------------ Xgboost model ------------\n\n',
                 'Parameters\n',
                 '  niter: ', models[[i]]$niter, '\n',
                 '  evaluation_log: \n', eval_log,
                 '\n\n'))
    }
    if (grepl('decision_tree', names(models[i]))) {
      list_rules_party <- utils::getFromNamespace('.list.rules.party', 'partykit')
      a <- as.vector(list_rules_party(models[[i]]))
      # The method above doesn't rise a note in check()
      # a <- as.vector(partykit:::.list.rules.party(models[[i]]))
      rules <- NULL
      for (j in 1:length(a)) {
        rules <- paste(rules, j, ': ', a[j], '\n')
      }
      cat(paste0('------------ Decision tree model ------------\n\n',
                 'The list of rules: \n', rules,
                 '\n\n'))
    }
    if (grepl('lightgbm', names(models[i]))) {
      # No method implemented to get parameters of model.
      params <- models[[i]]$params
      cat(paste0('------------ Lightgbm model ------------\n\n',
                 'Parameters\n',
                 '  num_iterations: ', params$num_iterations, '\n',
                 '\n\n'))
    }
    if (grepl('catboost', names(models[i]))) {
      params <- catboost::catboost.get_model_params(models[[i]])
      cat(paste0('------------ Catboost model ------------\n\n',
                 'Parameters\n',
                 '  depth: ', params$tree_learner_options$depth, '\n',
                 '  learning_rate: ', models[[i]]$learning_rate, '\n',
                 '  iterations: ', params$boosting_options$iterations, '\n',
                 '  border_count: ', params$cat_feature_params$simple_ctrs$ctr_binarization$border_count[1], '\n',
                 '\n\n'))
    }
  }
}
