#' Score models by suitable metrics
#'
#' @param models List of models trained by `train_models` function.
#' @param predictions List of predictions of every engine from test data.
#' @param observed Vector of true values from test data.
#' @param type String, determines if future task is `binary_clf` or `regression`.
#' @param metrics Vector of metrics names. For `NULL` most important metrics are returned.
#' For `all` all metrics are returned.
#' @param sort_by String with name of metric to sort by.
#' For `auto` models going to be sorted by 'mse' for regression and 'f1' for classification.
#'
#' @return A data.frame with 'no.' - number of model from models,
#' 'engine' - name of model from models, other metrics columns.
#' @export
#'
#' @examples
#' iris_bin <- iris[1:100, ]
#' iris_bin$Species <- factor(iris_bin$Species)
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
#' suppressWarnings(
#'   model <-
#'     train_models(train_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#' )
#' predictions <-
#'   predict_models(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#' score <-
#'   score_models(model,
#'                predictions,
#'                observed = split_data$test$Species,
#'                type = type)
#' @importFrom stats weighted.mean
score_models <- function(models,
                         predictions,
                         observed,
                         type,
                         metrics = NULL,
                         sort_by = 'auto')
{
  metrics_reggresion <- c('mse', 'rmse', 'r2', 'mad', 'mae')
  metrics_binary_clf <- c('f1', 'recall', 'precision', 'accuracy', 'auc')
  metrics_decreasing <- c('mse' = FALSE, 'rmse' = FALSE, 'r2' = TRUE,
                          'mad' = FALSE, 'mae' = FALSE, 'recall' = TRUE, 'precision' = TRUE,
                          'accuracy' = TRUE, 'auc' = TRUE, 'f1' = TRUE)



  if (is.null(metrics)) {
    if (type == 'regression') {
      metrics <- c('mse', 'r2', 'mad')
    }
    else if (type == 'binary_clf') {
      metrics <- c('f1', 'accuracy', 'recall')
    }
  } else if (metrics == 'all') {
    if (type == 'regression') {
      metrics <- metrics_reggresion
    }
    if (type =='binary_clf') {
      metrics <- metrics_binary_clf
    }
  }
  if (type == 'regression') {
    is_valid_metrics <- metrics %in% metrics_reggresion
    if (any(!is_valid_metrics)) {
      metrics <- metrics[is_valid_metrics]
      warning(paste('Not valid metrics ommited: ', paste(metrics[!is_valid_metrics], collapse = ', ')))
    }
    if (!(sort_by %in% metrics_reggresion)) {
      if (sort_by != 'auto') {
        warning(paste('sort_by need to by one of regression metrics. Choose one of the them: ',
                      paste(metrics_reggresion, collapse = ', ')))
      }
      sort_by <- 'mse'
    }
  } else if (type == 'binary_clf') {
    is_valid_metrics <- metrics %in% metrics_binary_clf
    if (any(!is_valid_metrics)){
      metrics <- metrics[is_valid_metrics]
      warning(paste('Not valid metrics ommited: ', paste(metrics[!is_valid_metrics], collapse = ', ')))
    }
    if (!(sort_by %in% metrics_binary_clf)) {
      if (sort_by != 'auto') {
        warning(paste('sort_by need to by one of binary classification metrics. Choose one of the them: ',
                      paste(metrics_binary_clf, collapse = ', ')))
      }
      sort_by <- 'f1'
    }
  }

  if (type == 'regression') {
    models_frame <- data.frame(matrix(nrow = length(models), ncol = length(metrics_reggresion) + 2))
    colnames(models_frame) <- c('no.', 'engine', metrics_reggresion)

    for(i in 1:length(models)) {
      models_frame[i, ] <- c(i,
                            names(models)[i],
                            model_performance_mse(unlist(predictions[i], use.names = FALSE),observed),
                            model_performance_rmse(unlist(predictions[i], use.names = FALSE), observed),
                            model_performance_r2(unlist(predictions[i], use.names = FALSE), observed),
                            model_performance_mad(unlist(predictions[i], use.names = FALSE), observed),
                            model_performance_mae(unlist(predictions[i], use.names = FALSE), observed)
      )
    }
    models_frame[, -2] <- sapply(models_frame[, -2], as.numeric)
    models_frame <- models_frame[
      order(
        models_frame[, sort_by],
        decreasing = metrics_decreasing[sort_by]
      ),
      c('no.', 'engine', metrics)
    ]

  } else if (type == 'binary_clf') {
    models_frame <- data.frame(matrix(nrow = length(models), ncol = length(metrics_binary_clf) + 2))
    colnames(models_frame) <- c('no.', 'engine', metrics_binary_clf)

    observed <- as.numeric(observed)
    for(i in 1:length(models)) {
      tp = sum((observed == 2) * (as.numeric(unlist(predictions[i])) >= 0.5))
      fp = sum((observed == 1) * (as.numeric(unlist(predictions[i])) >= 0.5))
      tn = sum((observed == 1) * (as.numeric(unlist(predictions[i])) < 0.5))
      fn = sum((observed == 2) * (as.numeric(unlist(predictions[i])) < 0.5))
      models_frame[i, ] <- c(i,
                            names(models[i]),
                            model_performance_f1(tp, fp, tn, fn),
                            model_performance_recall(tp, fp, tn, fn),
                            model_performance_precision(tp, fp, tn, fn),
                            model_performance_accuracy(tp, fp, tn, fn),
                            model_performance_auc(predictions[i], observed)
      )
    }
    models_frame[, -2] <- sapply(models_frame[, -2], as.numeric)
    models_frame <- models_frame[
      order(
        models_frame[, sort_by],
        decreasing = unname(metrics_decreasing[sort_by])
      ),
      c('no.', 'engine', metrics)
    ]

  }
  rownames(models_frame) <- NULL
  return(models_frame)
}

# this functions below are taken from DALEX package
model_performance_mse <- function(predicted, observed) {
  mean((predicted - observed) ^ 2, na.rm = TRUE)
}

model_performance_rmse <- function(predicted, observed) {
  sqrt(mean((predicted - observed) ^ 2, na.rm = TRUE))
}

model_performance_r2 <- function(predicted, observed) {
  1 - model_performance_mse(predicted, observed) / model_performance_mse(mean(observed), observed)
}

model_performance_mad <- function(predicted, observed) {
  median(abs(predicted - observed))
}

model_performance_mae <- function(predicted, observed) {
  mean(abs(predicted - observed))
}

model_performance_auc <- function(predicted, observed) {
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR <- c(0, cumsum(rev(tpr_tmp))) / sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR <- c(0, cumsum(rev(fpr_tmp))) / sum(1 - observed)

  auc <- sum(diff(FPR) * (TPR[-1] + TPR[-length(TPR)]) / 2)
  auc
}

model_performance_recall <- function(tp, fp, tn, fn) {
  tp / (tp + fn)
}

model_performance_precision <- function(tp, fp, tn, fn) {
  tp / (tp + fp)
}

model_performance_f1 <- function(tp, fp, tn, fn) {
  recall = tp / (tp + fn)
  precision = tp / (tp + fp)
  2 * (precision * recall) / (precision + recall)
}

model_performance_accuracy <- function(tp, fp, tn, fn) {
  (tp + tn) / (tp + fp + tn + fn)
}

model_performance_macro_f1 <- function(predicted, observed) {
  predicted_vectorized <- turn_probs_into_vector(predicted)
  confusion_matrixes <- calculate_confusion_matrixes(predicted_vectorized, observed)
  f1_scores <- sapply(confusion_matrixes, function(x) {
    model_performance_f1(x$tp, x$fp, x$tn, x$fn)
  })
  mean(f1_scores)
}

model_performance_micro_f1 <- function(predicted, observed) {
  # For case where each point can be assigned only to one class micro_f1 equals acc
  model_performance_accuracy_multi(predicted, observed)
}

model_performance_weighted_macro_f1 <- function(predicted, observed) {
  predicted_vectorized <- turn_probs_into_vector(predicted)
  confusion_matrixes <- calculate_confusion_matrixes(predicted_vectorized, observed)
  f1_scores <- sapply(confusion_matrixes, function(x) {
    model_performance_f1(x$tp, x$fp, x$tn, x$fn)
  })
  weighted.mean(f1_scores, prop.table(table(observed))[names(confusion_matrixes)])
}

model_performance_accuracy_multi <- function(predicted, observed) {
  predicted_vectorized <- turn_probs_into_vector(predicted)
  mean(predicted_vectorized == observed)
}

model_performance_weighted_macro_auc <- function(predicted, observed) {
  observed <- as.character(observed)
  auc_scores <- sapply(unique(observed), function(x) {
    model_performance_auc(predicted[,x], as.numeric(observed == x))
  })
  weighted.mean(auc_scores, prop.table(table(observed))[unique(observed)])
}

turn_probs_into_vector <- function(observed) {
  apply(observed, 1, function(x) {
    colnames(observed)[which.max(x)]
  })
}

calculate_confusion_matrixes <- function(predicted, observed) {
  observed <- as.character(observed)
  ret <- lapply(unique(observed), function(x) {
    tp <- mean(predicted[predicted == x] == observed[predicted == x])
    fp <- mean(predicted[predicted == x] != observed[predicted == x])
    tn <- mean(predicted[predicted != x] == observed[predicted != x])
    fn <- mean(predicted[predicted != x] != observed[predicted != x])
    list(tp = ifelse(is.nan(tp), 0, tp),
         fp = ifelse(is.nan(fp), 0, fp),
         tn = ifelse(is.nan(tn), 0, tn),
         fn = ifelse(is.nan(fn), 0, fn))
  })
  names(ret) <- unique(observed)
  return(ret)
}
