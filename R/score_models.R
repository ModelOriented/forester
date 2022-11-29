#' Score models by suitable metrics
#'
#' @param models A list of models trained by `train_models()` function.
#' @param predictions A list of predictions of every engine from the test data.
#' @param observed A vector of true values from the test data.
#' @param type A string, determines if the future task is `binary_clf` or `regression`.
#' @param metrics A vector of metrics names. By default param set for `auto`, most important metrics are returned.
#' For `all` all metrics are returned. For `NULL` no metrics returned but still sorted by `sort_by`.
#' @param sort_by String with name of metric to sort by.
#' For `auto` models going to be sorted by `mse` for regression and `f1` for classification.
#' @param metric_function The self-created function.
#' It should look like name(predictions, observed) and return the numeric value.
#' In case of using `metrics` param with value other than `auto` or `all`, is needed to use value `metric_function`
#' in order to see given metric in report. If `sort_by` is equal to `auto` models are sorted by `metric_function`.
#' @param metric_function_name The name of the column with values of param `metric_function`.
#' By default `metric_function_name` is `metric_function`.
#' @param metric_function_decreasing A logical value indicating how metric_function should be sorted. `TRUE` by default.
#' @param engine A vector of strings containing information of engine in `models` list.
#' @param tuning A vector of strings containing information of tuning method in `models` list.
#'
#' @return A data.frame with 'no.' - number of model from models,
#' 'engine' - name of the model from models, other metrics columns.
#' @export
#'
#' @examples
#' iris_bin          <- iris[1:100, ]
#' iris_bin$Species  <- factor(iris_bin$Species)
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
                         metrics = 'auto',
                         sort_by = 'auto',
                         metric_function = NULL,
                         metric_function_name = NULL,
                         metric_function_decreasing = TRUE,
                         engine = NULL,
                         tuning = NULL) {
  metrics_reggresion <- c('mse', 'rmse', 'r2', 'mad', 'mae')
  metrics_binary_clf <- c('auc', 'f1', 'recall', 'precision', 'accuracy')
  metrics_decreasing <- c('mse' = FALSE, 'rmse' = FALSE, 'r2' = TRUE,
                          'mad' = FALSE, 'mae' = FALSE, 'recall' = TRUE, 'precision' = TRUE,
                          'accuracy' = TRUE, 'auc' = TRUE, 'f1' = TRUE, 'metric_function' = metric_function_decreasing)

  metrics <- c(metrics)
  colnames_basic <- c('no.', 'name')
  nr_add_col <- 2
  if (!is.null(engine)) {
    nr_add_col <- nr_add_col + 1
    colnames_basic <- c(colnames_basic, 'engine')
  }
  if (!is.null(tuning)) {
    nr_add_col <- nr_add_col + 1
    colnames_basic <- c(colnames_basic, 'tuning')
  }

    if (is.null(metric_function)) {
    metric_function_name <- NULL
  } else {
    metrics_reggresion <- c('metric_function', metrics_reggresion)
    metrics_binary_clf <- c('metric_function', metrics_binary_clf)

    if (is.null(metric_function_name)) {
      metric_function_name <- 'metric_function'
    }
    if (sort_by == 'auto') {
      sort_by <- 'metric_function'
    }
  }

  if ('auto' %in% metrics) {
    if (type == 'regression') {
      metrics <- c(if (!is.null(metric_function)) {'metric_function'},
                   'mse', 'r2', 'mae')
    }
    else if (type == 'binary_clf') {
      metrics <- c(if (!is.null(metric_function)) {'metric_function'},
                   'auc', 'f1', 'accuracy')
    }
  } else if ('all' %in% metrics) {
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
        warning(paste('sort_by need to by one of regression metrics. Default metric applied : mse.
                      Choose one of the them: auto, metric_function (in case of usage custom function) ',
                      paste(metrics_reggresion, collapse = ', ')))
      }
      sort_by <- 'mse'
    }
  } else if (type == 'binary_clf') {
    is_valid_metrics <- metrics %in% metrics_binary_clf
    if (!all(is_valid_metrics)) {
      warning(paste('Not valid metrics ommited: ', paste(metrics[!is_valid_metrics], collapse = ', ')))
      metrics <- metrics[is_valid_metrics]
    }
    if (!(sort_by %in% metrics_binary_clf)) {
      if (sort_by != 'auto') {
        warning(paste('sort_by need to by one of binary classification metrics. Default metric applied : auc.
                      You can choose one of the them: auto, metric_function (in case of usage custom function) ',
                      paste(metrics_binary_clf, collapse = ', ')))
      }
        sort_by <- 'auc'
    }
  }
  if (type == 'regression') {
      models_frame           <- data.frame(matrix(nrow = length(models), ncol = length(metrics_reggresion) + nr_add_col))
      colnames(models_frame) <- c(colnames_basic, metrics_reggresion)

      for (i in 1:length(models)) {

        models_frame[i, ] <- c(i,
                               names(models)[i],
                               engine[i],
                               tuning[i],
                               if (!is.null(metric_function)) {metric_function_null(metric_function, predictions[[1]], observed - 1)},
                               model_performance_mse(unlist(predictions[i], use.names = FALSE), observed),
                               model_performance_rmse(unlist(predictions[i], use.names = FALSE), observed),
                               model_performance_r2(unlist(predictions[i], use.names = FALSE), observed),
                               model_performance_mad(unlist(predictions[i], use.names = FALSE), observed),
                               model_performance_mae(unlist(predictions[i], use.names = FALSE), observed)
                               )
    }
  } else if (type == 'binary_clf') {
      models_frame           <- data.frame(matrix(nrow = length(models), ncol = length(metrics_binary_clf) + nr_add_col))
      colnames(models_frame) <- c(colnames_basic, metrics_binary_clf)

      observed <- as.numeric(observed)
      for (i in 1:length(models)) {
        tp = sum((observed == 2) * (as.numeric(unlist(predictions[i])) >= 0.5))
        fp = sum((observed == 1) * (as.numeric(unlist(predictions[i])) >= 0.5))
        tn = sum((observed == 1) * (as.numeric(unlist(predictions[i])) < 0.5))
        fn = sum((observed == 2) * (as.numeric(unlist(predictions[i])) < 0.5))

        models_frame[i, ] <- c(i,
                               names(models[i]),
                               engine[i],
                               tuning[i],
                               if (!is.null(metric_function)) {metric_function_null(metric_function, predictions[[i]], observed - 1)},
                               model_performance_auc(predictions[i], observed - 1),
                               model_performance_f1(tp, fp, tn, fn),
                               model_performance_recall(tp, fp, tn, fn),
                               model_performance_precision(tp, fp, tn, fn),
                               model_performance_accuracy(tp, fp, tn, fn)
                               )
    }
  }
  models_frame[, -c(2:4)] <- sapply(models_frame[, -c(2:4)], as.numeric)
  models_frame            <- models_frame[order(models_frame[, sort_by],
                                          decreasing = unname(metrics_decreasing[sort_by])),
                                          c(colnames_basic, metrics)]

  if (!is.null(metric_function)) {
    colnames(models_frame)[colnames(models_frame) == 'metric_function'] <- metric_function_name
  }

  rownames(models_frame) <- NULL

  return(models_frame)
}

metric_function_null <- function(metric_function, predicted, observed) {
  if ('function' %in% class(metric_function)) {
    return(try(metric_function(predicted, observed), silent = TRUE))
  } else
    return(NA)
}

# Functions below are taken from the DALEX package.
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
  confusion_matrixes   <- calculate_confusion_matrixes(predicted_vectorized, observed)
  f1_scores            <- sapply(confusion_matrixes, function(x) {
    model_performance_f1(x$tp, x$fp, x$tn, x$fn)
  })
  mean(f1_scores)
}

model_performance_micro_f1 <- function(predicted, observed) {
  # For case where each point can be assigned only to one class micro_f1 equals acc.
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
  observed   <- as.character(observed)
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
