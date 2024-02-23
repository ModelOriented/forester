#' Score models by suitable metrics
#'
#' @param models A list of models trained by `train_models()` function.
#' @param predictions A list of predictions of every engine from the test data.
#' @param observed A vector of true values from the test data.
#' @param data A data for models created by `prepare_data()` function, used for
#' Brier score calculations.
#' @param type A string, determines if the future task is `binary_clf`, `regression`, `survival`, or `multiclass`.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used.
#' @param metrics A vector of metrics names. By default param set for `auto`,
#' most important metrics are returned. For `all` all metrics are returned.
#' For `NULL` no metrics returned but still sorted by `sort_by`. The metrics
#' available for the binary classification are: `auc`, `f1`, `recall`, `precision`,
#' `accuracy`, `sensitivity`, `specificity`, `balanced_accuracy`, for the
#' regression: `mse`, `rmse`, `r2`, `mad`, `mae`, for the survival analysis:
#' `Brier Score`, `Concordance Index (CIN)`, and for the multiclass classification: `accuracy`,
#' `micro_averaged_precision`, `micro_averaged_recall`, `micro_averaged_f1`,
#' `macro_averaged_precision`, `macro_averaged_recall`, `macro_averaged_f1`,
#' `weighted_averaged_precision`, `weighted_averaged_recall`, `weighted_averaged_f1`.
#' @param sort_by String with name of metric to sort by. For `auto` models going
#' to be sorted by `rmse` for regression and `accuracy` for both classification tasks.
#' @param metric_function The self-created function.
#' It should look like name(predictions, observed) and return the numeric value.
#' In case of using `metrics` param with value other than `auto` or `all`,
#' is needed to use value `metric_function` in order to see given metric in report.
#' If `sort_by` is equal to `auto` models are sorted by `metric_function`.
#' @param metric_function_name The name of the column with values of param
#' `metric_function`. By default `metric_function_name` is `metric_function`.
#' @param metric_function_decreasing A logical value indicating how metric_function
#' should be sorted. `TRUE` by default.
#' @param engine A vector of strings containing information of engine in `models` list.
#' @param tuning A vector of strings containing information of tuning method in `models` list.
#'
#' @return A data.frame with 'no.' - number of model from models,
#' 'engine' - name of the model from models, other metrics columns.
#' @export
#' @importFrom stats weighted.mean
score_models <- function(models,
                         predictions,
                         observed,
                         data,
                         type,
                         time    = NULL,
                         status  = NULL,
                         metrics = 'auto',
                         sort_by = 'auto',
                         metric_function = NULL,
                         metric_function_name = NULL,
                         metric_function_decreasing = TRUE,
                         engine = NULL,
                         tuning = NULL) {
  if (type == 'survival') {

    metrics            <- c('Brier Score', 'Concordance Index (CIN)')
    metrics_decreasing <- c('Brier Score' = FALSE, 'Concordance Index (CIN)' = TRUE)
    # metrics            <- c('Brier Score', 'Concordance Index (CIN)',
    #                         'Integrated Absolute Error (IAE)', 'Integrated Square Error (ISE)')
    # metrics_decreasing <- c('Brier' = FALSE, 'Concordance Index (CIN)' = TRUE,
    #                         'Integrated Absolute Error (IAE)' = FALSE,
    #                         'Integrated Square Error (ISE)' = FALSE)
    colnames_basic     <- c('no.', 'name', 'engine', 'tuning')
    nr_add_col         <- 4

    if (!(sort_by %in% metrics)) {
      if (sort_by != 'auto') {
        warning(paste('sort_by need to by one of regression metrics. Default metric applied : Brier Score.
                      Choose one of the them: auto, ', paste(metrics_reggresion, collapse = ', ')))
      }
      sort_by <- 'Brier Score'
    }

    models_frame           <- data.frame(matrix(nrow = length(models), ncol = length(metrics) + nr_add_col))
    colnames(models_frame) <- c(colnames_basic, metrics)

    for (i in 1:length(models)) {
      pred          <- randomForestSRC::predict.rfsrc(models[[i]], data$ranger_data)
      predictions   <- pred$survival
      ordered_times <- models[[i]]$time.interest
      median_idx    <- median(1:length(ordered_times))
      surv_object   <- survival::Surv(data$ranger_data[[time]], data$ranger_data[[status]])
      med_time      <- median(ordered_times)

      brier         <- SurvMetrics::Brier(object = surv_object, pre_sp = predictions[, ceiling(ncol(predictions) / 2)], t_star = med_time)
      cin           <- SurvMetrics::Cindex(object = surv_object, predictions[, ceiling(ncol(predictions) / 2)])
      #iaeise        <- SurvMetrics::IAEISE(object = surv_object, predictions, ordered_times)
      #iae           <- iaeise[1]
      #ise           <- iaeise[2]

      models_frame[i, ] <- c(i,
                             names(models)[i],
                             'rfsrc',
                             tuning[i],
                             brier,
                             cin)

      models_frame <- models_frame[order(models_frame[, sort_by],
                                         decreasing = unname(metrics_decreasing[sort_by])),
                                   c(colnames_basic, metrics)]
    }

  # Classification and regression
  } else {
    metrics_reggresion <- c('mse', 'rmse', 'r2', 'mad', 'mae')
    metrics_binary_clf <- c('accuracy', 'auc', 'f1', 'recall', 'precision',
                            'sensitivity', 'specificity', 'balanced_accuracy')
    metrics_multiclass <- c('accuracy',
                            'micro_averaged_precision',    'micro_averaged_recall',    'micro_averaged_f1',
                            'macro_averaged_precision',    'macro_averaged_recall',    'macro_averaged_f1',
                            'weighted_precision', 'weighted_recall', 'weighted_f1')
    metrics_decreasing <- c('mse' = FALSE, 'rmse' = FALSE, 'r2' = TRUE,
                            'mad' = FALSE, 'mae'  = FALSE, 'recall'   = TRUE, 'precision'   = TRUE,
                            'accuracy'    = TRUE, 'auc' = TRUE, 'f1'  = TRUE, 'sensitivity' = TRUE,
                            'specificity' = TRUE, 'balanced_accuracy' = TRUE,
                            'micro_averaged_precision'    = TRUE, 'micro_averaged_recall'    = TRUE, 'micro_averaged_f1' = TRUE,
                            'macro_averaged_precision'    = TRUE, 'macro_averaged_recall'    = TRUE, 'macro_averaged_f1' = TRUE,
                            'weighted_precision' = TRUE, 'weighted_recall' = TRUE, 'weighted_f1' = TRUE,
                            'metric_function' = metric_function_decreasing)

    metrics        <- c(metrics)
    colnames_basic <- c('no.', 'name')
    nr_add_col     <- 2
    if (!is.null(engine)) {
      nr_add_col     <- nr_add_col + 1
      colnames_basic <- c(colnames_basic, 'engine')
    }
    if (!is.null(tuning)) {
      nr_add_col     <- nr_add_col + 1
      colnames_basic <- c(colnames_basic, 'tuning')
    }
    if (is.null(metric_function)) {
      metric_function_name <- NULL
    } else {
      metrics_reggresion <- c('metric_function', metrics_reggresion)
      metrics_binary_clf <- c('metric_function', metrics_binary_clf)
      metrics_multiclass <- c('metric_function', metrics_multiclass)

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
                     'rmse', 'mse', 'r2', 'mae')
      } else if (type == 'binary_clf') {
        metrics <- c(if (!is.null(metric_function)) {'metric_function'},
                     'accuracy', 'auc', 'f1')
      } else if (type == 'multiclass') {
        metrics <- c(if (!is.null(metric_function)) {'metric_function'},
                     'accuracy', 'weighted_precision',
                     'weighted_recall', 'weighted_f1')
      }
    } else if ('all' %in% metrics) {
      if (type == 'regression') {
        metrics <- metrics_reggresion
      } else if (type =='binary_clf') {
        metrics <- metrics_binary_clf
      } else if (type =='multiclass') {
        metrics <- metrics_multiclass
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
          warning(paste('sort_by need to by one of regression metrics. Default metric applied : rmse.
                      Choose one of the them: auto, metric_function (in case of usage custom function) ',
                      paste(metrics_reggresion, collapse = ', ')))
        }
        sort_by <- 'rmse'
      }
    } else if (type == 'binary_clf') {
      is_valid_metrics <- metrics %in% metrics_binary_clf
      if (!all(is_valid_metrics)) {
        warning(paste('Not valid metrics ommited: ', paste(metrics[!is_valid_metrics], collapse = ', ')))
        metrics <- metrics[is_valid_metrics]
      }
      if (!(sort_by %in% metrics_binary_clf)) {
        if (sort_by != 'auto') {
          warning(paste('sort_by need to by one of binary classification metrics. Default metric applied : accuracy.',
                        'You can choose one of the them: auto, metric_function (in case of usage custom function) ',
                        paste(metrics_binary_clf, collapse = ', ')))
        }
        sort_by <- 'accuracy'
      }
    } else if (type == 'multiclass') {
      is_valid_metrics <- metrics %in% metrics_multiclass
      if (!all(is_valid_metrics)) {
        warning(paste('Not valid metrics ommited: ', paste(metrics[!is_valid_metrics], collapse = ', ')))
        metrics <- metrics[is_valid_metrics]
      }
      if (!(sort_by %in% metrics_multiclass)) {
        if (sort_by != 'auto') {
          warning(paste('sort_by need to by one of binary classification metrics. Default metric applied : accuracy.',
                        'You can choose one of the them: auto, metric_function (in case of usage custom function) ',
                        paste(metrics_multiclass, collapse = ', ')))
        }
        sort_by <- 'accuracy'
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
                               if (!is.null(metric_function)) {metric_function_null(metric_function, predictions[[i]], observed)},
                               model_performance_mse( unlist(predictions[[i]], use.names = FALSE), observed),
                               model_performance_rmse(unlist(predictions[[i]], use.names = FALSE), observed),
                               model_performance_r2(  unlist(predictions[[i]], use.names = FALSE), observed),
                               model_performance_mad( unlist(predictions[[i]], use.names = FALSE), observed),
                               model_performance_mae( unlist(predictions[[i]], use.names = FALSE), observed)
        )
      }
    } else if (type == 'binary_clf') {
      models_frame           <- data.frame(matrix(nrow = length(models), ncol = length(metrics_binary_clf) + nr_add_col))
      colnames(models_frame) <- c(colnames_basic, metrics_binary_clf)
      set.seed(1)
      observed <- as.numeric(observed)
      for (i in 1:length(models)) {
        tp <- sum((observed == 2) * (as.numeric(unlist(predictions[[i]])) >= 0.5))
        fp <- sum((observed == 1) * (as.numeric(unlist(predictions[[i]])) >= 0.5))
        tn <- sum((observed == 1) * (as.numeric(unlist(predictions[[i]])) < 0.5))
        fn <- sum((observed == 2) * (as.numeric(unlist(predictions[[i]])) < 0.5))

        models_frame[i, ] <- c(i,
                               names(models[i]),
                               engine[i],
                               tuning[i],
                               if (!is.null(metric_function)) {metric_function_null(metric_function, predictions[[i]], observed - 1)},
                               model_performance_accuracy(tp, fp, tn, fn),
                               model_performance_auc(predictions[[i]], observed - 1),
                               model_performance_f1(tp, fp, tn, fn),
                               model_performance_recall(tp, fp, tn, fn),
                               model_performance_precision(tp, fp, tn, fn),
                               model_performance_sensitivity(tp, fp, tn, fn),
                               model_performance_specificity(tp, fp, tn, fn),
                               model_performance_balanced_accuracy(tp, fp, tn, fn)
        )
      }
    } else if (type == 'multiclass') {
      models_frame           <- data.frame(matrix(nrow = length(models), ncol = length(metrics_multiclass) + nr_add_col))
      colnames(models_frame) <- c(colnames_basic, metrics_multiclass)
      set.seed(1)
      observed <- as.numeric(observed)
      for (i in 1:length(models)) {
        models_frame[i, ] <- c(i,
                               names(models[i]),
                               engine[i],
                               tuning[i],
                               if (!is.null(metric_function)) {metric_function_null(metric_function, predictions[[i]], observed - 1)},
                               model_performance_accuracy_multi(    predictions[[i]], observed),
                               model_performance_micro_precision(   predictions[[i]], observed),
                               model_performance_micro_recall(      predictions[[i]], observed),
                               model_performance_micro_f1(          predictions[[i]], observed),
                               model_performance_macro_precision(   predictions[[i]], observed),
                               model_performance_macro_recall(      predictions[[i]], observed),
                               model_performance_macro_f1(          predictions[[i]], observed),
                               model_performance_weighted_precision(predictions[[i]], observed),
                               model_performance_weighted_recall(   predictions[[i]], observed),
                               model_performance_weighted_f1(       predictions[[i]], observed)
        )
      }
    }
    models_frame[, -c(2:4)] <- sapply(models_frame[, -c(2:4)], as.numeric)
    models_frame            <- models_frame[order(models_frame[, sort_by], decreasing = unname(metrics_decreasing[sort_by])),
                                            c(colnames_basic, metrics)]
    if (!is.null(metric_function)) {
      colnames(models_frame)[colnames(models_frame) == 'metric_function'] <- metric_function_name
    }
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

# Regression metrics.
model_performance_mse <- function(predicted, observed) {
  return(mean((predicted - observed) ^ 2, na.rm = TRUE))
}

model_performance_rmse <- function(predicted, observed) {
  return(sqrt(mean((predicted - observed) ^ 2, na.rm = TRUE)))
}

model_performance_r2 <- function(predicted, observed) {
  return(1 - model_performance_mse(predicted, observed) / model_performance_mse(mean(observed), observed))
}

model_performance_mad <- function(predicted, observed) {
  return(median(abs(predicted - observed)))
}

model_performance_mae <- function(predicted, observed) {
  return(mean(abs(predicted - observed)))
}


# Binary classification metrics.
model_performance_auc <- function(predicted, observed) {
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR     <- c(0, cumsum(rev(tpr_tmp))) / sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR     <- c(0, cumsum(rev(fpr_tmp))) / sum(1 - observed)
  auc     <- sum(diff(FPR) * (TPR[-1] + TPR[-length(TPR)]) / 2)
  return(auc)
}

model_performance_recall <- function(tp, fp, tn, fn) {
  return(tp / (tp + fn))
}

model_performance_precision <- function(tp, fp, tn, fn) {
  return(tp / (tp + fp))
}

model_performance_f1 <- function(tp, fp, tn, fn) {
  recall    <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  return(2 * (precision * recall) / (precision + recall))
}

model_performance_accuracy <- function(tp, fp, tn, fn) {
  return((tp + tn) / (tp + fp + tn + fn))
}

model_performance_sensitivity <- function(tp, fp, tn, fn) {
  return((tp) / (tp + fn))
}

model_performance_specificity <- function(tp, fp, tn, fn) {
  return((tn) / (fp + tn))
}

model_performance_balanced_accuracy <- function(tp, fp, tn, fn) {
  return((model_performance_sensitivity(tp, fp, tn, fn) + model_performance_specificity(tp, fp, tn, fn)) / 2)
}


# Multiclass classification metrics.

# Precision.
model_performance_macro_precision <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  scores               <- sapply(confusion_matrixes, function(x) {
    model_performance_precision(x$tp, x$fp, x$tn, x$fn)
  })
  return(mean(scores, na.rm = TRUE))
}

model_performance_micro_precision <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  summed_matrix        <- confusion_matrixes[[1]]
  for (i in 2:length(confusion_matrixes)) {
    summed_matrix$tp <- summed_matrix$tp + confusion_matrixes[[i]]$tp
    summed_matrix$fp <- summed_matrix$fp + confusion_matrixes[[i]]$fp
    summed_matrix$tn <- summed_matrix$tn + confusion_matrixes[[i]]$tn
    summed_matrix$fn <- summed_matrix$fn + confusion_matrixes[[i]]$fn
  }
  return(model_performance_precision(summed_matrix$tp, summed_matrix$fp, summed_matrix$tn, summed_matrix$fn))
}

model_performance_weighted_precision <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  scores               <- sapply(confusion_matrixes, function(x) {
    model_performance_precision(x$tp, x$fp, x$tn, x$fn)
  })
  return(weighted.mean(scores, prop.table(table(observed))[names(confusion_matrixes)], na.rm = TRUE))
}

# Recall.
model_performance_macro_recall <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  scores               <- sapply(confusion_matrixes, function(x) {
    model_performance_recall(x$tp, x$fp, x$tn, x$fn)
  })
  return(mean(scores, na.rm = TRUE))
}

model_performance_micro_recall <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  summed_matrix        <- confusion_matrixes[[1]]
  for (i in 2:length(confusion_matrixes)) {
    summed_matrix$tp <- summed_matrix$tp + confusion_matrixes[[i]]$tp
    summed_matrix$fp <- summed_matrix$fp + confusion_matrixes[[i]]$fp
    summed_matrix$tn <- summed_matrix$tn + confusion_matrixes[[i]]$tn
    summed_matrix$fn <- summed_matrix$fn + confusion_matrixes[[i]]$fn
  }
  return(model_performance_recall(summed_matrix$tp, summed_matrix$fp, summed_matrix$tn, summed_matrix$fn))
}

model_performance_weighted_recall <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  scores               <- sapply(confusion_matrixes, function(x) {
    model_performance_recall(x$tp, x$fp, x$tn, x$fn)
  })
  return(weighted.mean(scores, prop.table(table(observed))[names(confusion_matrixes)], na.rm = TRUE))
}

# F1.
model_performance_macro_f1 <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  scores               <- sapply(confusion_matrixes, function(x) {
    model_performance_f1(x$tp, x$fp, x$tn, x$fn)
  })
  return(mean(scores, na.rm = TRUE))
}

model_performance_micro_f1 <- function(predicted, observed) {
  return(model_performance_accuracy_multi(predicted, observed))
}

model_performance_weighted_f1 <- function(predicted, observed) {
  confusion_matrixes   <- calculate_confusion_matrixes(predicted, observed)
  f1_scores <- sapply(confusion_matrixes, function(x) {
    model_performance_f1(x$tp, x$fp, x$tn, x$fn)
  })
  return(weighted.mean(f1_scores, prop.table(table(observed))[names(confusion_matrixes)], na.rm = TRUE))
}


# Rest
model_performance_accuracy_multi <- function(predicted, observed) {
  return(mean(predicted == observed, na.rm = TRUE))
}

calculate_confusion_matrixes <- function(predicted, observed) {
  observed <- as.character(observed)
  ret <- lapply(unique(observed), function(x) {
    tp <- sum(predicted[predicted == x] == observed[predicted == x])
    fp <- sum(predicted[predicted == x] != observed[predicted == x])
    tn <- sum(predicted[predicted != x] == observed[predicted != x])
    fn <- sum(predicted[predicted != x] != observed[predicted != x])
    list(tp = ifelse(is.nan(tp) || is.na(tp), 0, tp),
         fp = ifelse(is.nan(fp) || is.na(fp), 0, fp),
         tn = ifelse(is.nan(tn) || is.na(tn), 0, tn),
         fn = ifelse(is.nan(fn) || is.na(fn), 0, fn))
  })
  names(ret) <- unique(observed)
  return(ret)
}
