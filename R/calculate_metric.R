#' Metrices for evaluating model's performance.
#'
#' Function \code{calculate_metric} returns the value model's performance for classification and regression type measured by a specified metric.
#' For classification models following metrices can be calculated: F1, accuracy, recall, precision and AUC.
#' For regression models following metrices can be calculated: mean squared error, R squared, median absolute deviation.
#'
#' @param metric character, name of metric. For regression, options are: "mse", "rmse", "mad" and "r2".
#' For classification, options are: "auc", "recall", "precision", "f1" and "accuracy".
#' @param predicted numeric vector containing the prediction target values from model.
#' @param observed numeric vector containing actual values of target.
#' @param cutoff a cutoff for classification models, needed for measures like recall, precision, ACC, F1. By default 0.5.
#'
#' @return The real value of measured metric between predicted and observed vectors.
#'
#'
#'
#' @references \code{model_performance} from DALEX library. \url{https://github.com/ModelOriented/DALEX/blob/master/R/model_performance.R}
#' @importFrom stats median weighted.mean
#' @export
#'


calculate_metric <- function(metric, predicted, observed, cutoff = 0.5){
  
  print(metric)
  # Regression:
  if (metric == "mse"){
    mse <- model_performance_mse(predicted, observed)
    return(mse)
  }
  
  if (metric == "rmse"){
    rmse <- model_performance_rmse(predicted, observed)
    return(rmse)
  }
  
  if (metric == "r2"){
    r2 <- model_performance_r2(predicted, observed)
    return(r2)
  }
  
  if (metric == "mad"){
    mad <- model_performance_mad(predicted, observed)
    return(mad)
  }
  
  
  # Classification:
  # From this point, we will calculate the confusion matrix for classification problem:
  tp = sum((observed == 1) * (predicted >= cutoff))
  fp = sum((observed == 0) * (predicted >= cutoff))
  tn = sum((observed == 0) * (predicted < cutoff))
  fn = sum((observed == 1) * (predicted < cutoff))
  
  if (metric == "recall"){
    recall  <- model_performance_recall(tp, fp, tn, fn)
    return(recall)
  }
  
  if (metric == "precision"){
    precision <- model_performance_precision(tp, fp, tn, fn)
    return(precision)
  }
  
  if (metric == "f1"){
    f1 <- model_performance_f1(tp, fp, tn, fn)
    return(f1)
  }
  
  if (metric == "accuracy"){
    accuracy <- model_performance_accuracy(tp, fp, tn, fn)
    return(accuracy)
  }
  
  if (metric == "auc"){
    auc <- model_performance_auc(predicted, observed)
    return(auc)
  }
}


model_performance_mse <- function(predicted, observed) {
  mean((predicted - observed)^2, na.rm = TRUE)
}


model_performance_rmse <- function(predicted, observed) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}


model_performance_r2 <- function(predicted, observed) {
  1 - model_performance_mse(predicted, observed)/model_performance_mse(mean(observed), observed)
}


model_performance_mad <- function(predicted, observed) {
  median(abs(predicted - observed))
}


model_performance_auc <- function(predicted, observed) {
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR <- c(0,cumsum(rev(tpr_tmp)))/sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR <- c(0,cumsum(rev(fpr_tmp)))/sum(1 - observed)
  auc <- sum(diff(FPR)*(TPR[-1] + TPR[-length(TPR)])/2)
  auc
}


model_performance_recall <- function(tp, fp, tn, fn) {
  tp/(tp + fn)
}


model_performance_precision <- function(tp, fp, tn, fn) {
  tp/(tp + fp)
}


model_performance_f1 <- function(tp, fp, tn, fn) {
  recall = tp/(tp + fn)
  precision = tp/(tp + fp)
  2 * (precision * recall)/(precision + recall)
}


model_performance_accuracy <- function(tp, fp, tn, fn) {
  (tp + tn)/(tp + fp + tn + fn)
}


