#' Checking conditions for metrics
#'
#' \code{check_metric} verifies whether the given metric is chosen properly.
#'
#'
#' @param metric character, defining metric which should be used in further tasks.
#' @param type character, defining the task. Option is "regression" or "classification", namely binary classification.
#'
#' @return A verified metric
#'
#'
#' @references forester library \url{https://modeloriented.github.io/forester/}

check_metric <- function(metric, type){
  if (is.null(metric)){
    metric <- ifelse(type == "classification", "auc", "rmse")
  } else {
    if (type == "classification"){
      if (! metric %in% c("auc", "recall", "precision", "f1", "accuracy")){
        stop("Wrong metric selected. For classification, consider one of those metrics: 
           auc, recall, precision, f1, accuracy")
      }
    } else {
      if (! metric %in% c("mse", "rmse", "mad", "r2")){
        stop("Wrong metric selected. For regression, consider one of those metrics: 
           rmse, mse, mad, r2")
      }
    }
  }
  return(metric)
}
