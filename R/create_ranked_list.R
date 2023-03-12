#' Create final ranked_list
#'
#' @param basic A data.frame with ranked list of basic models.
#' @param random A data.frame with ranked list of random search models.
#' @param Bayes A data.frame with ranked list of Bayes Optimization models.
#' @param type A string which determines if Machine Learning task is the
#' `binary_clf` or `regression`.
#' @param score A data frame with metrics values for given models, by `score_models`.
#' @param sort_by String with name of metric to sort by.
#' For `auto` models going to be sorted by `mse` for regression and `f1` for classification.
#' @param metric_function The self-created function.
#' It should look like name(predictions, observed) and return the numeric value.
#' In case of using `metrics` param with value other than `auto` or `all`, is needed to use value `metric_function`
#' in order to see given metric in report. If `sort_by` is equal to `auto` models are sorted by `metric_function`.
#' @param metric_function_name The name of the column with values of param `metric_function`.
#' By default `metric_function_name` is `metric_function`.
#' @param metric_function_decreasing A logical value indicating how metric_function should be sorted. `TRUE` by default.

#'
#' @return A data.frame ranked list sorted by default metric.
#' @export
create_ranked_list <- function(basic,
                               random,
                               Bayes,
                               type,
                               score,
                               sort_by = 'auto',
                               metric_function = NULL,
                               metric_function_name = 'metric_function',
                               metric_function_decreasing = TRUE) {
  ranked_list            <- rbind(basic, random$score, Bayes)[, -1]

  metrics_decreasing     <- c('mse' = FALSE, 'rmse' = FALSE, 'r2' = TRUE,
                          'mad' = FALSE, 'recall' = TRUE, 'precision' = TRUE,
                          'accuracy' = TRUE, 'auc' = TRUE, 'f1' = TRUE, 'metric_function' = metric_function_decreasing)
  if (!is.null(metric_function) && sort_by == 'auto') {
    sort_by <- 'metric_function'
  }
  if (type == 'regression') {
    if (sort_by == 'auto' && is.null(metric_function)) {
      sort_by <- 'mse'
    }
  } else if (type == 'binary_clf') {
    if (sort_by == 'auto' && is.null(metric_function)) {
      sort_by <- 'f1'
    }
  }
  ranked_list[, sort_by] <- as.numeric(ranked_list[, sort_by])
  ranked_list            <- ranked_list[order(ranked_list[, sort_by],
                                        decreasing = metrics_decreasing[sort_by]), ]

  row.names(ranked_list) <- NULL

  idx <- c()
  for (i in 1:nrow(ranked_list)) {
    if (!(is.nan(ranked_list[i, 2]) &&
          is.nan(ranked_list[i, 3]) &&
          is.na(ranked_list[i, 4]))) {
      idx <- c(idx, i)
    }
  }
  ranked_list <- ranked_list[idx, ]

  if (is.null(metric_function_name)) {
    metric_function_name <- 'metric_function'
  }
  if (!is.null(metric_function)) {
    colnames(ranked_list)[colnames(ranked_list) == 'metric_function'] <- metric_function_name
  }

  return(ranked_list)
}
