#' Create final ranked_list
#'
#' @param basic A data.frame with ranked list of basic models.
#' @param random A data.frame with ranked list of random search models.
#' @param Bayes A data.frame with ranked list of Bayes Optimization models.
#' @param type A string which determines if Machine Learning task is the
#' `classification` or `regression`.
#'
#' @return A data.frame ranked list sorted by default metric.
#' @export
create_ranked_list <- function(basic, random, Bayes, type) {
  ranked_list            <- rbind(basic, random$score, Bayes)[, -1]

  metrics_decreasing     <- c('mse' = FALSE, 'rmse' = FALSE, 'r2' = TRUE,
                          'mad' = FALSE, 'recall' = TRUE, 'precision' = TRUE,
                          'accuracy' = TRUE, 'auc' = TRUE, 'f1' = TRUE)

  if (type == 'regression') {
    sort_by              <- 'mse'
    ranked_list[, 'mse'] <- as.numeric(ranked_list[, 'mse'])
    ranked_list[, 'r2']  <- as.numeric(ranked_list[, 'r2'])
    ranked_list[, 'mad'] <- as.numeric(ranked_list[, 'mad'])
  } else if (type == 'binary_clf') {
    sort_by              <- 'f1'
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

  return(ranked_list)
}
