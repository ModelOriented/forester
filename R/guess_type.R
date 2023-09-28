#' Guess task type by the target value from the dataset
#'
#' For now, supported types are binary classification and regression.
#' Multilabel classification is planned to be added later on.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param max_unique_numeric An integer describing the maximal number of unique
#' values in `y` if `y` is numeric.
#' @param max_unique_not_numeric An integer describing the maximal number of unique
#' values in `y` if `y` is NOT numeric.
#'
#' @return A string describing the type of ml task: `binary_clf`, `multi_clf`,
#' `regression`, or `survival`.
#' @export
guess_type <- function(data, y, max_unique_numeric = 5, max_unique_not_numeric = 15) {

  if (is.null(y)) {
    type <- 'survival'
  } else {
    target <- data[[y]]
    if (is.numeric(target)) {
      if (length(unique(target)) == 2) {
        type <- 'binary_clf'
      } else if (length(unique(target)) <= max_unique_numeric) {
        type <- 'multi_clf'
      } else {
        type <- 'regression'
      }
    } else if (length(unique(target)) == 2) {
      type <- 'binary_clf'
    } else if (length(unique(target)) <= max_unique_not_numeric) {
      type <- 'multi_clf'
    } else {
      type <- 'regression'
    }
  }
  return(type)
}
