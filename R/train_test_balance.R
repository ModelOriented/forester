#' Balancess and splits dataset
#'
#' @param data A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#' @param type A string which determines if machine learning task is the
#' `classification` or `regression`.
#' @param balance A logical value, determines if we want to balance the dataset.
#' @param fractions A vector with 3 numeric values that sums to 1 which
#' determine sizes of train, test, validation datasets. DEFAULT: c(0.6, 0.2, 0.2).
#'
#' @return A list of train, test and validation datasets.
#' @export
#'
#' @examples
#' data(lisbon)
#' b_lisbon <- train_test_balance(lisbon,'Price',balance = FALSE,'regression',
#' fractions = c(train = 0.6, valid = 0.2, test = 0.2))
train_test_balance <-
  function(data,
           y,
           type,
           balance = TRUE,
           fractions = c(0.6, 0.2, 0.2)) {
    #Balancing / stratifying classes
    target <- data[[y]]
    if (balance == TRUE) {
      inds  <- splitTools::partition(target, p = c(train = fractions[1],
                                                   test  = fractions[2],
                                                   valid = fractions[3]))
    }else{
      inds  <- splitTools::partition(target, p = c(train = fractions[1],
                                                   test  = fractions[2],
                                                   valid = fractions[3]),
                                                   type = 'basic')
    }

    return(list(
      train = data[inds$train, ],
      test  = data[inds$valid, ],
      valid = data[inds$test, ]
    ))
  }
