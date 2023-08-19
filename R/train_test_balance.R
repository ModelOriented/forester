#' Balance and split the dataset
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param balance A logical value, determines if we want to balance the dataset.
#' @param fractions A vector with 3 numeric values that sum to 1 which
#' determine sizes of train, test and validation datasets. DEFAULT: c(0.6, 0.2, 0.2).
#' @param seed An integer random seed. It allows for comparable results. If it
#' is NULL, the split is random.
#'
#' @return A list of train, test and validation datasets.
#' @export
#'
#' @examples
#' data(lisbon)
#' b_lisbon <- train_test_balance(lisbon, 'Price', balance = FALSE,
#'                                fractions = c(train = 0.6, valid = 0.2, test = 0.2))
train_test_balance <-
  function(data, y, balance = TRUE, fractions = c(0.6, 0.2, 0.2), seed = NULL) {
    #Balancing / stratifying classes.
    target <- data[[y]]

    if (sum(fractions) != 1) {
      stop('ERROR: Elements of fraction vector dont sum up to 1!')
    }

    if (balance == TRUE) {
      inds  <- splitTools::partition(target,
                                     p = c(train = fractions[1],
                                           test  = fractions[2],
                                           valid = fractions[3]),
                                     seed = seed)
    } else {
      inds  <- splitTools::partition(target,
                                     p = c(train = fractions[1],
                                           test  = fractions[2],
                                           valid = fractions[3]),
                                     seed = seed,
                                     type = 'basic')
    }

    return(list(
      train      = data[inds$train, ],
      test       = data[inds$test, ],
      valid      = data[inds$valid, ],
      train_inds = inds$train,
      test_inds  = inds$test,
      valid_inds = inds$valid
    ))
  }
