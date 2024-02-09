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
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list of train, test and validation datasets.
#' @export
train_test_balance <- function(data, y, balance = TRUE, fractions = c(0.6, 0.2, 0.2), seed = NULL, verbose = FALSE) {
    # Balancing / stratifying classes.
    target <- data[[y]]
    if (!(is.null(seed) || (is.numeric(seed) & as.integer(seed) == seed))) {
      verbose_cat(crayon::red('\u2716'), 'The split seed has to be an integer.', verbose = verbose)
      stop('The split seed has to be an integer.')
    }
    if (!is.vector(fractions)) {
      verbose_cat(crayon::red('\u2716'), 'The split needs to be a vector.', verbose = verbose)
      stop('The split needs to be a vector.')
    }
    if (!all(is.numeric(fractions))) {
      verbose_cat(crayon::red('\u2716'), 'The split needs to be a numeric vector.', verbose = verbose)
      stop('The split needs to be a numeric vector.')
    }
    if (length(fractions) != 3) {
      verbose_cat(crayon::red('\u2716'), 'The split needs to be a numeric vector of length equal to 3.', verbose = verbose)
      stop('The split needs to be a numeric vector of length equal to 3.')
    }
    if (sum(fractions) != 1) {
      verbose_cat(crayon::red('\u2716'), 'The split needs to be a numeric vector of length equal to 3 which sums up to 1.', verbose = verbose)
      stop('The split needs to be a numeric vector of length equal to 3 which sums up to 1.')
    }
    if (any(fractions <= 0)) {
      verbose_cat(crayon::red('\u2716'), 'The split needs to be a numeric vector of length equal to 3 which sums up to 1, where all values have to be positive.', verbose = verbose)
      stop('The split needs to be a numeric vector of length equal to 3 which sums up to 1, where all values have to be positive.')
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
