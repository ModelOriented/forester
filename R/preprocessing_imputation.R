#' Imputes missing values according to one of four prepared methods
#'
#' \itemize{
#' \item \code{`median-other`} The numeric features are imputed with median value,
#' whereas the categorical ones with the 'other' string. It is a fast method,
#' \item \code{`median-frequency`} The numeric features are imputed with median value,
#' whereas the categorical ones with the most frequent value. It is a fast method,
#' \item \code{`knn`} All features are imputed with KNN algorithm. It is a moderately fast method,
#' \item \code{`mice`} All features are imputed with MICE algorithm. It is a slow method.
#' }
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param imputation_method A string value indication the imputation method. The
#' imputation method must be one of 'median-other', 'median-frequency', 'knn', or 'mice'.
#' @param k An integer describing the number of nearest neighbours to use. By default
#' set to 10. The parameter applicable only if selected `imputation_method` is 'knn'.
#' @param m An integer describing the number of multiple imputations to use.
#' By default set to 5. The parameter applicable only if selected
#' `imputation_method` is 'mice'.
#'
#' @return Imputed dataset.
#' @export
preprocessing_imputation <- function(data,
                                     na_indicators = c(''),
                                     imputation_method = 'median-other',
                                     k = 10,
                                     m = 5) {

  if (!imputation_method %in% c('median-other', 'median-frequency', 'knn', 'mice')) {
    verbose_cat(crayon::red('\u2716'), 'Preprocessing imputation: The imputation method must be one of median-other, median-frequency, knn, or mice.', '\n', verbose = TRUE)
    stop('Preprocessing imputation: The imputation method must be one of median-other, median-frequency, knn, or mice.')
  }

  if (imputation_method == 'median-other') {
    data <- impute_basic(data = data, na_indicators = na_indicators,
                         categorical_imputation = 'other')
  } else if (imputation_method == 'median-frequency') {
    data <- impute_basic(data = data, na_indicators = na_indicators,
                         categorical_imputation = 'frequency')
  } else if (imputation_method == 'knn') {
    data <- impute_knn(data = data, na_indicators = na_indicators, k = k)
  } else if (imputation_method == 'mice') {
    data <- impute_mice(data = data, na_indicators = na_indicators, m = m)
  }
  return(data)
}

#' Imputes the dataset with median of column for numeric data and the class
#' 'other' or most frequent observation for categorical features
#'
#' Fast and simple imputation method treating categorical and numerical features
#' differently. Please, note that imputation is performed on all columns including
#' the target `y`, as we assume that it has no missing values or these were
#' handled by removal functions or the user.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param categorical_imputation A string value describing the imputation method
#' for categorical features. The user can choose from setting missing values as
#' 'other' or the most frequent value from feature. The respective options are:
#' 'other' or 'frequency'. By default set to 'other'.
#'
#' @return Imputed dataset.
#' @export
impute_basic <- function(data, na_indicators = c(''), categorical_imputation = 'other') {
  if (!is.vector(na_indicators)) {
    verbose_cat(crayon::red('\u2716'), 'Basic imputation: Provided na_indicators is not a list.', '\n', verbose = TRUE)
    stop('Basic imputation: Provided na_indicators is not a list.')
  }
  data    <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  num_idx <- c()
  fct_idx <- c()
  # Dividing the categorical and numeric features.
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      num_idx <- c(num_idx, i)
    } else {
      fct_idx <- c(fct_idx, i)
    }
  }
  num_data <- data[num_idx]
  fct_data <- data[fct_idx]
  # Median imputation for numeric features.
  if (ncol(num_data) >= 1) {
    for (i in 1:ncol(num_data)) {
      column <- num_data[, i]
      # Change all values considered as missing values to NA.
      for (j in 1:length(column)) {
        if (is.na(column[j]) || column[j] %in% na_indicators || is.null(column[j])) {
          column[j] <- NA
        }
      }
      med <- median(column, na.rm = TRUE)
      for (j in 1:length(column)) {
        if (is.na(column[j]) || column[j] %in% na_indicators || is.null(column[j])) {
          column[j] <- med
        }
      }
      num_data[, i] <- column
    }
  }

  # Other imputation for categorical features.
  if (ncol(fct_data) >= 1) {
    for (i in 1:ncol(fct_data)) {
      column <- fct_data[, i]
      if (categorical_imputation == 'other') {
        # Change all values considered as missing values to 'other'.
        for (j in 1:length(column)) {
          if (is.na(column[j]) || column[j] %in% na_indicators || is.null(column[j])) {
            # Add the factor 'other' if it is not already present.
            if ('other' %in% levels(column)) {
              column[j] <- 'other'
            } else {
              levels(column) <- c(levels(column), 'other')
              column[j] <- 'other'
            }
          }
        }
      } else if (categorical_imputation == 'frequency') {
        # Change all values considered as missing values to NA.
        for (j in 1:length(column)) {
          if (is.na(column[j]) || column[j] %in% na_indicators || is.null(column[j])) {
            column[j] <- NA
          }
        }
        most_frequent <- names(sort(table(column), decreasing = TRUE))[[1]]
        for (j in 1:length(column)) {
          if (is.na(column[j]) || column[j] %in% na_indicators || is.null(column[j])) {
            column[j] <- most_frequent
          }
        }
      }
      fct_data[, i] <- column
    }
  }
  if (length(num_idx) >= 1) {
    for (i in 1:length(num_idx)) {
      data[, num_idx[i]] <- num_data[, i]
    }
  }
  if (length(fct_idx) >= 1) {
    for (i in 1:length(fct_idx)) {
      data[, fct_idx[i]] <- fct_data[, i]
    }
  }
  return(data)
}

#' Imputes all missing values with the KNN algorithm
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param k An integer describing the number of nearest neighbours to use. By default
#' set to 10.
#'
#' @return Imputed dataset.
#' @export
impute_knn <- function(data, na_indicators = c(''), k = 10) {
  if (!is.vector(na_indicators)) {
    verbose_cat(crayon::red('\u2716'), 'KNN imputation: Provided na_indicators is not a list.', '\n', verbose = TRUE)
    stop('KNN imputation: Provided na_indicators is not a list.')
  }
  if (as.integer(k) != k || k < 1) {
    verbose_cat(crayon::red('\u2716'), 'KNN imputation: Number of neighbours must be a positive integer.', '\n', verbose = TRUE)
    stop('KNN imputation: Number of neighbours must be a positive integer.')
  }
  for (i in 1:ncol(data)) {
    for (j in 1:length(data[, i])) {
      if (is.na(data[j, i]) || data[j, i] %in% na_indicators || is.null(data[j, i])) {
        data[j, i] <- NA
      }
    }
  }
  suppressWarnings(data <- VIM::kNN(data, k = k, imp_var = FALSE))
  return(data)
}

#' Imputes all missing values with the MICE (Multivariate Imputation by Chained Equations) algorithm
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param m An integer describing the number of multiple imputations to use.
#' By default set to 5.
#'
#' @return Imputed dataset.
#' @export
impute_mice <- function(data, na_indicators = c(''), m = 5) {
  if (!is.vector(na_indicators)) {
    verbose_cat(crayon::red('\u2716'), 'MICE imputation: Provided na_indicators is not a list.', '\n', verbose = TRUE)
    stop('MICE imputation: Provided na_indicators is not a list.')
  }
  if (as.integer(m) != m || m < 1) {
    verbose_cat(crayon::red('\u2716'), 'MICE imputation: Number of multiple imputations must be a positive integer.', '\n', verbose = TRUE)
    stop('MICE imputation: Number of multiple imputations must be a positive integer.')
  }
  for (i in 1:ncol(data)) {
    for (j in 1:length(data[, i])) {
      if (is.na(data[j, i]) || data[j, i] %in% na_indicators || is.null(data[j, i])) {
        data[j, i] <- NA
      }
    }
  }
  # Setting non-numeric values as factor, otherwise MICE doesn't work properly.
  for (i in 1:ncol(data)) {
    if (!is.numeric(data[, i])) {
      data[, i] <- as.factor(data[, i])
    }
  }
  data <- mice::mice(data, m = m, seed = 123, print = FALSE, remove_collinear = FALSE)
  data <- mice::complete(data)
  return(data)
}
