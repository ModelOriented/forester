#' Helper function for the custom preprocessing removing columns and rows.
#'
#' This function includes 6 modules for the removal of unwanted features / observations.
#' We can remove duplicate columns, the ID-like columns, static columns
#' (with specified staticity threshold), sparse columns (with specified sparsity threshold),
#' and highly correlated ones (with specified high correlation threshold).
#' Additionally we can remove the observations that are too sparse (sparsity threshold),
#' and have missing target value. One can turn on and off each module by setting
#' proper `active_modules` logical values.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param active_modules A logical vector describing active removal modules. By default it
#' is set as `c(duplicate_cols = TRUE, id_like_cols = TRUE, static_cols = TRUE,
#' sparse_cols = TRUE, corrupt_rows = TRUE, correlated_cols = TRUE)`, which is
#' equal to c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE). Setting corrupt_rows to FALSE
#' still results in the removal of observations without target value.
#' @param id_names A vector of strings indicating which column names are perceived
#' as ID-like. By default the list is: ['id', 'nr', 'number', 'idx', 'identification', 'index'].
#' @param static_threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of dominating values for column If feature has more dominating
#' values it is going to be removed. By default set to 1, which indicates that
#' all values are equal.
#' @param sparse_columns_threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for columns If column has more missing fields
#' it is going to be removed. By default set to 0.7.
#' @param sparse_rows_threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for observation. If observation has more missing fields
#' it is going to be removed. By default set to 0.7.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param high_correlation_threshold A numeric value from [0,1] range, which indicates when we consider
#' the correlation to be high. If feature surpasses this threshold it is going to
#' be removed. By default set to 0.7.
#'
#' @return A list containing three objects:
#' \itemize{
#' \item \code{`data`} A dataset with deleted observations and columns.
#' \item \code{`rm_col`} The indexes of removed columns.
#' \item \code{`rm_row`} The indexes of removed rows.
#' }
#' @export
preprocessing_removal <- function(data,
                                  y,
                                  active_modules = c(duplicate_cols = TRUE, id_like_cols = TRUE,
                                                     static_cols = TRUE, sparse_cols = TRUE,
                                                     corrupt_rows = TRUE, correlated_cols = TRUE),
                                  id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                                  static_threshold = 0.99,
                                  sparse_columns_threshold = 0.7,
                                  sparse_rows_threshold = 0.7,
                                  na_indicators = c(''),
                                  high_correlation_threshold = 0.7) {

  if (!is.logical(active_modules) || !is.vector(active_modules) || length(active_modules) != 6) {
    verbose_cat(crayon::red('\u2716'), 'Preprocessing removal: The active_modules parameter is not a logical vector of length equal to 6.', '\n', verbose = TRUE)
    stop('Preprocessing removal: The active_modules parameter is not a logical vector of length equal to 6.')
  }

  to_rm_col <- c()
  to_rm_row <- c()

  if (active_modules[[1]]) {
    to_rm_col <- rm_duplicate_columns(data, y)[[2]]
  }
  if (active_modules[[2]]) {
    to_rm_col <- c(to_rm_col, rm_id_like_columns(data, id_names)[[2]])
  }
  if (active_modules[[3]]) {
    to_rm_col <- c(to_rm_col, rm_static_cols(data, y, threshold = static_threshold)[[2]])
  }
  if (active_modules[[4]]) {
    to_rm_col <- c(to_rm_col, rm_sparse_columns(data, y, threshold = sparse_columns_threshold,
                                                na_indicators = na_indicators)[[2]])
  }

  if (!is.null(to_rm_col)) {
    data <- data.frame(data[, -unique(to_rm_col)])
  }

  if (active_modules[[5]]) {
    to_rm_row <- c(to_rm_row, rm_corrupted_observations(data, y, threshold = sparse_rows_threshold,
                                                        na_indicators = na_indicators)[[2]])
    if (!is.null(to_rm_row)) {
      data <- data.frame(data[-to_rm_row, ])
    }
  } else {
    to_rm_row <- c(to_rm_row, rm_corrupted_observations(data, y, threshold = 1,
                                                        na_indicators = na_indicators)[[2]])
    if (!is.null(to_rm_row)) {
      data <- data.frame(data[-to_rm_row, ])
    }
  }
  # We have to impute the missing data in order to calculate the correlations.
  # The imputation with median-other is the least invasive for distributions.
  imputed_data <- preprocessing_imputation(data, na_indicators = c(''),
                                           imputation_method = 'median-other')
  if (active_modules[[6]]) {
    to_rm_col <- unique(to_rm_col)
    to_rm_cor <- rm_correlated_columns(imputed_data, y, threshold = high_correlation_threshold)[[2]]
    if (!is.null(to_rm_cor)) {
      data <- data.frame(data[, -to_rm_cor])
    }
    for (i in 1:length(to_rm_cor)) {
      k            <- sum(to_rm_col <= to_rm_cor[i])
      to_rm_cor[i] <- to_rm_cor[i] + k
    }
  }

  # Prepare the final output of removed columns.
  if (length(to_rm_col) != 0) {
    if (length(to_rm_cor) != 0) {
      to_rm_col <- c(to_rm_col, to_rm_cor)
    } else {
      to_rm_col <- to_rm_col
    }
  } else if (length(to_rm_cor) != 0) {
    to_rm_col <- to_rm_cor
  } else {
    to_rm_col <- c()
  }
  if (!is.null(to_rm_col)) {
    to_rm_col <- unique(to_rm_col)
  }

  return(list(
    data   = data,
    rm_col = to_rm_col,
    rm_row = to_rm_row
  ))
}

#' Remove observations with too much empty values or empty target
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for observation. If observation has more missing fields
#' it is going to be removed. By default set to 0.7.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#'
#' @return A list containing two objects
#' \itemize{
#' \item \code{`data`} A dataset with deleted observations.
#' \item \code{`idx`} The indexes of removed observations.
#' }
#' @export
rm_corrupted_observations <- function(data, y, threshold = 0.7, na_indicators = c('')) {
  if (!threshold >= 0 && !threshold <= 1) {
    verbose_cat(crayon::red('\u2716'), 'Removal of corrupted observations: The value of threshold is not in [0,1] range.', '\n', verbose = TRUE)
    stop('Removal of corrupted observations: The value of threshold is not in [0,1] range.')
  }
  if (!is.vector(na_indicators)) {
    verbose_cat(crayon::red('\u2716'), 'Removal of corrupted observations: Provided na_indicators is not a list.', '\n', verbose = TRUE)
    stop('Removal of corrupted observations: Provided na_indicators is not a list.')
  }
  to_rm <- c()
  X     <- data[names(data) != y]
  Y     <- as.vector(unlist(data[y]))
  for (i in 1:nrow(data)) {
    nas <- sum(as.integer(is.na(X[i,]))) / (ncol(X))
    k   <- 0

    for (j in 1:(ncol(X))) {
      if (X[i, j] %in% na_indicators || is.null(X[i, j])) {
        k <- k + 1
      }
    }
    nas <- nas + k
    # If target is missing.
    if (is.na(Y[i]) || is.null(Y[i]) || Y[i] %in% na_indicators) {
      to_rm <- c(to_rm, i)
    # If observation doesn't have too much missing values.
    } else if (nas >= 1 - threshold) {
      to_rm <- c(to_rm, i)
    }
  }
  if (!is.null(to_rm)) {
    data <- data[-to_rm, ]
  }

  return(list(
    data = data,
    idx  = to_rm
  ))
}


#' Remove ID-like columns.
#'
#' The columns are perceived as ID-like, when they are present in the id_names
#' vector fo strings or the column values are unique integers.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param id_names A vector of strings indicating which column names are perceived
#' as ID-like. By default the list is: ['id', 'nr', 'number', 'idx', 'identification', 'index'].
#'
#' @return A list containing two objects
#' \itemize{
#' \item \code{`data`} A dataset with deleted columns.
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
rm_id_like_columns <- function(data, id_names = c('id', 'nr', 'number', 'idx',
                                                  'identification', 'index')) {
  if (!is.vector(id_names)) {
    verbose_cat(crayon::red('\u2716'), 'Removal of ID-like columns: Provided id_names is not a vector', '\n', verbose = TRUE)
    stop('Removal of ID-like columns: Provided id_names is not a vector')
  }
  names     <- colnames(data)
  to_rm     <- c()
  for (i in 1:ncol(data)) {
    if (tolower(names[i]) %in% id_names) {
      to_rm <- c(to_rm, i)
    }
    if (isTRUE(all.equal(data[, i], as.integer(data[, i]))) &&
        length(unique(data[, i])) == nrow(data)) {
      to_rm <- c(to_rm, i)
    }
  }
  to_rm <- unique(to_rm)
  if (!is.null(to_rm)) {
    data <- data[-to_rm, ]
  }

  return(list(
    data = data,
    idx  = to_rm
  ))
}

#' Remove columns with too much empty values
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for columns If column has more missing fields
#' it is going to be removed. By default set to 0.7.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#'
#' @return A list containing two objects
#' \itemize{
#' \item \code{`data`} A dataset with deleted columns.
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
rm_sparse_columns <- function(data, y, threshold = 0.7, na_indicators = c('')) {
  if (!threshold >= 0 && !threshold <= 1) {
    verbose_cat(crayon::red('\u2716'), 'Removal of sparse columns: The value of threshold is not in [0,1] range.', '\n', verbose = TRUE)
    stop('Removal of sparse columns: The value of threshold is not in [0,1] range.')
  }

  if (!is.vector(na_indicators)) {
    verbose_cat(crayon::red('\u2716'), 'Removal of sparse columns:  Provided na_indicators is not a list.', '\n', verbose = TRUE)
    stop('Removal of sparse columns: Provided na_indicators is not a list.')
  }
  to_rm <- c()
  y_idx <- which(names(data) == y)
  X     <- data[names(data) != y]
  for (i in 1:ncol(X)) {
    nas <- sum(as.integer(is.na(X[, i]))) / nrow(data)
    k   <- 0
    for (j in 1:nrow(X)) {
      if (X[j, i] %in% na_indicators || is.null(X[j, i])) {
        k <- k + 1
      }
    }
    nas <- nas + k
    if (nas >= 1 - threshold) {
      to_rm <- c(to_rm, i)
    }
  }
  if (!is.null(to_rm)) {
    to_rm <- ifelse (to_rm >= y_idx, to_rm + 1, to_rm)
    data <- data[-to_rm, ]
  }
  return(list(
    data = data,
    idx  = to_rm
  ))
}


#' Remove duplicate columns iteratively from data set.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#'
#' @return A list containing two objects
#' \itemize{
#' \item \code{`data`} A dataset with deleted columns.
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
rm_duplicate_columns <- function(data, y) {
  X          <- data[names(data) != y]
  X_2        <- X
  y_idx      <- which(names(data) == y)
  to_rm      <- c()
  duplicates <- TRUE
  while(duplicates) {
    mat <- find_duplicate_columns(X_2)
    if (sum(mat) == 0) {
      duplicates <- FALSE
    } else {
      add   <- sum(to_rm <= which.max(colSums(mat)))
      to_rm <- c(to_rm, which.max(colSums(mat)) + add)
      X_2   <- X[, -to_rm]
    }
  }
  if (!is.null(to_rm)) {
    # If the index of our target is lower-equal than the value of index we have to
    # increase the index by one.
    ifelse (to_rm >= y_idx, to_rm + 1, to_rm)
    data <- data[-to_rm, ]
  }

  return(list(
    data = data,
    idx  = to_rm
  ))
}

#' Find duplicate columns
#'
#' It is a helper function for the rm_duplicate_columns(). It creates a 2 dimensional
#' matrix which has 0s when the columns are not duplicates and 1s otherwise.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#'
#' @return A numeric 0-1 matrix.
#' @export
find_duplicate_columns <- function(data) {
  duplicate_mt <- matrix(0, ncol(data), ncol(data))
  for (i in 1:ncol(data)) {
    for (j in i:ncol(data)) {
      if (i != j && identical(data[, i], data[, j])) {
        duplicate_mt[i, j] <- 1
      }
    }
  }
  return(duplicate_mt)
}

#' Remove strongly correlated columns (Spearman's rank for numerical, Crammer's V for
#' categorical)
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param threshold A numeric value from [0,1] range, which indicates when we consider
#' the correlation to be high. If feature surpasses this threshold it is going to
#' be removed. By default set to 0.7.
#'
#' @return A list containing four objects
#' \itemize{
#' \item \code{`data`} A dataset with deleted columns.
#' \item \code{`idx`} The indexes of removed columns.
#' \item \code{`rm_num`} The indexes of removed numerical columns.
#' \item \code{`rm_cat`} The indexes of removed categorical columns.
#' }
#'
#' @export
rm_correlated_columns <- function(data, y, threshold = 0.7) {
  if (!threshold >= 0 && !threshold <= 1) {
    verbose_cat(crayon::red('\u2716'), 'Removal of highly correlated columns: The value of threshold is not in [0,1] range.', '\n', verbose = TRUE)
    stop('Removal of highly correlated columns: The value of threshold is not in [0,1] range.')
  }
  y_idx   <- which(names(data) == y)
  data    <- data[, !names(data) %in% y]
  data    <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  num_idx <- c()
  fct_idx <- c()
  # Dividing the categorical and numeric features.
  for (i in 1:ncol(data)) {
    if (is.factor(data[, i])) {
      if (length(levels(data[, i])) > 1) {
        fct_idx <- c(fct_idx, i)
      }
    } else {
      num_idx <- c(num_idx, i)
    }
  }
  cor_num       <- NULL
  to_rm_numeric <- c()
  if (length(num_idx) != 0) {
    # Calculate Spearman's correlation.
    cor_num             <- round(cor(data[num_idx]), 4)
    strong_Spearman_mat <- matrix(0, length(num_idx), length(num_idx))
    to_rm               <- c()
    strong_Spearman_cor <- TRUE
    # A copy needed for removal of features.
    for (i in 1:ncol(cor_num)) {
      for (j in i:ncol(cor_num)) {
        if (i != j && cor_num[i, j] >= threshold) {
          strong_Spearman_mat[i, j] <- 1
        }
      }
    }
    strong_Spearman_cp  <- strong_Spearman_mat
    while(strong_Spearman_cor) {
      if (sum(strong_Spearman_cp) == 0) {
        strong_Spearman_cor <- FALSE
      } else {
        add                <- sum(to_rm <= which.max(colSums(strong_Spearman_cp)))
        to_rm              <- c(to_rm, which.max(colSums(strong_Spearman_cp)) + add)
        strong_Spearman_cp <- strong_Spearman_mat[, -to_rm]
      }
    }
    to_rm_numeric <- num_idx[to_rm]
  }
  cor_fct           <- NULL
  to_rm_categorical <- c()

  if (length(fct_idx) != 0) {
    fct_tbl <- data[fct_idx]
    cor_fct <- matrix(0, nrow = length(fct_idx), ncol = length(fct_idx))
    colnames(cor_fct)  <- names(fct_tbl)
    row.names(cor_fct) <- names(fct_tbl)

    # Calcualte CrammerV correlation
    for (i in 1:length(fct_idx)) {
      for (j in 1:length(fct_idx)) {

        a = as.numeric(length(unique(fct_tbl[, i])))
        b = as.numeric(length(unique(fct_tbl[, j])))
        # With 2^31 we have to big vector that can't be allocated (over 7.4 Gb).
        if (as.numeric(a * b) <= 2^29 - 1){
          cor_fct[i, j] <- round(rcompanion::cramerV(fct_tbl[, i], fct_tbl[, j]), 2)
        } else {
          cor_fct[i, j] <- NA
        }

      }
    }
    strong_CrammerV_mat <- matrix(0, length(fct_idx), length(fct_idx))
    to_rm               <- c()
    strong_CrammerV_cor <- TRUE
    # A copy needed for removal of features.

    for (i in 1:ncol(cor_fct)) {
      for (j in i:ncol(cor_fct)) {
        if (i != j && cor_fct[i, j] >= threshold) {
          strong_CrammerV_mat[i, j] <- 1
        }
      }
    }
    strong_CrammerV_cp  <- strong_CrammerV_mat

    while(strong_CrammerV_cor) {
      if (sum(strong_CrammerV_cp) == 0) {
        strong_CrammerV_cor <- FALSE
      } else {
        add                 <- sum(to_rm <= which.max(colSums(strong_CrammerV_cp)))
        to_rm               <- c(to_rm, which.max(colSums(strong_CrammerV_cp)) + add)
        strong_CrammerV_cp  <- strong_CrammerV_mat[, -to_rm]
      }
    }
    to_rm_categorical <- fct_idx[to_rm]
  }

  if (length(to_rm_categorical) != 0) {
    if (length(to_rm_numeric) != 0) {
      to_rm <- c(to_rm_numeric, to_rm_categorical)
      to_rm <- ifelse (to_rm >= y_idx, to_rm + 1, to_rm)
      data  <- data[, -to_rm]
    } else {
      to_rm <- to_rm_categorical
      to_rm <- ifelse (to_rm >= y_idx, to_rm + 1, to_rm)
      data  <- data[, -to_rm]
    }
  } else if (length(to_rm_numeric) != 0) {
    to_rm <- to_rm_numeric
    to_rm <- ifelse (to_rm >= y_idx, to_rm + 1, to_rm)
    data  <- data[, -to_rm]
  } else {
    to_rm <- c()
    data  <- data
  }

  return(list(
    data   = data,
    idx    = to_rm,
    rm_num = to_rm_numeric,
    rm_cat = to_rm_categorical
  ))
}

#' Remove columns which are dominated by one value
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param threshold A numeric value from [0,1] range, which indicates the maximum
#' threshold of dominating values for column If feature has more dominating
#' values it is going to be removed. By default set to 1, which indicates that
#' all values are equal.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with deleted columns
#' \item \code{`idx`} The indexes of removed columns
#' }
#' @export
rm_static_cols <- function(data, y, threshold = 1) {
  if (!threshold >= 0 && !threshold <= 1) {
    verbose_cat(crayon::red('\u2716'), 'Removal of static columns: The value of threshold is not in [0,1] range.', '\n', verbose = TRUE)
    stop('Removal of static columns: The value of threshold is not in [0,1] range.')
  }
  to_rm <- c()
  for (i in 1:ncol(data)) {
    if (sort(table(data[[i]]), decreasing = TRUE)[[1]] / nrow(data) >= threshold) {
      to_rm <- c(to_rm, i)
    }
  }
  if (!is.null(to_rm)) {
    data <- data[, -to_rm]
  }
  return(list(
    data = data,
    idx  = to_rm
  ))
}
