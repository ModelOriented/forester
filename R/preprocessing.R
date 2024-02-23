#' Conduct preprocessing processes
#'
#' @param data A data source, that is one the of major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param type A string that determines if Machine Learning task is the
#' `binary_clf`, `regression`, `survival`, or `multiclass` task.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list containing three objects:
#' \itemize{
#' \item \code{`data`} A preprocessed dataset,
#' \item \code{`rm_colnames`} The names of removed columns,
#' \item \code{`bin_labels`} The text labels before target binarization.
#' }
#' @export
preprocessing <- function(data, y = NULL, time = NULL, status = NULL, type, verbose = FALSE) {
  pre_data   <- pre_rm_static_cols(data, y)
  pre_data   <- manage_missing(pre_data, y)
  binary     <- binarize_target(pre_data, type, y, status)
  pre_data   <- binary$bin_data
  bin_labels <- binary$labels
  # Legacy version of advanced preprocessing
  # if (advanced) {
  #   del_cor  <- delete_correlated_values(pre_data, y, verbose = verbose)
  #   pre_data <- del_cor$data
  #   pre_data <- delete_id_columns(pre_data)
  #   pre_data <- boruta_selection(pre_data, y)
  # }
  del_cols   <- save_deleted_columns(data, pre_data)

  if (type %in% c('binary_clf', 'multiclass')) {
    pre_data[, y] <- as.factor(pre_data[, y])
  } else if (type == 'survival') {
    pre_data[, status] <- as.factor(pre_data[, status])
  }

  return(
    list(
      data        = pre_data,
      rm_colnames = del_cols,
      bin_labels  = bin_labels
    )
  )
}

#' Remove columns with one value for all rows
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#'
#' @return A dataset with deleted unwanted columns.
#' @export
pre_rm_static_cols <- function(data, y) {
  del <- c()
  for (i in 1:ncol(data)) {
    if (length(unique(data[, i])) == 1) {
      del <- c(del, i)
    }
  }
  if (!is.null(del)) {
    data <- data[, -del]
  } else {
    data <- data
  }
  return(data)
}

#' Binarize the target column
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param type A character, one of `binary_clf`/`regression`/`survival`/`auto`/`multiclass` that
#' sets the type of the task. If `auto` (the default option) then
#' the function will figure out `type` based on the number of unique values
#' in the `y` variable, or the presence of time/status columns.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or status can be used. By default NULL.
#'
#' @return A dataset with binarized target column.
#' @export
binarize_target <- function(data, type, y = NULL, status = NULL) {
  if (type %in% c('binary_clf', 'survival', 'multiclass')) {
    if (is.null(y)) {
      y <- status
    }
    bin_data      <- data
    data[[y]]     <- as.factor(data[[y]])
    bin_data[[y]] <- as.integer(data[[y]]) # From 1 to n.
    labels        <- levels(data[[y]])
  } else {
    bin_data <- data
    labels   <- NULL
  }
  return(
    list(
      bin_data = bin_data,
      labels   = labels
    )
  )
}


#' Manage missing values
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name.
#'
#' @return A dataframe with removed and imputed missing values.
#' @export
manage_missing <- function(df, y) {
  # Remove mostly missing columns.
  col_to_rm <- c()
  for (i in 1:ncol(df)) {
    if (length(df[, i][df[, i] == '']) >= length(df[, i]) / 2) {
      col_to_rm <- c(col_to_rm, i)
    }
    if (is.numeric(df[, i]) == FALSE) {
      df[, i] <- as.factor(df[, i])
    }
  }
  if (length(col_to_rm) > 0) {
    df <- df[, -col_to_rm]
  }
  # Input missing values via mice algorithm.
  df <- mice::mice(df, seed = 123, print = FALSE, remove_collinear = FALSE)
  df <- mice::complete(df)
  return(df)
}


#' Save column names deleted during preprocessing process
#'
#' @param df A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#' @param pre_df A data source after preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#'
#' @return A vector of strings with column names.
#' @export
save_deleted_columns <- function(df, pre_df) {
  names_df        <- colnames(df)
  names_pre_df    <- colnames(pre_df)
  deleted_columns <- c()

  for (i in 1:length(names_df)) {
    if (!(names_df[i] %in% names_pre_df)) {
      deleted_columns <- c(deleted_columns, names_df[i])
    }
  }
  return(deleted_columns)
}

#' Delete correlated values
#'
#' The deleting process starts with the extraction of correlated pairs of columns.
#' Later, they are sorted decreasingly by correlation value and we count the number
#' of occurrences to sort column names later by the amount of them. In the end we
#' remove columns iteratively from the most occurring one, so we will remove the
#' bare minimum of columns in the end.
#'
#' @param data A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#' @param y A string that indicates a target column name.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with 2 items: data set with removed correlated columns and
#' names of removed columns.
#' @export
delete_correlated_values <- function(data, y, verbose = TRUE) {
  # Create dataframe with correlation info.
  cor                  <- check_cor(data, y, verbose = verbose)
  columns              <- c('col1', 'col2', 'cor')
  correlated           <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(correlated) <- columns
  k                    <- 0

  if (!is.null(cor$cor_num)) {
    for (i in 1:ncol(cor$cor_num)) {
      for (j in i:ncol(cor$cor_num)) {
        if (i != j && cor$cor_num[i, j] >= 0.7) {
          k = k + 1
          correlated[k, ] <- c(cor$num_names[i], cor$num_names[j], cor$cor_num[i, j])
        }
      }
    }
  }
  if (!is.null(cor$cor_fct)) {
    for (i in 1:ncol(cor$cor_fct)) {
      for (j in i:ncol(cor$cor_fct)) {
        if (i != j && cor$cor_fct[i, j] >= 0.7) {
          k = k + 1
          correlated[k, ] <- c(cor$fct_names[i], cor$fct_names[j], cor$cor_fct[i, j])
        }
      }
    }
  }

  # Count number of occurrences for every correlated column (also order by
  # correlation value).
  correlated <- data.table::setorder(correlated, -cor)
  if (nrow(correlated) > 0) {
    col_names   <- c()
    occurrences <- c()
    for (i in 1:nrow(correlated)) {
      if (!(correlated[i, 1] %in% col_names)) {
        col_names   <- c(col_names, correlated[i, 1])
        occurrences <- c(occurrences, 1)
      } else {
        for (j in 1:length(col_names)) {
          if (correlated[i, 1] == col_names[j]) {
            occurrences[j] <- occurrences[j] + 1
          }
        }
      }

      if (!(correlated[i, 2] %in% col_names)) {
        col_names   <- c(col_names, correlated[i, 2])
        occurrences <- c(occurrences, 1)
      } else {
        for (j in 1:length(col_names)) {
          if (correlated[i, 2] == col_names[j]) {
            occurrences[j] <- occurrences[j] + 1
          }
        }
      }
    }
    # Remove columns iteratively from the most frequently occurring ones.
    corrr    <- correlated
    cols_occ <- data.frame(col_names, occurrences)
    cols_occ <- data.table::setorder(cols_occ, -occurrences)
    deleted  <- c()
    k        <- 0

    while (nrow(corrr) > 0) {
      k          <- k + 1
      del        <- cols_occ[k, 1]
      col_to_del <- c()

      for (i in 1:nrow(corrr)) {
        if (corrr[i, 1] == del && !is.null(corrr[i, 2])) {
          col_to_del <- c(col_to_del, i)
          deleted    <- c(deleted, del)
        }
        if (corrr[i, 2] == del && !is.null(corrr[i, 1])) {
          col_to_del <- c(col_to_del, i)
          deleted    <- c(deleted, del)
        }
      }
      if (!is.null(col_to_del)) {
        corrr <- corrr[-col_to_del, ]
      }
    }

    return(
      list(
        data         = data[, !(colnames(data) %in% unique(deleted))],
        deleted_cols = unique(deleted)
      )
    )
  } else {
    return(
      list(
        data         = data,
        deleted_cols = NULL
      )
    )
  }
}


#' Delete columns that are ID-like columns
#'
#' @param data A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#'
#' @return A dataset with removed ID columns.
#' @export
delete_id_columns <- function(data) {
  names    <- colnames(data)
  id_names <- c('id', 'no', 'nr', 'number', 'idx', 'identification')
  sus      <- c()
  for (i in 1:ncol(data)) {
    if (tolower(names[i]) %in% id_names) {
      sus <- c(sus, i)
    }
    if (all.equal(data[, i], as.integer(data[, i])) == TRUE &&
        length(unique(data[, i])) == nrow(data)) {
      sus <- c(sus, i)
    }
  }
  sus <- unique(sus)

  if (length(sus) > 0) {
    data <- data[, -sus]
  }
  return(data)
}

#' Perform Boruta algorithm for selecting most important features
#'
#' @param data A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#' @param y A string that indicates a target column name.
#'
#' @return A dataset with removed columns that are not needed according to the Boruta algorithm.
#' @export
boruta_selection <- function(data, y) {
  boruta.bank_train            <- eval(parse(text = paste0('Boruta::Boruta(', y, '~.,', ' data', ')')))
  suppressWarnings(boruta.bank <- Boruta::TentativeRoughFix(boruta.bank_train))
  selection                    <- boruta.bank$finalDecision
  selection                    <- names(selection[selection == 'Confirmed'])
  selection                    <- c(selection, y)
  data                         <- data[, selection]

  return(data)
}
