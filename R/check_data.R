#' Runs data check pipeline to seek for potential problems with the data
#'
#' @param data A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A list with every line of the report.
#' @export
#'
#' @examples
#' check_data(iris[1:100,], 'Species')
#' check_data(lisbon, 'Price')
#' check_data(compas, 'Two_yr_Recidivism')
#' check_data(iris, 'Species')
#' check_data(lymph, 'class')
#' @importFrom stats IQR cor median sd
#' @importFrom utils capture.output
check_data <- function(data, y) {
  df <- as.data.frame(data)
  str <- capture.output(cat(' -------------------- CHECK DATA REPORT END -------------------- \n \n'))
  cat(' ---------------------- CHECK DATA REPORT ---------------------- \n \n')
  str <- c(str, basic_info(df, y))
  str <- c(str, check_static(df))
  str <- c(str, check_duplicate_col(df))
  str <- c(str, check_missing(df, y))
  df  <- manage_missing(df, y)
  str <- c(str, check_dim(df))
  str <- c(str, check_cor(df, y))
  str <- c(str, check_outliers(df))
  str <- c(str, check_y_balance(df, y))
  cat(' -------------------- CHECK DATA REPORT END -------------------- \n \n')
  str <- c(str,
           capture.output(cat(' -------------------- CHECK DATA REPORT END -------------------- \n \n')))
  return(str)
}

#' Provides basic dataset information
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
basic_info <- function(df, y) {
  cat('The dataset has ', nrow(df), ' observations and ', ncol(df),
      ' columns, which names are: \n', paste0(colnames(df), sep='; '),
      '\nWith the target value described by column ', y, '.', '\n \n', sep = '')
  str <- capture.output(cat('The dataset has ', nrow(df), ' observations and ', ncol(df),
                            ' columns which names are: \n', paste0(colnames(df), sep='; '),
                            '\nWith the target value described by column ', y, '.', '\n \n', sep = ''))
  return(str)
}

#' Searches for columns dominated by a single value
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_static <- function(df) {
  dominator      <- FALSE
  dominator_vals <- c()
  dominator_cols <- c()
  for (i in 1:ncol(df)) {
    if (any(table(df[, i]) / nrow(df) > 0.99)) {
      is_dominating <- (table(df[, i]) / nrow(df) > 0.99)

      for (j in 1:length(is_dominating)) {
        if (is_dominating[j]) {
          dominator      <- TRUE
          dominator_vals <- c(dominator_vals, names(is_dominating)[j], '; ')
          dominator_cols <- c(dominator_cols, names(df)[i], '; ')
        }
      }
    }
  }
  if (!dominator) {
    cat('No static columns. \n')
    str <- capture.output(cat('No static columns. \n'))

  } else{
    cat('Static columns are: ', dominator_cols, '\n', sep = '')
    cat('With dominating values: ', dominator_vals, '\n', sep = '')
    str <- capture.output(cat('Static columns are: ', dominator_cols, '\n', sep = ''))
    str <- c(str, capture.output(cat('With dominating values: ', dominator_vals,
                                     '\n', sep = '')))
  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}
#' Searches for duplicates between columns
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_duplicate_col <- function(df) {

  pairs <- c()

  for (i in 1:ncol(df)) {
    for (j in i:ncol(df)) {
      if (i != j && identical(df[, i], df[, j])) {
        pairs <- c(pairs, c(colnames(df[i]), ' - ', colnames(df[j]), '; '))
      }
    }
  }
  if (length(pairs) == 0) {
    cat('No duplicate columns.')
    str <- capture.output(cat('No duplicate columns.'))

  } else{
    cat('These column pairs are duplicate: ', pairs, '\n', sep = '')
    str <- capture.output(cat('These column pairs are duplicate: ', pairs, '\n',
                              sep = ''))
  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}

#' Searches for missing values in target column and predictors
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_missing <- function(df, y) {

  df_sm     <- df[, !names(df) %in% y]
  missing_y <- length(df[[y]][df[[y]] == ''])
  missing_x <- 0

  for (i in 1:nrow(df_sm)) {
    if (length(df[i,][df[i,] == '']) != 0) {
      missing_x = missing_x + 1
    }
  }

  if (missing_y == 0) {
    cat('No target values are missing. \n')
    str <- capture.output(cat('No target values are missing. \n'))

  } else{
    cat(missing_y, ' target values are missing.', sep = '')
    str <- capture.output(cat(missing_y, ' target values are missing.', sep = ''))

  }

  if (missing_x == 0) {
    cat('No predictor values are missing. \n')
    str <- c(str, capture.output(cat('No predictor values are missing. \n')))

  } else{
    cat(missing_x, ' observations have missing fields.', sep = '')
    str <- c(str, capture.output(cat(missing_x, ' observations have missing fields.', sep = '')))

  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}

#' Searches for dimensionality problems in the dataset
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_dim <- function(df) {
  rows <- dim(df)[1]
  cols <- dim(df)[2]

  if (cols > 30) {
    cat('Too big dimensionality with ', cols, ' colums. Forest models wont use so many of them. \n', sep = '')
    str <- capture.output(
      cat('Too big dimensionality with ', cols, ' colums. Forest models wont use so many of them. \n', sep = ''))

  }
  if (cols >= rows) {
    cat('More features than observations, try reducing dimensionality or add new observations. \n')
    str <- capture.output(
      cat('More features than observations, try reducing dimensionality or add new observations. \n'))

  }
  if (cols < rows && cols <= 30) {
    cat('No issues with dimensionality. \n')
    str <- capture.output(cat('No issues with dimensionality. \n'))

  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}

#' Searches for strongly correlated values (Spearman for numerical, Crammer V for
#' categorical)
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_cor <- function(df, y) {

  data    <- df[, !names(df) %in% y]
  data    <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  num_idx <- c()
  fct_idx <- c()

  for (i in 1:ncol(data)) {
    if (is.factor(data[, i])) {
      if (length(levels(data[, i])) > 1) {
        # CramerV doesnt work with singular level and
        # we check columns with single value before so no need for additional warning
        fct_idx <- c(fct_idx, i)
      }
    } else{
      num_idx <- c(num_idx, i)
    }
  }

  if (length(num_idx) != 0) {
    cor_num    <- round(cor(data[num_idx]), 2)
    no_cor_num <- TRUE
    num_names  <- colnames(cor_num)

    for (i in 1:ncol(cor_num)) {
      for (j in i:ncol(cor_num)) {
        if (i != j && cor_num[i, j] >= 0.7) {
          if (no_cor_num) {
            cat('Strongly correlated pairs of numerical values are: \n')
            str <- capture.output(cat('Strongly correlated pairs of numerical values are: \n'))
            no_cor_num <- FALSE

          }
          cat(num_names[i], ' - ', num_names[j], ': ', cor_num[i, j], ';\n', sep = '')
          str <- c(str, capture.output(
            cat(num_names[i], ' - ', num_names[j], ': ', cor_num[i, j], ';\n', sep = '')))
        }
      }
    }
    if (no_cor_num) {
      cat('No strongly correlated pairs of numerical values. \n')
      str <- capture.output(cat('No strongly correlated pairs of numerical values. \n'))
    }
  }

  if (length(fct_idx) != 0) {
    fct_tbl <- data[fct_idx]
    cor_fct <- matrix(0, nrow = length(fct_idx), ncol = length(fct_idx))

    colnames(cor_fct)  <- names(fct_tbl)
    row.names(cor_fct) <- names(fct_tbl)

    for (i in 1:length(fct_idx)) {
      for (j in 1:length(fct_idx)) {
        cor_fct[i, j] <- round(rcompanion::cramerV(fct_tbl[, i], fct_tbl[, j]), 2)
      }
    }
    no_cor_fct = TRUE
    fct_names <- colnames(cor_fct)

    for (i in 1:ncol(cor_fct)) {
      for (j in i:ncol(cor_fct)) {
        if (i != j && cor_fct[i, j] >= 0.7) {
          if (no_cor_fct) {
            cat('\nStrongly correlated pairs of categorical values are: \n')
            str <- c(str, capture.output(cat('\nStrongly correlated pairs of categorical values are: \n')))
            no_cor_fct = FALSE

          }
          cat(fct_names[i], ' - ', fct_names[j], ': ', cor_fct[i, j], ';\n', sep = '')
          str <- c(str, capture.output(cat(fct_names[i], ' - ', fct_names[j], ': ', cor_fct[i, j], ';\n', sep = '')))
        }
      }
    }
    if (no_cor_fct) {
      cat('No strongly correlated pairs of categorical values. \n')
      str <- c(str, capture.output(
        cat('No strongly correlated pairs of categorical values. \n')))
    }
  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}

#' Searches for outliers via mean standard deviation, median abosulte deviation
#' and inter quantile range
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_outliers <- function(df) {
  # Methods from: https://www.reneshbedre.com/blog/find-outliers.html
  mean_standard_deviation <- function(x) {
    mean = mean(x)
    std = sd(x)
    Tmin = mean - (3 * std)
    Tmax = mean + (3 * std)
    return (which(x < Tmin | x > Tmax))
  }

  median_absolute_deviation <- function(x) {
    med = median(x)
    abs_dev = abs(x - med)
    mad = 1.4826 * median(abs_dev) # for normally distributed data
    Tmin = med - (3 * mad)
    Tmax = med + (3 * mad)
    return (which(x < Tmin | x > Tmax))
  }

  inter_quntile_range <- function(x) {
    Tmin = summary(x)[2] - (1.5 * IQR(x))
    Tmax = summary(x)[4] + (1.5 * IQR(x))
    return (which(x < Tmin | x > Tmax))
  }

  data <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
  num_idx <- c()

  for (i in 1:ncol(data)) {
    if (!is.factor(data[, i])) {
      num_idx <- c(num_idx, i)
    }
  }
  data_num <- data[num_idx]
  outliers <- c()

  for (i in 1:ncol(data_num)) {
    outliers_tmp <- c()
    outliers_tmp <- c(outliers_tmp, mean_standard_deviation(data_num[, i]))
    outliers_tmp <- c(outliers_tmp, median_absolute_deviation(data_num[, i]))
    outliers_tmp <- c(outliers_tmp, inter_quntile_range(data_num[, i]))
    bool_outlier <- table(outliers_tmp)
    outliers     <- c(outliers, names(bool_outlier[bool_outlier == 3]))
  }

  outliers <- unique(outliers)
  outliers <- sort(outliers)
  if (length(outliers) == 0) {
    cat('No outliers in the dataset. \n')
    str <- capture.output(cat('No outliers in the dataset. \n'))

  } else{
    cat('These obserwation migth be outliers due to their numerical columns values: \n', outliers, ';\n')
    str <- capture.output(
      cat('These obserwation migth be outliers due to their numerical columns values: \n', outliers, ';\n'))
  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}

#' Checks wheter target column is unbalanced (for regression it bins values
#' via quantiles)
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_y_balance <- function(df, y) {
  type   <- guess_type(df, y)
  target <- df[[y]]

  if (type %in% c('binary_clf')) {
    if (table(target)[1] / table(target)[2] > 1.5 || table(target)[1] / table(target)[2] < 0.75) {
      cat('Dataset is unbalanced with ', table(target)[1] / table(target)[2], ' proportion. \n')
      str <- capture.output(
        cat('Dataset is unbalanced with ', table(target)[1] / table(target)[2], ' proportion. \n'))

    } else{
      cat('Dataset is balanced. \n')
      str <- capture.output(cat('Dataset is balanced. \n'))
    }
  } else if (type == 'regression') {

    Q1   <- summary(target)[2]
    Mean <- summary(target)[4]
    Q2   <- summary(target)[5]
    bins <- c(0, 0, 0, 0)

    for (i in 1:length(target)) {
      if (target[i] < Q1) {
        bins[1] <- bins[1] + 1
      } else if (target[i] < Mean) {
        bins[2] <- bins[2] + 1
      } else if (target[i] < Q2) {
        bins[3] <- bins[3] + 1
      } else{
        bins[4] <- bins[4] + 1
      }
    }

    balanced <- TRUE
    for (i in 1:4) {
      for (j in i:4) {
        if (bins[i] / bins[j] > 1.5) {
          balanced <- FALSE
        }
      }
    }

    perc_bins <- round(bins / sum(bins), 2)

    if (balanced) {
      cat('Target data is evenly distributed. \n')
      str <- capture.output(cat('Target data is evenly distributed. \n'))

    } else{
      cat('Target data is not evenly distributed with quantile bins:', perc_bins, '\n')
      str <- capture.output(
        cat('Target data is not evenly distributed with quantile bins:', perc_bins, '\n'))
    }


  } else if (type == 'multi_clf') {
    cat('Multilabel classification is not supported yet. \n')
    str <- capture.output(cat('Multilabel classification is not supported yet. \n'))
  }
  cat('\n')
  str <- c(str, capture.output(cat('\n')))

  return(str)
}
