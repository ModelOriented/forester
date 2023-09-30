#' Run data check pipeline to seek for potential problems with the data
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with two vectors: lines of the report (str) and the outliers (outliers).
#' @export
#'
#' @examples
#' check_data(iris[1:100, ], 'Species')
#' check_data(lisbon, 'Price')
#' check_data(compas, 'Two_yr_Recidivism')
#' check_data(iris, 'Species')
#' check_data(lymph, 'class')
#' @importFrom stats IQR cor median sd
#' @importFrom utils capture.output
check_data <- function(data, y = NULL, time = NULL, status = NULL, verbose = TRUE) {
  options(warn = -1)

  if (is.null(y)) {
    if (is.null(time) | is.null(status)) {
      verbose_cat(crayon::red('\u2716'), 'Lack of target variables. Please specify',
                  'either y (for classification or regression tasks), or time and',
                  'status (for survival analysis). \n\n', verbose = verbose)
      return(NULL)
    }
  } else {
    if (!is.null(time) | !is.null(status)) {
      verbose_cat(crayon::red('\u2716'), 'Provided too many targets. Please specify',
                  'either y (for classification or regression tasks), or time and',
                  'status (for survival analysis). \n\n', verbose = verbose)
      return(NULL)
    }
  }

  df  <- as.data.frame(data)
  str <- capture.output(cat(' -------------------- **CHECK DATA REPORT** -------------------- \n \n'))
  verbose_cat(' -------------------- CHECK DATA REPORT -------------------- \n \n', verbose = verbose)
  str <- c(str, basic_info(df, y, time, status, verbose))
  str <- c(str, check_static(df, verbose))
  str <- c(str, check_duplicate_col(df, verbose))
  str <- c(str, check_missing(df, y, time, status, verbose))
  df  <- manage_missing(df, y)
  str <- c(str, check_dim(df, verbose))
  str <- c(str, check_cor(df, y, time, status, verbose)$str)
  rtr <- check_outliers(df, verbose)
  str <- c(str, rtr$str)
  str <- c(str, check_y_balance(df, y, time, status, verbose))
  str <- c(str, detect_id_columns(df, verbose))
  verbose_cat(' -------------------- CHECK DATA REPORT END -------------------- \n \n', verbose = verbose)
  str <- c(str,
           capture.output(cat(' -------------------- **CHECK DATA REPORT END** -------------------- \n \n')))

  ret <- list(
    str      = str,
    outliers = rtr$outliers
  )
  return(ret)
}


#' Provide basic dataset information
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
basic_info <- function(df, y = NULL, time = NULL, status = NULL, verbose = TRUE) {
  # Distinction between survival analysis and other tasks.
  if (!is.null(y)) {
    target <- paste0('a column ', y, '.')
    target2 <- paste0('a column** ', y, '.')
  } else {
    target <- paste0('columns ', time, ' for time and ', status, ' for status.')
    target2 <- paste0('columns** ', time, ' for time and ', status, ' for status.')
  }
  verbose_cat('The dataset has ', nrow(df), ' observations and ', ncol(df),
      ' columns, which names are: \n', paste0(colnames(df), sep='; '),
      '\n\nWith the target described by ', target, '\n\n', sep = '', verbose = verbose)
  str <- capture.output(cat('**The dataset has ', nrow(df), ' observations and ', ncol(df),
                            ' columns which names are: **\n\n', paste0(colnames(df), sep='; '),
                            '\n\n **With the target described by** ', target, '\n \n', sep = ''))
  return(str)
}


#' Search for columns dominated by a single value
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_static <- function(df, verbose = TRUE) {
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
    verbose_cat(crayon::green('\u2714'), 'No static columns. \n', verbose = verbose)
    str <- capture.output(cat('**No static columns. **\n\n'))

  } else {
    verbose_cat(crayon::red('\u2716'), ' Static columns are: \n ', dominator_cols, '\n\n', sep = '', verbose = verbose)
    verbose_cat(crayon::red('\u2716'), ' With dominating values: \n', ' ',  dominator_vals, '\n ', sep = '', verbose = verbose)
    str <- capture.output(cat('**Static columns are: **', dominator_cols, '\n\n', sep = ''))
    str <- c(str, capture.output(cat('**With dominating values: **', dominator_vals,
                                     '\n\n', sep = '')))
  }
  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))

  return(str)
}


#' Search for duplicates between columns
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_duplicate_col <- function(df, verbose = TRUE) {
  pairs <- c()

  for (i in 1:ncol(df)) {
    for (j in i:ncol(df)) {
      if (i != j && identical(df[, i], df[, j])) {
        pairs <- c(pairs, c(colnames(df[i]), ' - ', colnames(df[j]), '; '))
      }
    }
  }
  if (length(pairs) == 0) {
    verbose_cat(crayon::green('\u2714'), 'No duplicate columns.\n', verbose = verbose)
    str <- capture.output(cat('**No duplicate columns.**\n'))
  } else {
    verbose_cat(crayon::red('\u2716'), ' These column pairs are duplicate:\n ', pairs, '\n', sep = '', verbose = verbose)
    str <- capture.output(cat('**These column pairs are duplicate: **\n', pairs, '\n\n',
                              sep = ''))
  }
  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))

  return(str)
}


#' Search for missing values in the target column and predictors
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_missing <- function(df, y = NULL, time = NULL, status = NULL, verbose = TRUE) {
  # Distinction between survival analysis and other tasks.
  if (!is.null(y)) {
    df_sm     <- df[, !names(df) %in% y]
    missing_y <- length(df[[y]][df[[y]] == ''])
  } else {
    df_sm     <- df[, !names(df) %in% c(time, status)]
    missing_y <- length(df[df[[time]] == '' & df[[status]] == '', ])
  }
  missing_x <- 0

  for (i in 1:nrow(df_sm)) {
    if (length(df[i, ][df[i, ] == '']) != 0) {
      missing_x <- missing_x + 1
    }
  }
  if (missing_y == 0) {
    verbose_cat(crayon::green('\u2714'), 'No target values are missing. \n\n', verbose = verbose)
    str <- capture.output(cat('**No target values are missing. **\n\n'))
  } else {
    verbose_cat(crayon::red('\u2716'), ' ', missing_y,' Target values are missing. \n\n', sep = '', verbose = verbose)
    str <- capture.output(cat(missing_y, ' **Target values are missing.**\n\n', sep = ''))
  }

  if (missing_x == 0) {
    verbose_cat(crayon::green('\u2714'), 'No predictor values are missing. \n', verbose = verbose)
    str <- c(str, capture.output(cat('**No predictor values are missing. **\n')))
  } else {
    verbose_cat(crayon::red('\u2716'), ' ', missing_x, ' observations have missing fields.\n',   sep = '', verbose = verbose)
    str <- c(str, capture.output(cat('** ', missing_x, ' observations have missing fields.**\n', sep = '')))
  }

  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))
  return(str)
}


#' Search for dimensionality problems in the dataset
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_dim <- function(df, verbose = TRUE) {
  rows <- dim(df)[1]
  cols <- dim(df)[2]

  if (cols > 30) {
    verbose_cat(crayon::red('\u2716'), ' Too big dimensionality with ', cols, ' colums. Forest models wont use so many of them. \n', sep = '', verbose = verbose)
    str <- capture.output(cat('**Too big dimensionality with ', cols, ' colums. Forest models wont use so many of them. **\n', sep = ''))
  }
  if (cols >= rows) {
    verbose_cat(crayon::red('\u2716'), ' More features than observations, try reducing dimensionality or add new observations. \n', verbose = verbose)
    str <- capture.output(cat('**More features than observations, try reducing dimensionality or add new observations. **\n'))
  }
  if (cols < rows && cols <= 30) {
    verbose_cat(crayon::green('\u2714'), ' No issues with dimensionality. \n', verbose = verbose)
    str <- capture.output(cat('**No issues with dimensionality. **\n'))
  }
  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))
  return(str)
}


#' Search for strongly correlated values (Spearman's rank for numerical, Crammer's V for
#' categorical)
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_cor <- function(df, y = NULL, time = NULL, status = NULL, verbose = TRUE) {
  # Distinction between survival analysis and other tasks.
  if (!is.null(y)) {
    data  <- df[, !names(df) %in% y]
  } else {
    data  <- df[, !names(df) %in% c(time, status)]
  }
  data    <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  num_idx <- c()
  fct_idx <- c()

  for (i in 1:ncol(data)) {
    if (is.factor(data[, i])) {
      if (length(levels(data[, i])) > 1) {
        # CramerV doesn't work with a singular level, and we check columns
        # with a single value before, so no need for an additional warning.
        fct_idx <- c(fct_idx, i)
      }
    } else {
      num_idx <- c(num_idx, i)
    }
  }
  num_names <- NULL
  cor_num   <- NULL
  if (length(num_idx) != 0) {
    cor_num    <- round(cor(data[num_idx]), 2)
    no_cor_num <- TRUE
    num_names  <- colnames(cor_num)

    for (i in 1:ncol(cor_num)) {
      for (j in i:ncol(cor_num)) {

        if (is.na(abs(cor_num[i, j]) >= 0.7)) {
          strong_Spearman_cor <- FALSE
        } else {
          strong_Spearman_cor <- (abs(cor_num[i, j]) >= 0.7)
        }
        if (i != j && strong_Spearman_cor) {
          if (no_cor_num) {
            verbose_cat(crayon::red('\u2716'), 'Strongly correlated, by Spearman rank, pairs of numerical values are: \n', verbose = verbose, '\n')
            str <- capture.output(cat('**Strongly correlated, by Spearman rank, pairs of numerical values are: **\n\n'))
            no_cor_num <- FALSE
          }
          verbose_cat(' ', num_names[i], ' - ', num_names[j], ': ', cor_num[i, j], ';\n', sep = '', verbose = verbose)
          str <- c(str, capture.output(
            cat(' ', num_names[i], ' - ', num_names[j], ': ', cor_num[i, j], ';\n', sep = '')))
        }
      }
    }
    if (no_cor_num) {
      verbose_cat(crayon::green('\u2714'), 'No strongly correlated, by Spearman rank, pairs of numerical values. \n', verbose = verbose)
      str <- capture.output(cat('**No strongly correlated, by Spearman rank, pairs of numerical values. **\n\n'))
    }
    verbose_cat('\n', verbose = verbose)
  }

  fct_names <- NULL
  cor_fct   <- NULL
  if (length(fct_idx) != 0) {
    fct_tbl <- data[fct_idx]
    cor_fct <- matrix(0, nrow = length(fct_idx), ncol = length(fct_idx))

    colnames(cor_fct)  <- names(fct_tbl)
    row.names(cor_fct) <- names(fct_tbl)

    for (i in 1:length(fct_idx)) {
      for (j in 1:length(fct_idx)) {

        a = as.numeric(length(unique(fct_tbl[, i])))
        b = as.numeric(length(unique(fct_tbl[, j])))

        if (as.numeric(a * b) <= 2^29 - 1){ # with 2^31 we have to big vector that can't be allocated (over 7.4 Gb)
          cor_fct[i, j] <- round(rcompanion::cramerV(fct_tbl[, i], fct_tbl[, j]), 2)
        } else {
          cor_fct[i, j] <- NA
          verbose_cat(crayon::red('\nWARNING!'), ' Correlation: ', colnames(cor_fct)[i], ' - ', colnames(cor_fct)[j], ' was ommited because of too much unique values. \n', verbose = verbose)
          str <- c(str, capture.output(cat('\nWARNING!', ' Correlation: ', colnames(cor_fct)[i], ' - ', colnames(cor_fct)[j], ' was ommited because of too much unique values. **\n')))
        }
      }
    }
    no_cor_fct = TRUE
    fct_names <- colnames(cor_fct)

    for (i in 1:ncol(cor_fct)) {
      for (j in i:ncol(cor_fct)) {

        if (is.na(abs(cor_fct[i, j]) >= 0.7)) {
          strong_V_cor <- FALSE
        } else {
          strong_V_cor <- (abs(cor_fct[i, j]) >= 0.7)
        }
        if (i != j && strong_V_cor) {
          if (no_cor_fct) {
            verbose_cat(crayon::red('\u2716'), 'Strongly correlated, by Crammer\'s V rank, pairs of categorical values are: \n', verbose = verbose)
            str <- c(str, capture.output(cat('\n', '**Strongly correlated, by Crammer\'s V rank, pairs of categorical values are: **\n\n')))
            no_cor_fct = FALSE
          }
          verbose_cat(' ', fct_names[i], ' - ', fct_names[j], ': ', cor_fct[i, j], ';\n', sep = '', verbose = verbose)
          str <- c(str, capture.output(cat(' ', fct_names[i], ' - ', fct_names[j], ': ', cor_fct[i, j], ';\n', sep = '')))
        }
      }
    }
    if (no_cor_fct) {
      verbose_cat(crayon::green('\u2714'), 'No strongly correlated, by Crammer\'s V rank, pairs of categorical values. \n', verbose = verbose)
      str <- c(str, capture.output(
        cat('**No strongly correlated, by Crammer\'s V rank, pairs of categorical values. **\n')))
    }
    verbose_cat('\n', verbose = verbose)
  }

  str <- c(str, capture.output(cat('\n')))

  return(
    list(
      str       = str,
      num_names = num_names,
      cor_num   = cor_num,
      fct_names = fct_names,
      cor_fct   = cor_fct
    )
  )
}


#' Search for outliers via mean standard deviation, median absolute deviation
#' and inter quantile range
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_outliers <- function(df, verbose = TRUE) {
  # Methods from: https://www.reneshbedre.com/blog/find-outliers.html
  mean_standard_deviation <- function(x) {
    mean <- mean(x)
    std  <- sd(x)
    Tmin <- mean - (3 * std)
    Tmax <- mean + (3 * std)
    return (which(x < Tmin | x > Tmax))
  }

  median_absolute_deviation <- function(x) {
    med     <- median(x)
    abs_dev <- abs(x - med)
    mad     <- 1.4826 * median(abs_dev) # for normally distributed data
    Tmin    <- med - (3 * mad)
    Tmax    <- med + (3 * mad)
    return (which(x < Tmin | x > Tmax))
  }

  inter_quantile_range <- function(x) {
    Tmin <- summary(x)[2] - (1.5 * IQR(x))
    Tmax <- summary(x)[4] + (1.5 * IQR(x))
    return (which(x < Tmin | x > Tmax))
  }

  data    <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
  num_idx <- c()

  for (i in 1:ncol(data)) {
    if (!is.factor(data[, i])) {
      num_idx <- c(num_idx, i)
    }
  }
  data_num <- data[num_idx]
  outliers <- c()

  if (ncol(data_num) != 0) {
    for (i in 1:ncol(data_num)) {
      outliers_tmp <- c()
      outliers_tmp <- c(outliers_tmp, mean_standard_deviation(data_num[, i]))
      outliers_tmp <- c(outliers_tmp, median_absolute_deviation(data_num[, i]))
      outliers_tmp <- c(outliers_tmp, inter_quantile_range(data_num[, i]))
      bool_outlier <- table(outliers_tmp)
      outliers     <- c(outliers, names(bool_outlier[bool_outlier == 3]))
    }

    outliers <- unique(outliers)
    outliers <- sort(outliers)

    if (length(outliers) == 0) {
      verbose_cat(crayon::green('\u2714'), 'No outliers in the dataset. \n', verbose = verbose)
      str <- capture.output(cat('**No outliers in the dataset. **\n'))
    } else if (length(outliers) < 50) {
      verbose_cat(crayon::red('\u2716'), 'These observations migth be outliers due to their numerical columns values: \n', outliers, ';\n', verbose = verbose)
      str <- capture.output(cat('**These observations migth be outliers due to their numerical columns values: **\n\n', outliers, ';\n'))
    } else {
      verbose_cat(crayon::red('\u2716'), 'There are more than 50 possible outliers in the data set, so we are not printing them. They are returned in the output as a vector. \n', verbose = verbose)
      str <- capture.output(cat('**There are more than 50 possible outliers in the data set, so we are not printing them. They are returned in the output as a vector. **\n'))
    }
    verbose_cat('\n', verbose = verbose)

    str      <- c(str, capture.output(cat('\n')))
    outliers <- as.numeric(outliers)
  } else {
    verbose_cat(crayon::green('\u2714'), 'No numeric data in the dataset. \n', verbose = verbose)
    str <- capture.output(cat('**No numeric data in the dataset. **\n'))
    verbose_cat('\n', verbose = verbose)
  }

  rtr <- list(
    str       = str,
    outliers  = outliers
  )

  return(rtr)
}


#' Check whether the target column is unbalanced (for regression it bins values
#' via quantiles)
#'
#' @param df A data source, that is one of the major R formats: data.table, data.frame,
#' matrix, and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#'
#' @export
check_y_balance <- function(df, y = NULL, time = NULL, status = NULL, verbose = TRUE) {
  type     <- guess_type(df, y)
  # Distinction between survival analysis and other tasks.
  if (!is.null(y)) {
    target <- df[[y]]
  } else {
    target <- df[[status]]
  }

  if (type %in% c('binary_clf', 'survival')) {
    if (table(target)[1] / table(target)[2] > 1.5 || table(target)[1] / table(target)[2] < 0.75) {
      if (table(target)[1] > table(target)[2]) {
        dominating <- rownames(table(target))[1]
      } else {
        dominating <- rownames(table(target))[2]
      }
      verbose_cat(crayon::red('\u2716'), 'Dataset is unbalanced with:', table(target)[1] / table(target)[2], 'proportion with', dominating, 'being a dominating class.\n', verbose = verbose)
      str <- capture.output(
        cat('**Dataset is unbalanced with:**', table(target)[1] / table(target)[2], 'proportion with', dominating, 'being a dominating class.\n'))

    } else {
      verbose_cat(crayon::green('\u2714'), 'Dataset is balanced. \n', verbose = verbose)
      str <- capture.output(cat('**Dataset is balanced. **\n'))
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
      } else {
        bins[4] <- bins[4] + 1
      }
    }
    balanced <- TRUE
    for (i in 1:4) {
      for (j in i:4) {
        if (bins[j] == 0) {
          if (bins[i] > 0) {
            balanced <- FALSE
          }
        } else {
          if (bins[i] / bins[j] > 1.5) {
            balanced <- FALSE
          }
        }
      }
    }
    perc_bins <- round(bins / sum(bins), 2)

    if (balanced) {
      verbose_cat(crayon::green('\u2714'), 'Target data is evenly distributed. \n', verbose = verbose)
      str <- capture.output(cat('**Target data is evenly distributed. **\n'))

    } else {
      verbose_cat(crayon::red('\u2716'), 'Target data is not evenly distributed with quantile bins:', perc_bins, '\n', verbose = verbose)
      str <- capture.output(cat('**Target data is not evenly distributed with quantile bins:**', perc_bins, '\n'))
    }

  } else if (type == 'multi_clf') {
    verbose_cat(crayon::green('\u2716'), 'Multilabel classification is not supported yet. \n', verbose = verbose)
    str <- capture.output(cat('**Multilabel classification is not supported yet. **\n'))
  }
  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))
  return(str)
}


#' Detect columns that are ID-like columns
#'
#' @param data A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list with every line of the sub-report.
#' @export
detect_id_columns <- function(data, verbose = TRUE) {
  names     <- colnames(data)
  id_names  <- c('id', 'nr', 'number', 'idx', 'identification', 'index')
  sus_names <- c()
  sus_data  <- c()
  for (i in 1:ncol(data)) {
    if (tolower(names[i]) %in% id_names) {
      sus_names <- c(sus_names, names[i])
    }

    if (isTRUE(all.equal(data[, i], as.integer(data[, i]))) &&
        length(unique(data[, i])) == nrow(data)) {
      sus_data <- c(sus_data, names[i])
    }
  }

  if (length(sus_names) > 0) {
    verbose_cat(crayon::red('\u2716'), 'Columns names suggest that some of them are IDs, removing them can improve the model.\n Suspicious columns are:',
        sus_names, '.\n\n', verbose = verbose)
    str <- capture.output(
      cat('**Columns names suggest that some of them are IDs, removing them can improve the model. Suspicious columns are: **\n\n',
          sus_names, '\n\n'))
  } else {
    verbose_cat(crayon::green('\u2714'), 'Columns names suggest that none of them are IDs. \n\n', verbose = verbose)
    str <- capture.output(
      cat('**Columns names suggest that none of them are IDs. **\n\n'))
  }
  if (length(sus_data) > 0) {
    verbose_cat(crayon::red('\u2716'), 'Columns data suggest that some of them are IDs, removing them can improve the model.\n Suspicious columns are:',
        sus_data, '.\n', verbose = verbose)
    str <- c(str, capture.output(
      cat('**Columns data suggest that some of them are IDs, removing them can improve the model. Suspicious columns are: **\n\n',
          sus_data, '\n\n')))
  } else {
    verbose_cat(crayon::green('\u2714'), 'Columns data suggest that none of them are IDs. \n', verbose = verbose)
    str <- c(str, capture.output(
      cat('**Columns data suggest that none of them are IDs. **\n\n')))
  }

  verbose_cat('\n', verbose = verbose)
  str <- c(str, capture.output(cat('\n')))
  return(str)
}
