#' Conducts a feature selection process with one out of four proposed methods
#'
#' \itemize{
#' \item \code{`VI`} The variable importance method based on random forest - short time,
#' \item \code{`MCFS`} The Monte Carlo Feature Selection - long time,
#' \item \code{`MI`} The Varrank method based on mutual information scores - moderate time,
#' if we set too big `max_features` it can work really long,
#' \item \code{`BORUTA`} The BORUTA algorithm - short time.
#' }
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param feature_selection_method A string value indication the feature selection method.
#' The imputation method must be one of 'VI', 'MCFS', 'MI', or 'BORUTA'.
#' @param max_features A positive integer value describing the desired number of
#' selected features. Initial value set as 'default' which is 10 for `VI` and `MI`, and
#' NULL (number of relevant features chosen by the method) for `MCFS`.
#' Only `MCFS` can use the NULL value. `BORUTA` doesn't use this parameter.
#' @param nperm An integer describing the number of permutations performed, relevant
#' for the `VI` method. By default set to 1.
#' @param cutoffPermutations An non-negative integer value that determines the number of permutation
#' runs. It needs at least 20 permutations for a statistically significant result.
#' Minimum value of this parameter is 3, however if it is 0 then permutations
#' method is turned off. Relevant for the `MCFS` method.
#' @param threadsNumber A positive integer value describing the number of threads to use
#' in computation. More threads needs more CPU cores as well as memory usage is
#' a bit higher. It is recommended to set this value equal to or less than CPU
#' available cores. By default set to NULL, which will use maximal number of cores
#' minus 1. Relevant for the `MCFS` method.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with selected columns,
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
preprocessing_feature_selection <- function(data,
                                            y,
                                            feature_selection_method = 'VI',
                                            max_features = 'default',
                                            nperm = 1,
                                            cutoffPermutations = 20,
                                            threadsNumber = NULL) {

  if (!feature_selection_method %in% c('VI', 'MCFS', 'MI', 'BORUTA')) {
    verbose_cat(crayon::red('\u2716'), 'Preprocessing feature selection: The feature selection method must be one of VI, MCFS, MI, or BORUTA.', '\n', verbose = TRUE)
    stop('Preprocessing feature selection: The feature selection method must be one of VI, MCFS, MI, or BORUTA.')
  }

  if (feature_selection_method == 'VI') {
    if (max_features == 'default') {
      max_features <- 10
    }
    fs_data <- select_vi(data, y, nperm = 1, max_features = max_features)
  } else if (feature_selection_method == 'MCFS') {
    if (max_features == 'default') {
      max_features <- NULL
    }
    fs_data <- select_mcfs(data, y, cutoffPermutations = cutoffPermutations, threadsNumber = threadsNumber, max_features = max_features)
  } else if (feature_selection_method == 'MI') {
    if (max_features == 'default') {
      max_features <- 10
    }
    fs_data <- select_mi_varrank(data, y, max_features = max_features)
  } else if (feature_selection_method == 'BORUTA') {
    if (max_features == 'default') {
      max_features <- NULL
    }
    fs_data <- select_boruta(data, y)
  }

  return(list(
    data   = fs_data$data,
    rm_col = fs_data$idx
  ))
}

#' Feature Selection performed by Variable Importance algorithm based on random forest
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param nperm An integer describing the number of permutations performed.
#' By default set to 1.
#' @param max_features A positive integer value describing the desired number of
#' selected features. By default set to 10.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with selected columns,
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
select_vi <- function(data, y, nperm = 1, max_features = 10) {
  if (nperm < 1 || as.integer(nperm) != nperm) {
    verbose_cat(crayon::red('\u2716'), 'VI feature selection: Number of permutations must be an integer greater-equal than 1.', '\n', verbose = TRUE)
    stop('VI feature selection: Number of permutations must be an integer greater-equal than 1.')
  }
  if (max_features < 1 || max_features >= ncol(data) || as.integer(max_features) != max_features) {
    verbose_cat(crayon::red('\u2716'), 'VI feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).', '\n', verbose = TRUE)
    stop('VI feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).')
  }
  data_org <- data
  for (i in 1:ncol(data)) {
    if (!is.numeric(data[, i])) {
      data[, i] <- as.factor(data[, i])
      # Cforest can't handle features with more than 30 levels.
      if (length(levels(data[, i])) >= 31) {
        data[, i] <- as.integer(data[, i])
      }
    }
  }
  form       <- as.formula(paste0(y, ' ~.'))
  regressor  <- partykit::cforest(formula = form, data = data)
  importance <- partykit::varimp(regressor, nperm = nperm)
  importance <- names(sort(abs(importance), decreasing = TRUE))[1:max_features]
  # We select those features seen as important for Variable Importance algorithm.
  features           <- 1:ncol(data)
  selected_fs        <- names(data) %in% importance
  y_idx              <- which(names(data) == y)
  selected_fs[y_idx] <- TRUE
  selected_idx       <- features[selected_fs]
  data               <- data_org[, selected_fs]
  # Save features removed by FS algorithm.
  to_rm <- abs(as.numeric(selected_fs) - 1)
  to_rm <- features[as.logical(to_rm)]

  return(list(
    data = data,
    idx  = to_rm
  ))
}


#' Feature Selection performed by MCFS (Monte Carlo Feature Selection) algorithm
#'
#' Unfortunately it is impossible to turn off the prints, as they come from used
#' rmcfs package's back-end written in Java.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param cutoffPermutations An non-negative integer value that determines the number of permutation
#' runs. It needs at least 20 permutations for a statistically significant result.
#' Minimum value of this parameter is 3, however if it is 0 then permutations
#' method is turned off.
#' @param threadsNumber A positive integer value describing the number of threads to use
#' in computation. More threads needs more CPU cores as well as memory usage is
#' a bit higher. It is recommended to set this value equal to or less than CPU
#' available cores. By default set to NULL, which will use maximal number of cores
#' minus 1.
#' @param max_features A positive integer value describing the desired number of
#' selected features. By default set to NULL, then the MCFS algorithm uses all
#' features that seems important to it.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with selected columns,
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
select_mcfs <- function(data, y, cutoffPermutations = 20, threadsNumber = NULL, max_features = NULL) {
  if (is.null(threadsNumber)) {
    threadsNumber <- parallel::detectCores() - 1
  } else {
    if (as.integer(threadsNumber) != threadsNumber || threadsNumber < 1) {
      verbose_cat(crayon::red('\u2716'), 'MCFS feature selection: Number of threads must be a positive integer.', '\n', verbose = TRUE)
      stop('MCFS feature selection: Number of threads must be a positive integer.')
    }
  }
  if (threadsNumber > parallel::detectCores()) {
    verbose_cat(crayon::red('\u2716'), 'MCFS feature selection: Number of cores cannot be larger than available cores.', '\n', verbose = TRUE)
    stop('MCFS feature selection: Number of cores cannot be larger than available cores.')
  }
  if (cutoffPermutations < 0 || cutoffPermutations %in% c(1,2) ||
      as.integer(cutoffPermutations) != cutoffPermutations) {
    verbose_cat(crayon::red('\u2716'), 'MCFS feature selection: Number of cutoffPermutations must be equal 0, or greater-equal than 3, and be an integer.', '\n', verbose = TRUE)
    stop('MCFS feature selection: Number of cutoffPermutations must be equal 0, or greater-equal than 3, and be an integer.')
  }
  if (!is.null(max_features)) {
    if (max_features < 1 || max_features >= ncol(data) || as.integer(max_features) != max_features) {
      verbose_cat(crayon::red('\u2716'), 'MCFS feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).', '\n', verbose = TRUE)
      stop('MCFS feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).')
    }
  }

  form <- as.formula(paste0(y, ' ~.'))
  # We cannot suppress prints as they come from Java back-end.
  mcfs <- rmcfs::mcfs(form, data, threadsNumber = threadsNumber, cutoffPermutations = cutoffPermutations)
  # We select those features seen as important for MCFS algorithm.
  if (!is.null(max_features)) {
    max_fs <- min(mcfs$cutoff_value, max_features)
  } else {
    max_fs <- mcfs$cutoff_value
  }
  features           <- 1:ncol(data)
  selected_fs        <- names(data) %in% mcfs$RI[1:max_fs, 2]
  y_idx              <- which(names(data) == y)
  selected_fs[y_idx] <- TRUE
  selected_idx       <- features[selected_fs]
  data               <- data[, selected_fs]
  # Save features removed by FS algorithm.
  to_rm <- abs(as.numeric(selected_fs) - 1)
  to_rm <- features[as.logical(to_rm)]

  return(list(
    data = data,
    idx  = to_rm
  ))
}

#' Feature selection performed by varrank package
#'
#' It heuristically estimates the variables ranks based on mutual information
#' with multiple model and search schemes.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param max_features A positive integer value describing the desired number of
#' selected features. By default set to 10.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with selected columns,
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
select_mi_varrank <- function(data, y, max_features = 10) {
  if (max_features < 1 || max_features >= ncol(data) || as.integer(max_features) != max_features) {
    verbose_cat(crayon::red('\u2716'), 'MI varrank feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).', '\n', verbose = TRUE)
    stop('MI varrank feature selection: Number of max_features must be an integer greater-equal than 1, and smaller than ncol(data).')
  }
  varrank <- varrank::varrank(data, y, method = 'estevez', algorithm = 'forward', scheme = 'mid',
                              n.var = max_features, discretization.method = 'cencov')
  # We select those features seen as important for varrank algorithm.
  features           <- 1:ncol(data)
  selected_fs        <- names(data) %in% varrank$ordered.var
  y_idx              <- which(names(data) == y)
  selected_fs[y_idx] <- TRUE
  selected_idx       <- features[selected_fs]
  data               <- data[, selected_fs]
  # Save features removed by FS algorithm.
  to_rm <- abs(as.numeric(selected_fs) - 1)
  to_rm <- features[as.logical(to_rm)]

  return(list(
    data = data,
    idx  = to_rm
  ))
}

#' Perform Boruta algorithm for selecting most important features
#'
#' @param data A data source before preprocessing, that is one of the major R formats:
#' data.table, data.frame, matrix, and so on.
#' @param y A string that indicates a target column name.
#'
#' @return A list containing two objects:
#' \itemize{
#' \item \code{`data`} A dataset with selected columns,
#' \item \code{`idx`} The indexes of removed columns.
#' }
#' @export
select_boruta <- function(data, y) {
  boruta.bank_train            <- eval(parse(text = paste0('Boruta::Boruta(', y, '~.,', ' data', ')')))
  suppressWarnings(boruta.bank <- Boruta::TentativeRoughFix(boruta.bank_train))
  selection                    <- boruta.bank$finalDecision
  selection                    <- names(selection[selection == 'Confirmed'])
  selection                    <- c(selection, y)
  # We select those features seen as important for varrank algorithm.
  features     <- 1:ncol(data)
  selected_fs  <- names(data) %in% selection
  selected_idx <- features[selected_fs]
  data         <- data[, selection]
  # Save features removed by FS algorithm.
  to_rm <- abs(as.numeric(selected_fs) - 1)
  to_rm <- features[as.logical(to_rm)]

  return(list(
    data = data,
    idx  = to_rm
  ))
}
