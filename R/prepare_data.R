#' Prepare data into format correct for the selected model engine
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used. By default NULL.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`, `lightgbm`, `catboost`, `decision_tree`.
#' Determines which models will be later learnt. It doesn't matter for survival analysis.
#' @param predict A logical value, determines whether the data set will be used
#' for prediction or training. It is necessary, because lightgbm model can't predict
#' on training dataset.
#' @param train A train data, if predict is TRUE you have to provide training
#' dataset from split data here.
#'
#' @return A dataset in format proper for the selected engines.
#' @export
prepare_data <- function(data,
                         y       = NULL,
                         time    = NULL,
                         status  = NULL,
                         engine  = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                         predict = FALSE,
                         train   = NULL) {
  ranger_data        <- NULL
  xgboost_data       <- NULL
  decision_tree_data <- NULL
  lightgbm_data      <- NULL
  catboost_data      <- NULL

  # Distinction between survival analysis and other tasks.
  if (!is.null(y)) {
    target <- c(y)
  } else {
    target <- c(time, status)
  }

  data <- as.data.frame(unclass(data), stringsAsFactors = TRUE) # Important part
  # is conversion strings to factors because we can work on them and
  # add category `other` for predictions.

  if (!predict) {
    # If it is a training dataset we add the level other and in first
    # observation of the dataset we change all factor values to others so that
    # we will have other values in the training dataset and the model can recognize that.
    for (i in 1:ncol(data)) {
      if ('factor' %in% class(data[, i]) && !names(data[i]) %in% target) {
        levels(data[, i]) <- c(levels(data[, i]), 'other')
        data[1, i]        <- 'other'
        data[, i]         <- droplevels(data[, i])
      }
    }

  } else {
    # If it is a test or validation dataset we perform otherisation of levels for the train again.
    train <- as.data.frame(unclass(train), stringsAsFactors = TRUE)
    for (i in 1:ncol(train)) {
      if ('factor' %in% class(train[, i]) && !names(train[i]) %in% target) {
        levels(train[, i]) <- c(levels(train[, i]), 'other')
        train[1, i]        <- 'other'
        train[, i]         <- droplevels(train[, i])
      }
    }
    # Then we change all factors unseen in the train to category other.
    for (i in 1:ncol(data)) {
      if ('factor' %in% class(data[, i]) && !names(data[i]) %in% target) {
        levels(data[, i]) <- c(levels(data[, i]), 'other')
        for (j in 1:nrow(data)) {
          if (!(data[j, i] %in% levels(train[, i]))) {
            data[j, i] <- 'other'
          }
        }
        data[, i] <- droplevels(data[, i])
      }
    }
  }

  # Ranger data is always needed as *almost* the unprocessed one. Its also enough
  # for survival analysis.
  ranger_data <- data.frame(data)
  if (is.null(y)){
    return(list(ranger_data = ranger_data))
  }
  # We have to drop levels that were changed to other and insert all levels
  # from the train (for OHE in the xgboost model).
  if ('xgboost' %in% engine) {
    # The xgboost model works on numerical data only, so we have to perform OHE.
    for (i in 1:ncol(data)) {
      if ('factor' %in% class(data[, i])) {
        levels(data[, i]) <- c(levels(data[, i]), levels(train[, i]))
      }
    }

    ohe_feats     <-  c()
    xgboost_data  <- data
    for (i in 1:ncol(data)) {
      if ('factor' %in% class(data[, i])) {
        condition <- !is.numeric(varhandle::unfactor(data[, i]))
      } else {
        condition <- !is.numeric(data[, i])
      }
      if (condition) {
        # We need to unfactor the data.
        if (colnames(data)[i] == y) {
          # We use the integer encoding if the column is the target.
          xgboost_data[y] <- as.numeric(unlist(data[y]))
        } else {
          # The OHE for the rest of variables.
          ohe_feats <- c(ohe_feats, colnames(data)[i])
        }
      }
    }

    label          <- xgboost_data[, y]
    xgboost_table  <- data.table::as.data.table(data[, -which(names(xgboost_data) == y)])
    xgboost_data   <- mltools::one_hot(xgboost_table)
    xgboost_data   <- apply(xgboost_data, 2, as.numeric)
    xgboost_data   <- as.data.frame(xgboost_data) # couldn't figure out how to do
    # the next step on data.table
    xgboost_data   <- xgboost_data[, order(names(xgboost_data))] # For proper
    # work. Without this, the OHE cols in the train and test are not in the same order
    # and the xgboost model can't work properly.
    xgboost_data   <- as.matrix(xgboost_data)
    if (nrow(data) == 1) {
      xgboost_data <- t(xgboost_data)
    }
    #xgboost_data  <- xgboost::xgb.DMatrix(data = as.matrix(xgboost_data), label = label)
  }
  if ('decision_tree' %in% engine) {
    decision_tree_data <- data
    for (i in 1:ncol(decision_tree_data)) {
      if (is.character(decision_tree_data[, i])) {
        # If we get more than 31 levels per column, the decision tree cannot be
        # build, thus we encode it numerically in this case.
        if (length(levels(factor(decision_tree_data[, i]))) >= 30) {
          decision_tree_data[, i] <- as.integer(factor(decision_tree_data[, i]))
        } else {
          decision_tree_data[, i] <- factor(decision_tree_data[, i])
        }
      } else if (is.factor(decision_tree_data[, i]) && length(levels(factor(decision_tree_data[, i]))) >= 30) {
        decision_tree_data[, i]   <- as.integer(factor(decision_tree_data[, i]))
      }
    }
    if (guess_type(data, y) == 'binary_clf') {
      decision_tree_data[[y]] <- as.factor(decision_tree_data[[y]])
    }

    decision_tree_data <- data.frame(decision_tree_data)
  }
  if ('lightgbm' %in% engine) {
    if (predict == FALSE) {
      y_true <- data[, which(names(data) == y)]
      if (guess_type(data, y) != 'regression') {
        label <- as.matrix(as.numeric(y_true) - 1)
      } else {
        label <- as.matrix(y_true)
      }

      X   <- data[, -which(names(data) == y)]
      dat <- as.matrix(X)
      suppressWarnings(lightgbm_data <- lightgbm::lgb.Dataset(data = dat, label = label))
      # -1, because lgb enumarates classees from 0.
    } else { # The lgbm model can't predict on lgb.Dataset.
      X <- data[, -which(names(data) == y)]
      lightgbm_data <- data.matrix(X)
    }
  }
  if ('catboost' %in% engine) {
    y_true <- data[, which(names(data) == y)]

    X <- data[, -which(names(data) == y)]
    X <- data.matrix(X)
    X <- apply(X, 2, as.numeric)
    X <- as.matrix(X, ncol = ncol(data) - 1)
    if (nrow(data) == 1) {
      X <- t(X)
    }
    X <- data.matrix(X)
    catboost_data <- catboost::catboost.load_pool(X, as.numeric(y_true))
  }

  return(
    list(
      ranger_data        = ranger_data,
      xgboost_data       = xgboost_data,
      decision_tree_data = decision_tree_data,
      lightgbm_data      = lightgbm_data,
      catboost_data      = catboost_data
    )
  )
}
