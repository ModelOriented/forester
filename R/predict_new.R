#' Perform predictions on new data
#'
#' This function is used when the user created the models and evaluated them
#' on a dataset x and later on, he got new observations and wants to predict the
#' target value for them. The `predict_new()` function quickly normalizes their format, like a
#' subset of columns that were deleted for the models and makes predictions.
#' It is strongly advised that the new dataset shouldn't have any missing values.
#'
#' @param train_out The output of the `train()` function.
#' @param data A dataset with the same format as the one used in `train()` function,
#' but with new observations.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return A list of predictions.
#' @export
#'
#' @examples
#' data(lisbon)
#' lisbon_train <- lisbon[1:200, ]
#' lisbon_new   <- lisbon[201:246, ]
#' out          <- train(lisbon_train, 'Price', verbose = FALSE)
#' preds        <- predict_new(out, lisbon_new)
#' preds
predict_new <- function(train_out, data, verbose = TRUE) {
  del_cols   <- train_out$deleted_columns
  y          <- train_out$y
  bin_labels <- train_out$bin_label
  engine     <- train_out$engine
  train_data <- train_out$raw_train$ranger_data
  model      <- train_out$models_list
  type       <- train_out$type

  # preprocessing simulation
  data <- data[, !(names(data) %in% del_cols)]
  # labelling
  if (type == 'binary_clf') {
    levels(data[[y]]) <- c(1, 2)
  }

  if (nrow(data) > 29) {
    tryCatch(
      data <- manage_missing(data, y),
      error = function(e)
        verbose_cat(
          'Too few observations to perform imputation. MICE algorithm requires
          at least 30 observations. \n',
          verbose = verbose
        )
    )
  } else if (nrow(data) > 0) {
    verbose_cat(
      'No imputation performed due to only one observation. If any values are
      missing, user has to handle them by himself. \n',
      verbose = verbose
    )
  }
  data         <- prepare_data(data, y, engine, TRUE, train_data)
  preds        <- predict_models_all(model, data, y, type)
  names(preds) <- names(train_out$models_list)

  return(preds)
}
