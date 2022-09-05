draw_confusion_matrix <- function(best_models, test_data, observed) {
  preds <- best_model_predict(best_models, test_data)
  preds <- ifelse(preds > 0.5, 1, 0) # unequivocal classification

  # Confusion matrix
  d_binomial <- tibble::tibble("target" = observed,
                       "prediction"     = preds)
  eval <- cvms::evaluate(d_binomial,
                   target_col           = "target",
                   prediction_cols      = "prediction",
                   type                 = "binomial")
  cvms::plot_confusion_matrix(eval$`Confusion Matrix`[[1]],
                   add_normalized       = FALSE,
                   add_row_percentages  = FALSE,
                   add_col_percentages  = FALSE,
                   rm_zero_percentages  = FALSE,
                   palette              = 'Greens',
                   tile_border_color    = '#456243')
}


# Draw ROC curve for the best model.
# draw_roc_plot(models$lightgbm_model, test_data, split_data$test$Species-1)
draw_roc_plot <- function(best_models, test_data, observed) {
  preds <- best_model_predict(best_models, test_data)
  # ROC curve
  return(verification::roc.plot(observed, preds))
}


#' Predict
#'
#' @param best_models List of models.
#' @param test_data Test data.
#'
#' @return Vecto of predicitons between 0 and 1 values.
#' @export
#'
#' @examples
best_model_predict <- function(best_models, test_data) {
  preds <- NULL

  if (names(best_models[1]) == 'ranger_model') {
    preds <- predict(best_models$ranger_model, test_data$ranger_data)$predictions - 1

  } else if (names(best_models[1]) == 'xgboost_model') {
    preds <- predict(best_models$xgboost_model, test_data$xgboost_data) - 1

  } else if (names(best_models[1]) == 'decision_tree_model') {
    preds <- as.numeric(predict(best_models$decision_tree_model, test_data$decision_tree_data)) - 1

  } else if (names(best_models[1]) == 'lightgbm_model') {
    preds <- predict(best_models$lightgbm_model, test_data$lightgbm_data)

  } else if (names(best_models[1]) == 'catboost_model') {
    preds <- catboost::catboost.predict(best_models$catboost_model, test_data$catboost_data,
                                        prediction_type = 'Probability')

  }
  return(preds)
}


#' Radar chart of one metric
#'
#' @param score_frame The result of `score_model` function.
#' @param type A string which determines if machine learning task is the
#' `classification` or `regression`.
#'
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin <- iris[1:100,]
#' iris_bin$Species <- factor(iris_bin$Species)
#' type <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species')
#' split_data <-
#'   train_test_balance(preprocessed_data$data,
#'                      'Species',
#'                      type = type,
#'                      balance = FALSE)
#' train_data <-
#'   prepare_data(split_data$train,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
#'                'catboost'))
#' test_data <-
#'   prepare_data(split_data$test,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
#'                'catboost'),
#'                predict = TRUE,
#'                train = split_data$train)
#'
#' model <-
#'   train_models(train_data,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
#'                'catboost'),
#'                type = type)
#' predictions <-
#'   predict_models(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
#'                  'catboost'),
#'                  type = type)
#' score <-
#'   score_models(model,
#'                predictions,
#'                observed = split_data$test$Species,
#'                type = type)
#' draw_radar_plot(score, 'binary_clf')
draw_radar_plot <- function(score_frame, type) {
  data <- NULL

  if (type == 'binary_clf') {
    # Metric between 0 and 1
    f1_row        <- NULL
    recall_row    <- NULL
    precision_row <- NULL
    accuracy_row  <- NULL
    auc_row       <- NULL

    if ('f1' %in% names(score_frame)) {
      f1_row <- as.data.frame(t(as.numeric(unlist(score_frame['f1']))))
    }
    if ('recall' %in% names(score_frame)) {
      recall_row <- as.data.frame(t(as.numeric(unlist(score_frame['recall']))))
    }
    if ('precision' %in% names(score_frame)) {
      precision_row <- as.data.frame(t(as.numeric(unlist(score_frame['precision']))))
    }
    if ('accuracy' %in% names(score_frame)) {
      accuracy_row <- as.data.frame(t(as.numeric(unlist(score_frame['accuracy']))))
    }
    if ('auc' %in% names(score_frame)) {
      auc_row <- as.data.frame(t(as.numeric(unlist(score_frame['auc']))))
    }

    data <- rbind(f1_row, recall_row, precision_row, accuracy_row, auc_row)


  } else {
    r2_row <- NULL
    if ('r2' %in% names(score_frame)) {
      r2_row <- as.data.frame(t(as.numeric(unlist(score_frame['r2']))))
    }
    data <- r2_row
  }

  colnames(data) <- score_frame$engine
  data           <- rbind(rep(1, 5), rep(0, 5), data) # First row: minimal value of metric,
  # Second row: maximal value of metric

  op <- graphics::par(mar = c(3, 3, 3, 3)) # Remove margins
  fmsb::radarchart(data,
                   title       = 'Score metrics',
                   pcol        = rev(forester_palette()),
                   plty        = 1,
                   plwd        = 3, # line width
                   cglcol      = "black",
                   axislabcol  = "black",
                   seg         = 2,
                   axistype    = 4,
                   vlcex       = 0.7)
  if (type == 'binary_clf') {
    graphics::legend(
                   x           = 'topright',
                   legend      = names(score_frame[-c(1)]),
                   bty         = "n",
                   pch         = 20 ,
                   col         = rev(forester_palette()),
                   text.col    = "black",
                   cex         = 1,
                   pt.cex      = 1.5)
  } else {
    graphics::legend(
                   x           = 'topright',
                   legend      = 'r2',
                   bty         = "n",
                   pch         = 20,
                   col         = rev(forester_palette()),
                   text.col    = "black",
                   cex         = 1,
                   pt.cex      = 1.5)
  }
}


#' Draw boxplot of resuduals - for regression
#'
#' @param observed A vector of true values of target.
#' @param predictions A list of predictions for each trained model.
#'
#' @export
#'
#' @examples
draw_boxplot <- function(observed, predictions) {
  residuals <- NULL

  if ('ranger_preds' %in% names(predictions)) {
    residuals$ranger <- observed - predictions$ranger_preds
  }
  if ('xgboost_preds' %in% names(predictions)) {
    residuals$xgboost <- observed - predictions$xgboost_preds
  }
  if ('decision_tree_preds' %in% names(predictions)) {
    residuals$decision <- observed - predictions$decision_tree_preds
  }
  if ('lightgbm_preds' %in% names(predictions)) {
    residuals$lightgbm <- observed - predictions$lightgbm_preds
  }
  if ('catboost_preds' %in% names(predictions)) {
    residuals$catboost <- observed - predictions$catboost_preds
  }

  graphics::boxplot(residuals,
          horizontal = TRUE,
          col        = forester_palette(),
          names      = names(residuals),
          las        = 2,
          main       = 'Residuals',
          xlab       = 'residual value'
  )
}


#' Draw scatterplot of true vs predicted values of target for training and test data for one model
#'
#' @param train_observed A vector of true values for training data.
#' @param train_predictions A vector of predicted values for training data.
#' @param test_observed A vector of true values for test data.
#' @param test_predictions A vector of predicted values for test data.
#'
#' @export
#'
#' @examples
#' type <- guess_type(lisbon, 'Price')
#' preprocessed_data <- preprocessing(lisbon, 'Price')
#' split_data <-
#'   train_test_balance(preprocessed_data$data,
#'                      y = 'Price',
#'                      type = type,
#'                      balance = FALSE)
#' train_data <- prepare_data(split_data$train,
#'                      y = 'Price',
#'                      engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
#' )
#' test_data <-
#'   prepare_data(split_data$test,
#'                'Price',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                predict = TRUE,
#'                train = split_data$train)
#'
#'
#' models <-  train_models(train_data,
#'                        'Price',
#'                        engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                        type = type)
#'predictions <-
#' predict_models(models,
#'                test_data,
#'                'Price',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                type = type)
#' draw_scatterplot(split_data$train$Price, models$ranger_model$predictions,
#' split_data$test$Price, predictions$ranger_preds)
draw_scatterplot <- function(train_observed, train_predictions, test_observed, test_predictions) {
  graphics::plot(c(0, max(unlist(train_predictions), unlist(train_observed))),
                 c(0, max(unlist(train_predictions), unlist(train_observed))),
                 xlab = 'true value', ylab = 'predicted value', col = 'black', type = 'l',
                 main = 'True and predicted values with y = x curve\n for TRAINING data')
  graphics::points(train_observed, train_predictions, col = forester_palette('dark green'), pch = 19)

  graphics::plot(c(0, max(unlist(test_predictions), unlist(test_observed))),
                 c(0, max(unlist(test_predictions), unlist(test_observed))),
                 xlab = 'true value', ylab = 'predicted value', col = 'black', type = 'l',
                 main = 'True and predicted values with y = x curve\n for TEST data')
  graphics::points(test_observed, test_predictions, col = forester_palette('dark green'), pch = 19)

}


#' Draws train vs test RMSE plot for models
#'
#' @param train_observed A vector of true values for training dataset.
#' @param train_predictions A list of lists of predictions on training dataset for models but just for c('ranger', 'xgboost', 'decision_tree')
#' @param test_observed A vector of true values for test dataset.
#' @param test_predictions A list of lists of predictions on test dataset for models but just for c('ranger', 'xgboost', 'decision_tree')
#'
#' @export
#'
#' @examples
draw_rmse_plot <- function(train_observed, train_predictions, test_observed, test_predictions) {
  train_ranger_rmse        <- model_performance_rmse(train_predictions$ranger_preds, train_observed)
  test_ranger_rmse         <- model_performance_rmse(test_predictions$ranger_preds, test_observed)

  train_xgboost_rmse       <- model_performance_rmse(train_predictions$xgboost_preds, train_observed)
  test_xgboost_rmse        <- model_performance_rmse(test_predictions$xgboost_preds, test_observed)

  train_decision_tree_rmse <- model_performance_rmse(train_predictions$decision_tree_preds, train_observed)
  test_decision_tree_rmse  <- model_performance_rmse(test_predictions$decision_tree_preds, test_observed)

  train_rmse               <- c(train_ranger_rmse, train_xgboost_rmse, train_decision_tree_rmse)
  test_rmse                <- c(test_ranger_rmse, train_xgboost_rmse, test_decision_tree_rmse)


  graphics::plot(c(0, max(unlist(train_rmse), unlist(test_rmse))),
                 c(0, max(unlist(train_rmse), unlist(test_rmse))),
                 xlab = 'RMSE test', ylab = 'RMSE train', col = 'black', type = 'l',
                 main = 'RMSE with y = x curve')
  graphics::points(test_rmse, train_rmse,
                   col = rev(forester_palette()), pch = 19)
  graphics::legend(x = 'topleft',
                   legend = c('ranger', 'xgboost', 'decision_tree'),
                   bty = "n", pch = 20 , col = rev(forester_palette()),
                   text.col = "black", cex = 1, pt.cex = 1.5)

}


draw_feature_importance <- function(best_models, test_data, y, engine) {
  explainer <- explain(best_models, test_data, y, engine)
  graphics::plot(DALEX::model_parts(explainer = explainer$ranger_explainer))
}
