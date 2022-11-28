#' Draw confusion matrix for the model
#'
#' @param best_models A list of models.
#' @param test_data A test dataset.
#' @param observed A vector of observed target values.
#'
#' @return A confusion matrix ggplot object.
#' @export

draw_confusion_matrix <- function(best_models, test_data, observed) {
  preds <- best_model_predict(best_models, test_data)
  preds <- ifelse(preds > 0.5, 1, 0) # unequivocal classification

  # Confusion matrix
  if (!is.null(ncol(preds))) {
    d_binomial <- tibble::tibble('target'     = observed,
                                 'prediction' = preds[, 2])
  } else {
    d_binomial <- tibble::tibble('target'     = observed,
                                 'prediction' = preds)
  }

  Y <- c(0, 0, 0, 0)
  for (i in 1:nrow(d_binomial)) {
    if (d_binomial$target[i] == 0 && d_binomial$prediction[i] == 0) {
      Y[4] <- Y[4] + 1
    } else if (d_binomial$target[i] == 0 && d_binomial$prediction[i] == 1) {
      Y[2] <- Y[2] + 1
    } else if (d_binomial$target[i] == 1 && d_binomial$prediction[i] == 1) {
      Y[1] <- Y[1] + 1
    } else if (d_binomial$target[i] == 1 && d_binomial$prediction[i] == 0) {
      Y[3] <- Y[3] + 1
    }
  }

  Target     <- factor(c(0, 0, 1, 1))
  Prediction <- factor(c(0, 1, 0, 1))
  df         <- data.frame(Target, Prediction, Y)

  ggplot2::ggplot(data =  df, mapping = ggplot2::aes(x = Target, y = Prediction)) +
    ggplot2::geom_tile(ggplot2::aes(fill = Y), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%1.0f", Y)), vjust = 1, colour = forester_palette()[[5]], size = 5) +
    ggplot2::scale_fill_gradient(low = forester_palette()[[2]], high = forester_palette()[[3]]) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle('Confusion Matrix') +
    ggplot2::theme(legend.position = 'none',
                   plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 20),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 16),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10))
}


#' Draw AUC ROC curve for the best model
#'
#' @param best_models A list of models.
#' @param test_data A test dataset.
#' @param observed A vector of observed target values.
#'
#' @return An AUC ROC ggplot object.
#' @export
draw_roc_plot <- function(best_models, test_data, observed) {
  preds <- best_model_predict(best_models, test_data)
  # ROC curve
  # preds <- ifelse(preds > 0.5, 1, 0) Tutaj binazryzacja jest, czy nie jest potrzebna?
  # bez niej otrzymujemy ładniejszy wykres ROC i wyższe AUC
  if (!is.null(ncol(preds))) {
    preds <- preds[, 2]
  }

  rocobj <- pROC::roc(observed, preds)
  auc    <- round(pROC::auc(observed, preds), 4)

  return(pROC::ggroc(rocobj, colour = forester_palette()[[3]], size = 2) +
           ggplot2::ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
           ggplot2::theme_minimal() +
           ggplot2::theme(legend.position = 'none',
                          plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 20),
                          axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 16),
                          axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10)))
}


#' Predict a single model
#'
#' @param best_models A list of models.
#' @param test_data A test dataset.
#'
#' @return A vector of predicitons between 0 and 1 values.
#' @export
best_model_predict <- function(best_models, test_data) {
  preds <- NULL

  if (grepl('ranger', names(best_models[1]))) {
    preds <- eval(parse(text = paste0(
      'predict(best_models$', names(best_models[1]), ', test_data$ranger_data)$predictions'
    )))

  } else if (grepl('xgboost', names(best_models[1]))) {
    preds <- eval(parse(text = paste0(
      'predict(best_models$', names(best_models[1]), ', test_data$xgboost_data)'
    )))

  } else if (grepl('decision_tree', names(best_models[1]))) {
    preds <- eval(parse(text = paste0(
      'as.numeric(predict(best_models$', names(best_models[1]), ', test_data$decision_tree_data))'
    )))

  } else if (grepl('lightgbm', names(best_models[1]))) {
    preds <- eval(parse(text = paste0(
      'predict(best_models$', names(best_models[1]), ', test_data$lightgbm_data)'
    )))

  } else if (grepl('catboost', names(best_models[1]))) {
    preds <- eval(parse(text = paste0(
      'catboost::catboost.predict(best_models$', names(best_models[1]), ', test_data$catboost_data, prediction_type = "Probability")'
    )))

  }
  return(preds)
}


#' Plot radar chart of one metric
#'
#' @param score_frame The result of `score_model` function.
#' @param type A string which determines if machine learning task is the
#' `classification` or `regression`.
#'
#' @export
#'
#' @examples
#' train_out <- train(lisbon, 'Price', verbose = FALSE, best_model_number = 10)
#' draw_radar_plot(train_out$best_models[1:10, ], train_out$type)
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

    data  <- rbind(f1_row, recall_row, precision_row, accuracy_row, auc_row)
    data  <- data.frame(names(score_frame[-c(1)]), data)
    title <- 'Model comparison'

  } else {
    r2_row <- NULL
    if ('r2' %in% names(score_frame)) {
      r2_row <- as.data.frame(t(as.numeric(unlist(score_frame['r2']))))
    }
    data  <- r2_row
    title <- 'R2 comparison'
  }

  colnames(data) <- score_frame$engine

  ggradar::ggradar(plot.data                = data,
                   base.size                = 10,
                   values.radar             = c(0, 0.5, 1),
                   grid.line.width          = 0.6,
                   gridline.min.linetype    = 2,
                   gridline.mid.linetype    = 2,
                   gridline.max.linetype    = 2,
                   gridline.min.colour      = forester_palette()[[5]],
                   gridline.mid.colour      = forester_palette()[[5]],
                   gridline.max.colour      = forester_palette()[[5]],
                   grid.label.size          = 5,
                   axis.line.colour         = forester_palette()[[5]],
                   axis.label.size          = 2.5,
                   axis.label.offset        = 1.15,
                   group.line.width         = 1.5,
                   group.point.size         = 4,
                   group.colours            = unname(forester_palette()),
                   legend.title             = 'Metric',
                   plot.title               = title,
                   legend.text.size         = 10,
                   legend.position          = 'right',
                   background.circle.colour = 'white') +
    ggplot2::theme(plot.title   = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 20),
                   legend.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 16),
                   legend.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 16),
                   axis.title   = ggplot2::element_text(colour = forester_palette()[[5]], size = 10)) +
    ggplot2::coord_fixed()
}


#' Draw boxplot of resuduals - for regression
#'
#' @param observed A vector of the true values of target.
#' @param predictions A list of predictions for each trained model.
#' @param models A list of models.
#'
#' @export
draw_boxplot <- function(observed, predictions, models) {
  residuals_table <- c()

  for (i in 1:length(models)) {
    residuals       <- list(observed - predictions[[i]])
    residuals_table <- c(residuals_table, residuals)
  }

  Residuals <- NULL
  Model     <- NULL
  Nr        <- NULL

  df <- data.frame(Residuals = unlist(residuals_table), Model = names(models), Nr = length(models):1)

  ggplot2::ggplot(df, ggplot2::aes(y = stats::reorder(Model, Nr), x = Residuals)) +
    ggplot2::geom_boxplot(fill = forester_palette()[[3]]) +
    ggplot2::theme_minimal() +
    ggplot2::ylab('Models') +
    ggplot2::scale_fill_manual(values = unname(forester_palette())) +
    ggplot2::ggtitle('Combined Models Residuals Plot') +
    ggplot2::theme(legend.position = 'none',
                   plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 20),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 16),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10))
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
#' type              <- guess_type(lisbon, 'Price')
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
  Observed   <- NULL
  Prediction <- NULL
  df_train   <- data.frame(Observed = train_observed, Prediction = train_predictions)
  df_test    <- data.frame(Observed = test_observed, Prediction = test_predictions)

  plt1 <- ggplot2::ggplot(df_train, ggplot2::aes(x = Observed, y = Prediction)) +
    ggplot2::geom_point(colour = forester_palette()[[1]]) +
    ggplot2::ggtitle('Observed vs Predicted values for \nbest model train subset') +
    ggplot2::xlim(0,max(unlist(train_predictions), unlist(train_observed))) +
    ggplot2::ylim(0,max(unlist(train_predictions), unlist(train_observed))) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = forester_palette()[[5]], size = 0.5, linetype = 'dashed') +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = 'none',
                   plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 16, hjust = 0.5),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 14),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10))

  plt2 <- ggplot2::ggplot(df_test, ggplot2::aes(x = Observed, y = Prediction)) +
    ggplot2::geom_point(colour = forester_palette()[[1]]) +
    ggplot2::ggtitle('Observed vs Predicted values for \nbest model test subset') +
    ggplot2::xlim(0,max(unlist(test_predictions), unlist(test_observed))) +
    ggplot2::ylim(0,max(unlist(test_predictions), unlist(test_observed))) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = forester_palette()[[5]], size = 0.5, linetype = 'dashed') +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = 'none',
                   plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 16, hjust = 0.5),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 14),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10))

  return(list(
    plt1 = plt1,
    plt2 = plt2
  ))
}


#' Draws train vs test RMSE plot for models
#'
#' @param train_observed A vector of the true values for training dataset.
#' @param train_predictions A list of lists of predictions on the training dataset for models but just for c('ranger', 'xgboost', 'decision_tree')
#' @param test_observed A vector of the true values for test dataset.
#' @param test_predictions A list of lists of predictions on test dataset for models but just for c('ranger', 'xgboost', 'decision_tree')
#' @param models A list of models.
#'
#' @export
draw_rmse_plot <- function(train_observed, train_predictions, test_observed, test_predictions, models) {

  train_rmse  <- c()
  test_rmse   <- c()
  engine      <- c()
  model_names <- names(models)

  for (i in 1:length(train_predictions)) {

    rmse1      <- model_performance_rmse(train_observed, as.numeric(unlist(train_predictions[i])))
    rmse2      <- model_performance_rmse(test_observed, as.numeric(unlist(test_predictions[i])))
    train_rmse <- c(train_rmse, rmse1)
    test_rmse  <- c(test_rmse, rmse2)
    m_class    <- class(models[[i]])

    if ('ranger' %in% m_class) {
      eng <- 'ranger'
    } else if ('xgb.Booster' %in% m_class) {
      eng <- 'xgboost'
    } else if ('constparty' %in% m_class) {
      eng <- 'decision_tree'
    } else if ('lgb.Booster' %in% m_class) {
      eng <- 'lightgbm'
    } else if ('catboost.Model' %in% m_class) {
      eng <- 'catboost'
    }
    engine <- c(engine, eng)
  }

  Train_rmse <- NULL
  Test_rmse  <- NULL
  Engine     <- NULL
  df         <- data.frame(Train_rmse = train_rmse, Test_rmse = test_rmse,
                           Engine = engine, Model_name = model_names)

  ggplot2::ggplot(df, ggplot2::aes(x = Train_rmse, y = Test_rmse, colour = Engine)) +
    ggplot2::geom_point(size = 4) +
    ggrepel::geom_text_repel(label = model_names,
                             size = 3,
                             max.overlaps = Inf,
                             box.padding = 0.7,
                             seed = 42,
                             verbose = FALSE) +
    ggplot2::scale_color_manual(values = unname(forester_palette())) +
    ggplot2::ggtitle('RMSE Train vs Test plot') +
    ggplot2::xlim(min(unlist(train_rmse), unlist(test_rmse)),max(unlist(train_rmse), unlist(test_rmse))) +
    ggplot2::ylim(min(unlist(train_rmse), unlist(test_rmse)),max(unlist(train_rmse), unlist(test_rmse))) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = forester_palette()[[5]], size = 0.5, linetype = 'dashed') +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 20),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 14),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10),
                   legend.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 16),
                   legend.text = ggplot2::element_text(colour = forester_palette()[[1]], size = 10))

}
#' Draw Feature Importance plot
#'
#' @param best_models A list of models.
#' @param test_data A test dataset.
#' @param y A target variable. It can be either
#' (1) a vector of the same number of observations as `data` or
#' (2) a character name of variable in the `data` that contains
#' the target variable.
#'
#' @return A ggplot object.
#' @export
draw_feature_importance <- function(best_models, test_data, y) {
  explainer <- explain(best_models[[1]], test_data, y)

  plt <- graphics::plot(DALEX::model_parts(explainer = explainer), max_vars = 8, show_boxplots = TRUE)
  plt <- plt +
    ggplot2::scale_color_manual(values = forester_palette()[[3]]) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 20),
                   line = ggplot2::element_line(color = forester_palette()[[2]]),
                   plot.subtitle = ggplot2::element_text(colour = forester_palette()[[1]], size = 12),
                   strip.text = ggplot2::element_text(colour = forester_palette()[[1]], size = 12, hjust = 0),
                   axis.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[1]], size = 14),
                   axis.text  = ggplot2::element_text(colour = forester_palette()[[5]], size = 10),
                   legend.title = ggplot2::element_text(face = 'bold', colour = forester_palette()[[5]], size = 16),
                   legend.text = ggplot2::element_text(colour = forester_palette()[[1]], size = 10))

  plt$layers[[3]]$aes_params$fill <- forester_palette()[[1]]
  plt$layers[[3]]$aes_params$colour <- forester_palette()[[1]]
  return(plt)
}
