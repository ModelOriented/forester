#' Plot of Metrics for Binary Classification Models
#'
#' Function plot.classification plots measure of quality of regression models.
#'
#' @param x An object, returned from \code{train()} function.
#' @param models A character or numeric that indicates which models
#' will be presented. If `NULL` (the default option) then the three best models
#' will be presented.
#' @param type A character one of `comparison`, `roc`, `confusion-matrix`, `train-test`
#' indicates the type of chart.
#' @param metric A character one of `accuracy`/`auc`/`f1` indicates the metric on the plots.
#' @param ... Other parameters that are necessary for consistency with generic plot function.
#'
#' @return a ggplot2 object
#'
#' @examples
#' \dontrun{
#' library('forester')
#' data('adult')
#'
#' x <- train(adult, 'salary', bayes_iter = 0, random_evals = 0)
#' plot(x)
#' }
#'
#' @import ggplot2
#' @import patchwork
#' @export


plot.binary_clf <- function(x,
                            models = NULL,
                            type   = 'comparison',
                            metric = 'accuracy',
                            ...){

  if (!(c('binary_clf') %in% class(x)))
    stop('The plot() function requires an object created with train() function for binary classification task.')

  if (!any(type %in% c('comparison', 'roc', 'confusion-matrix', 'train-test')))
    stop('The selected plot type does not exist.')

  models_names <- models_num <- NULL

  if (is.null(models)) {
    models_names <- x$score_test$name[1:3]
    models_num   <- 1:3
  } else{
    if (is.character(models)) {
      if (any(models %in% names(x$models_list))) {
        models_names <- models[models %in% names(x$models_list)]
        models_num   <- which(names(x$models_list) %in% models)
        if (length(models_num) < length(models)) {
          message(paste0(
            'Check the given models.',
            ' Models do not exist: ',
            paste(models[!models %in% names(x$models_list)], collapse = ', '),
            '.'
          ))
        }
      } else{
        stop('Models with the given names do not exist.')
      }
    } else{
      if (any(models %in% 1:length(x$models_list))) {
        models_names <- names(x$models_list)[which(1:length(x$models_list) %in% models)]
        models_num   <- which(1:length(x$models_list) %in% models)
        if (length(models_num) < length(models)) {
          message(paste0(
            'Check the given models.',
            ' Models do not exist: ',
            paste(models[which(!models %in% 1:length(x$models_list))], collapse = ', '),
            '.'
          ))
        }
      } else{
        stop('Models with the given numbers do not exist.')
      }
    }
  }
  if (type == 'comparison') {
    test_scores    <- data.frame(t(x$score_test[, (NCOL(x$score_test) - 2):NCOL(x$score_test)]))
    no_cols        <- min(10, ncol(test_scores))
    test_scores    <- test_scores[, 1:no_cols]
    test_y         <- data.frame(metric = rownames(test_scores), value = unlist(test_scores))
    test_data      <- x$score_test[1:no_cols, ]
    test_data$name <- factor(test_data$name, levels = unique(test_data$name))
    test_all       <- cbind(test_data[rep(seq_len(nrow(test_data)), each = 3), ],
                            data.frame(metric = rownames(test_scores), value = unlist(test_scores)))
    test_all       <- test_all[, c('name', 'engine', 'tuning', 'metric', 'value')]

    valid_scores    <- data.frame(t(x$score_valid[, (NCOL(x$score_valid) - 2):NCOL(x$score_valid)]))
    valid_scores    <- valid_scores[, 1:no_cols]
    valid_y         <- data.frame(metric = rownames(valid_scores), value = unlist(valid_scores))
    valid_data      <- x$score_valid[1:no_cols, ]
    valid_data$name <- factor(valid_data$name, levels = unique(valid_data$name))
    valid_all       <- cbind(valid_data[rep(seq_len(nrow(valid_data)), each = 3), ],
                            data.frame(metric = rownames(valid_scores), value = unlist(valid_scores)))
    valid_all       <- valid_all[, c('name', 'engine', 'tuning', 'metric', 'value')]

    comparison_plot <- function(test, all) {
      if (test) {
        all  <- test_all
        text <- 'testing'
      } else {
        all  <- valid_all
        text <- 'validation'
      }
      p <- ggplot(all, aes(
        x     = all$name,
        y     = all$value,
        color = metric,
        group = metric
      )) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = colors_discrete_forester(length(unique(all$name)))) +
        theme_forester() +
        geom_point() +
        labs(
          title    = 'Model comparison',
          subtitle = paste0('on ', text, ' dataset'),
          y        = 'Value of metric',
          x        = 'Model',
          color    = 'Metric'
        ) + theme(axis.text.x = element_text(angle = 25))
      return(p)
    }

    plot_test  <- comparison_plot(TRUE, test_all)
    plot_valid <- comparison_plot(FALSE, valid_all)
    return(patchwork::wrap_plots(list(plot_test, plot_valid), ncol = 1))
  }


  if (type == 'roc') {
    suppressMessages(
      test_rocobj  <- pROC::roc(x$test_observed,
                                x$predictions_test[[x$score_test$name[[1]]]]))
    suppressMessages(
      test_auc     <- round(pROC::auc(x$test_observed,
                                      x$predictions_test[[x$score_test$name[[1]]]]), 4))
    suppressMessages(
      valid_rocobj <- pROC::roc(x$valid_observed,
                                x$predictions_valid[[x$score_valid$name[[1]]]]))
    suppressMessages(
      valid_auc    <- round(pROC::auc(x$valid_observed,
                                      x$predictions_valid[[x$score_valid$name[[1]]]]), 4))

    roc_plot <- function(test) {
      if (test) {
        rocobj  <- test_rocobj
        auc     <- test_auc
        text    <- 'testing'
        name    <- x$score_test$name[[1]]
      } else {
        rocobj  <- valid_rocobj
        auc     <- valid_auc
        text    <- 'validation'
        name    <- x$score_valid$name[[1]]
      }

      p <- pROC::ggroc(rocobj, colour = colors_discrete_forester(1), linewidth = 2) +
        theme_forester() +
        labs(
          title    = paste0('ROC Curve ', '(AUC = ', auc, ')'),
          subtitle = paste0('for the best ', text, ' model: ' , name),
          y        = 'True positive rate (Sensitivity)',
          x        = 'True negative rate (Specificity)'
        )
      return(p)
    }

    plot_test  <- roc_plot(TRUE)
    plot_valid <- roc_plot(FALSE)
    return(patchwork::wrap_plots(plot_test, plot_valid))
  }


  if(type == 'confusion-matrix') {
    test_preds     <- sapply(x$predictions_test[[x$score_test$name[[1]]]], FUN = round)
    test_observed  <- x$test_observed
    valid_preds    <- sapply(x$predictions_valid[[x$score_valid$name[[1]]]], FUN = round)
    valid_observed <- x$valid_observed

    confusion_matrix <- function(test) {
      if (test) {
        observed  <- test_observed
        preds     <- test_preds
        text      <- 'testing'
        name      <- x$score_test$name[[1]]
      } else {
        observed  <- valid_observed
        preds     <- valid_preds
        text      <- 'validation'
        name      <- x$score_valid$name[[1]]
      }

      d_binomial <- tibble::tibble('target'= observed, 'prediction' = preds)

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

      p <- ggplot(data =  df, mapping = aes(x = Target, y = Prediction)) +
        geom_tile(aes(fill = Y), colour = 'white') +
        geom_text(aes(label = sprintf('%1.0f', Y)), vjust = 1, colour = colors_discrete_forester(5)[5], size = 5) +
        scale_fill_gradient(low = colors_diverging_forester()[1], high = colors_diverging_forester()[2]) +
        theme_forester() +
        labs(
          title    = paste0('Confusion Matrix'),
          subtitle = paste0('for the best ', text, ' model: ' , name),
          y        = 'Prediction',
          x        = 'Target'
        ) +
        theme(legend.position = 'bottom')

      return(p)
    }
    plot_test  <- confusion_matrix(TRUE)
    plot_valid <- confusion_matrix(FALSE)
    return(patchwork::wrap_plots(plot_test, plot_valid))
  }

  if(type == 'train-test') {
    models_names <- x$score_test$name[1:10]

    train_score <- x$score_train[x$score_train$name %in% models_names, ]
    names(train_score)[which(names(train_score) %in% c('accuracy', 'auc', 'f1'))] <-
      paste0(names(train_score)[which(names(train_score) %in% c('accuracy', 'auc', 'f1'))], '_train')

    test_score <- x$score_test[x$score_test$name %in% models_names, c('accuracy', 'auc', 'f1')]
    names(test_score)[which(names(test_score) %in% c('accuracy', 'auc', 'f1'))] <-
      paste0(names(test_score)[which(names(test_score) %in% c('accuracy', 'auc', 'f1'))], '_test')

    score <- cbind(train_score, test_score)

    p <- ggplot(score, aes(x = .data[[paste0(metric, '_train')]], y = .data[[paste0(metric, '_test')]], color = 'engine')) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      theme_forester() +
      scale_color_manual(values = colors_discrete_forester(length(unique(score$engine)))) +
      labs(
        title = paste(toupper(metric), 'train vs test'),
        x     = 'Train',
        y     = 'Test',
        color = 'Engine'
      ) +
      ggrepel::geom_text_repel(aes(label = score$name), show.legend = FALSE, max.time = 3) +
      scale_x_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')]))) +
      scale_y_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')])))
  }

  return(p)
}

