#' Plot of Metrics for Regression Models
#'
#' Function plot.regression plots measure of quality of regression models.
#'
#' @param x An object, returned from \code{train()} function.
#' @param models A character or numeric that indicates which models
#' will be presented. If `NULL` (the default option) then the three best models
#' will be presented.
#' @param type A character one of `residuals`/`train-test-observed-predicted`/`train-test-rmse`
#' indicates the type of chart.
#' @param metric A character one of `rmse`/`mse`/`r2`/`mae` indicates the metric on the plots.
#' @param ... Other parameters that are necessary for consistency with generic plot function.
#'
#' @return a ggplot2 object
#'
#' @examples
#' \dontrun{
#' library('forester')
#' data('lisbon')
#'
#' x <- train(lisbon, 'Price', bayes_iter = 0, random_evals = 0)
#' plot(x)
#' }
#'
#' @import ggplot2
#' @import patchwork
#' @export


plot.regression <- function(x,
                            models = NULL,
                            type   = 'residuals',
                            metric = 'rmse',
                            ...){
  if (!(c('regression') %in% class(x)))
    stop('The plot() function requires an object created with train() function for regression task.')

  if (!any(type %in% c('residuals', 'train-test-observed-predicted', 'train-test')))
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

  if (type == 'residuals') {
    residuals_all <- c()

    for (model in models_num) {
      residuals     <- (x$test_observed - x$predict_test[[model]])
      residuals_all <- c(residuals_all, residuals)
    }

    residuals_table <-
      data.frame('model' = rep(models_names, each = length(x$test_observed)),
                 'residuals' = residuals_all)

    p <- ggplot(residuals_table, aes(x = model, y = residuals)) +
      geom_boxplot(fill = colors_discrete_forester(1)) +
      theme_forester() +
      coord_flip() +
      labs(
        title    = 'Distribution of residuals',
        subtitle = paste('for ', paste(models_names, collapse = ', ')),
        y        = 'Residuals',
        x        = ''
      )
  }


  if (type == 'train-test-observed-predicted') {
    prediction_train_all <- c()
    prediction_test_all  <- c()

    for (model in models_num) {
      prediction_train     <- x$predict_train[[model]]
      prediction_train_all <- c(prediction_train_all, prediction_train)
      prediction_test      <- x$predict_test[[model]]
      prediction_test_all  <- c(prediction_test_all, prediction_test)
    }

    value_table_train <-
      data.frame(
        'model'      = rep(models_names, each = length(x$train_observed)),
        'observed'   = x$train_observed,
        'prediction' = prediction_train_all,
        'data'       = 'train'
      )

    value_table_test <-
      data.frame(
        'model'      = rep(models_names, each = length(x$test_observed)),
        'observed'   = x$test_observed,
        'prediction' = prediction_test_all,
        'data'       = 'test'
      )

    value_table      <- rbind(value_table_train, value_table_test)
    value_table$data <- factor(value_table$data, levels = c('train', 'test'))

    p <- ggplot(value_table, aes(x = value_table$observed, y = value_table$prediction)) +
      geom_point(color = colors_discrete_forester(1)) +
      geom_abline(intercept = 0, slope = 1) +
      facet_grid(model ~ data, scales = "free_y") +
      theme_forester() +
      labs(
        title    = 'Observed vs prediction',
        subtitle = paste('for ', paste(models_names, collapse = ', ')),
        y        = 'Prediction',
        x        = 'Observed'
      )
  }


  if (type == 'train-test') {
    models_names <- x$score_test$name[1:10]

    train_score <- x$score_train[x$score_train$name %in% models_names, ]
    names(train_score)[which(names(train_score) %in% c('rmse', 'mse', 'r2', 'mae'))] <-
      paste0(names(train_score)[which(names(train_score) %in% c('rmse', 'mse', 'r2', 'mae'))], '_train')

    test_score <- x$score_test[x$score_test$name %in% models_names, c('rmse' , 'mse', 'r2', 'mae')]
    names(test_score)[which(names(test_score) %in% c('rmse', 'mse', 'r2', 'mae'))] <-
      paste0(names(test_score)[which(names(test_score) %in% c('rmse', 'mse', 'r2', 'mae'))], '_test')

    score <- cbind(train_score, test_score)

    p <- ggplot(score, aes_string(x = paste0(metric, '_train'), y = paste0(metric, '_test'), color = 'engine')) +
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
      ggrepel::geom_text_repel(aes(label = score$name), show.legend = FALSE) +
      scale_x_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')]))) +
      scale_y_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')])))
  }
  return(p)
}

