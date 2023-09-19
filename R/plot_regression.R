#' Plot of Metrics for Regression Models
#'
#' Function plot.regression plots measure of quality of regression models.
#'
#' @param train_output An object, returned from \code{train()} function.
#' @param models A character or numeric that indicates which models
#' will be presented. If `NULL` (the default option) then the three best models
#' will be presented.
#' @param type A character one of `residuals`/`train-test-observed-predicted`/`train-test-rmse`
#' indicates the type of chart.
#' @param metric A character one of `rmse`/`mse`/`r2`/`mae` indicates the metric on the plots.
#'
#' @return a ggplot2 object
#'
#' @examples
#'
#' library('forester')
#' data('lisbon')
#'
#' train_output <- train(lisbon, 'Price', baye_iter = 0, random_evals = 0)
#'
#' plot(train_output)
#'
#'
#' @import ggplot2
#' @export


plot.regression <- function(train_output,
                            models = NULL,
                            type = 'residuals',
                            metric = 'rmse'){

  if (!(c('regression') %in% class(train_output)))
    stop('The plot() function requires an object created with train() function for regression task.')

  if (!any(type %in% c('residuals', 'train-test-observed-predicted', 'train-test')))
    stop('The selected plot type does not exist.')

  models_names <- models_num <- NULL

  if (is.null(models)) {
    models_names <- train_output$score_test$name[1:3]
    models_num <- 1:3
  } else{
    if (is.character(models)) {
      if (any(models %in% names(train_output$models_list))) {
        models_names <- models[models %in% names(train_output$models_list)]
        models_num <- which(names(train_output$models_list) %in% models)
        if (length(models_num) < length(models)) {
          message(paste0(
            'Check the given models.',
            ' Models do not exist: ',
            paste(models[!models %in% names(train_output$models_list)], collapse = ', '),
            '.'
          ))
        }
      } else{
        stop('Models with the given names do not exist.')
      }
    } else{
    if (any(models %in% 1:length(train_output$models_list))) {
      models_names <-
        names(train_output$models_list)[which(1:length(train_output$models_list) %in% models)]
      models_num <-
        which(1:length(train_output$models_list) %in% models)
      if (length(models_num) < length(models)) {
        message(paste0(
          'Check the given models.',
          ' Models do not exist: ',
          paste(models[which(!models %in% 1:length(train_output$models_list))], collapse = ', '),
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
      residuals <-
        (train_output$test_observed - train_output$predict_test[[model]])
      residuals_all <- c(residuals_all, residuals)
    }

    residuals_table <-
      data.frame('model' = rep(models_names, each = length(train_output$test_observed)),
                 'residuals' = residuals_all)

    p <- ggplot(residuals_table, aes(x = model, y = residuals)) +
      geom_boxplot(fill = colors_discrete_forester(1)) +
      theme_forester() +
      coord_flip() +
      labs(
        title = 'Distribution of residuals',
        subtitle = paste('for ', paste(models_names, collapse = ', ')),
        y = 'Residuals',
        x = ''
      )
  }


  if (type == 'train-test-observed-predicted') {
    prediction_train_all <- c()
    prediction_test_all <- c()

    for (model in models_num) {
      prediction_train <- train_output$predict_train[[model]]
      prediction_train_all <- c(prediction_train_all, prediction_train)
      prediction_test <- train_output$predict_test[[model]]
      prediction_test_all <- c(prediction_test_all, prediction_test)
    }

    value_table_train <-
      data.frame(
        'model' = rep(models_names, each = length(train_output$train_observed)),
        'observed' = train_output$train_observed,
        'prediction' = prediction_train_all,
        'data' = 'train'
      )

    value_table_test <-
      data.frame(
        'model' = rep(models_names, each = length(train_output$test_observed)),
        'observed' = train_output$test_observed,
        'prediction' = prediction_test_all,
        'data' = 'test'
      )

    value_table <- rbind(value_table_train, value_table_test)
    value_table$data <-
      factor(value_table$data, levels = c('train', 'test'))

    p <- ggplot(value_table, aes(x = observed, y = prediction)) +
      geom_point(color = colors_discrete_forester(1)) +
      geom_abline(intercept = 0, slope = 1) +
      facet_grid(model ~ data, scales = "free_y") +
      theme_forester() +
      labs(
        title = 'Observed vs prediction',
        subtitle = paste('for ', paste(models_names, collapse = ', ')),
        y = 'Prediction',
        x = 'Observed'
      )
  }


  if(type == 'train-test') {

  train_score <-
      train_output$score_train[train_output$score_train$name %in% models_names, ] #na razie valid, bo nie ma train w tabeli
    names(train_score)[which(names(train_score) %in% c('rmse', 'mse', 'r2', 'mae'))] <-
      paste0(names(train_score)[which(names(train_score) %in% c('rmse', 'mse', 'r2', 'mae'))], '_train')
    test_score <-
      train_output$score_test[train_output$score_test$name %in% models_names, c('rmse' , 'mse', 'r2', 'mae')]
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
        x = 'Train',
        y = 'Test',
        color = 'Engine'
      ) +
      geom_text_repel(aes(label = name), show.legend = FALSE) +
      scale_x_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')]))) +
      scale_y_continuous(limits = c(min(score[paste0(metric, '_train')], score[paste0(metric, '_test')]),
                                    max(score[paste0(metric, '_train')], score[paste0(metric, '_test')])))


  }
  p
}

