#' Plot of Metrics for Binary Classification Models
#'
#' Function plot.classification plots measure of quality of regression models.
#'
#' @param train_output An object, returned from \code{train()} function.
#' @param models A character or numeric that indicates which models
#' will be presented. If `NULL` (the default option) then the three best models
#' will be presented.
#' @param type A character one of `TO DO`
#' indicates the type of chart.
#' @param metric A character one of `accuracy`/`auc`/`f1` indicates the metric on the plots.
#'
#' @return a ggplot2 object
#'
#' @examples
#'
#'
#' @import ggplot2
#' @export


plot.classification <- function(train_output,
                            models = NULL,
                            type = 'comparison',
                            metric = 'rmse'){

  if (!(c('binary_clf') %in% class(train_output)))
    stop('The plot() function requires an object created with train() function for binary classification task.')

  if (!any(type %in% c('comparison', 'roc', 'confusion-matrix')))
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

  if (type == 'comparison') {
    scores <-
      data.frame(t(train_output$score_test[, (NCOL(train_output$score_test) - 2):NCOL(train_output$score_test)]))

    y <- data.frame(metric = rownames(scores),
                    value = unlist(scores))

    data <- train_output$score_test
    all <-
      cbind(data[rep(seq_len(nrow(data)), each = 3),], data.frame(metric = rownames(scores),
                                                                  value = unlist(scores)))

    all <- all[, c('name', 'engine', 'tuning', 'metric', 'value')]

    ggplot(all, aes(
      x = name,
      y = value,
      color = metric,
      group = metric
    )) +
      geom_line(size = 1) +
      scale_color_manual(values = colors_discrete_forester(length(unique(all$name)))) +
      theme_forester() +
      scale_y_continuous(limits = c(0, 1)) +
      geom_point() +
      labs(
        title = 'Model comparison',
        y = 'Value of metric',
        x = 'Model',
        color = 'Metric'
      )

    # w przypadku ggradar

    ggradar('TO DO',
            base.size                = 10,
            values.radar             = c(0, 0.5, 1),
            grid.line.width          = 0.6,
            gridline.min.linetype    = 2,
            gridline.mid.linetype    = 2,
            gridline.max.linetype    = 2,
            gridline.min.colour      = 'gray',
            gridline.mid.colour      = 'gray',
            gridline.max.colour      = 'gray',
            grid.label.size          = 5,
            axis.line.colour         = 'gray',
            axis.label.size          = 2.5,
            axis.label.offset        = 1.15,
            group.line.width         = 1.5,
            group.point.size         = 4,
            legend.title             = 'Metric',
            plot.title               = 'title',
            legend.text.size         = 10,
            legend.position          = 'right',
            background.circle.colour = 'white') +
      ggplot2::coord_fixed() +
      scale_color_manual(values = colors_discrete_forester(length(unique(train_output$score_test$engine))))

  }


  if (type == 'roc') {
  }


  if(type == 'confusion-matrix') {
  }
}

