#' Select models from train() output/
#'
#' The function provides a necessary interface for minimizing the size of output
#' created with train(). It not only selects the models, but also the data,
#' predictions, and all other necessary information.
#'
#' @param train_output The output created with train() function.
#' @param model_names The character vector including the names of models, that
#' we want to select.
#'
#' @return The train_output with selected models.
#' @export
#'
#' @examples
#' \dontrun{
#' out     <- train(iris, 'Species', random_evals = 30, bayes_iters = 0)
#' new_out <- select_models(out, c('random_forest_RS_1', 'xgboost_RS_14'))
#' }
select_models <- function(train_output, model_names) {
  output <- train_output
  selected_engines <- c()
  for (engine in output$engine) {
    for (name in model_names) {
      if (grepl(engine, name)) {
        selected_engines <- c(selected_engines, engine)
      }
    }
  }
  output$engine      <- unique(selected_engines)
  output$models_list <- output$models_list[model_names]

  output$train_data  <- output$train_data[paste0(output$engine, '_data')]
  output$test_data   <- output$test_data[paste0(output$engine, '_data')]
  output$valid_data  <- output$valid_data[paste0(output$engine, '_data')]

  output$predictions_train <- output$predictions_train[model_names]
  output$predictions_test  <- output$predictions_test[model_names]
  output$predictions_valid <- output$predictions_valid[model_names]

  output$score_test  <- output$score_test[output$score_test$name %in% model_names, ]
  output$score_train <- output$score_train[output$score_train$name %in% model_names, ]
  output$score_valid <- output$score_valid[output$score_valid$name %in% model_names, ]

  no_best      <- min(length(train_output$best_models_on_valid$engine), length(model_names))
  best_names   <- output$score_valid$name[1:no_best]
  best_engines <- output$score_valid$engine[1:no_best]

  output$best_models_on_valid$models <- output$models_list[best_names]
  output$best_models_on_valid$engine <- best_engines

  output$predictions_best_train <- output$predictions_train[best_names]
  output$predictions_best_test  <- output$predictions_test[best_names]
  output$predictions_best_valid <- output$predictions_valid[best_names]


  if (output$type %in% c('binary_clf', 'multiclass')) {
    output$predictions_train_labels <- output$predictions_train_labels[model_names]
    output$predictions_test_labels  <- output$predictions_test_labels[model_names]
    output$predictions_valid_labels <- output$predictions_valid_labels[model_names]

    output$predictions_best_train_labels <- output$predictions_train_labels[best_names]
    output$predictions_best_test_labels  <- output$predictions_test_labels[best_names]
    output$predictions_best_valid_labels <- output$predictions_valid_labels[best_names]

  }

  return(output)
}
