#' Choose the bests models, according to the score data frame
#'
#' @param models A list of models trained by `train_models()` function.
#' @param score A data frame with metrics values for given models, by `score_models`.
#' @param number The number of models to return.
#' @param engine A vector of tree-based models that shall be tested.
#' Possible values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#'
#' @return The list of chosen best models.
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin          <- iris[1:100, ]
#' iris_bin$Species  <- factor(iris_bin$Species)
#' type              <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species', type)
#' preprocessed_data <- preprocessed_data$data
#' split_data <-
#'   train_test_balance(preprocessed_data,
#'                      'Species',
#'                      balance = FALSE)
#' train_data <-
#'   prepare_data(split_data$train,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
#' test_data <-
#'   prepare_data(split_data$test,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                predict = TRUE,
#'                train = split_data$train)
#'
#' model <-
#'   train_models(train_data,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                type = type)
#' predictions <-
#'   predict_models(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#' score <-
#'   score_models(model,
#'                predictions,
#'                observed = split_data$test$Species,
#'                type = type)
#' best_models <-
#'   choose_best_models(model,
#'                      score = score,
#'                      number = 3)
choose_best_models <- function(models, engine, score, number) {
  number <- min(number, length(models))
  return(list(
    models = models[score[1:number, 'name']],
    engine = score[1:number, 'engine']))
}
