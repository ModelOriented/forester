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
choose_best_models <- function(models, engine, score, number) {
  number <- min(number, length(models))
  return(list(
    models = models[score[1:number, 'name']],
    engine = score[1:number, 'engine']))
}
