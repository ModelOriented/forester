#' Train models with forester
#'
#' The `train()` function is the core function of this package.
#' The only obligatory arguments are `data` and `target`.
#' Setting and changing other arguments will affect model
#' validation strategy, tested model families, and so on.
#'
#' @param data A `data.frame` or `matrix` - data which will be
#' used to build models. By default model will be trained
#' on all columns in the `data`.
#' @param y A target variable. It can be either
#' (1) a vector of the same number of observations as `data` or
#' (2) a character name of variable in the `data` that contains
#' the target variable.
#' @param type A character, one of `classification`/`regression`/`guess` that
#' sets the type of the task. If `guess` (the default option) then
#' forester will figure out `type` based on the number of unique values
#' in the `y` variable.
#' @param engine A vector of tree-based models that shall be tested.
#' Possible values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#' All models from this vector will be trained and the best one will be returned.
#' @param loss A character with the name of the loss function.
#' If `default` then the default loss function for a given task will be used.
#' @param validation A validation/model selection a character of
#' data.frame. If `data.frame` then this data will be used for the model
#' assessment and selection. If `character`, then it will be a name of
#' the hyperparameter.
#' @param tuning A character with the name of the tuning procedure.
#' one out of `default`/`portfolio`/`bayesian`/`random search`.
#' If `default` then no tuning, just default hyperparameteres are used.
#' If `portfolio` then static portfolio of hyperparametes are tried.
#' If `bayesian` then Bayesian optimization is performed.
#' If `random search` then random search is performed.
#' @param keep Shall all models be returned (`keep = FALSE`) or only the best one
#' (`keep = FALSE`, default option)?
#' @param verbose A logical value, if set to TRUE, provides all information about
#' training process, if FALSE gives none.
#' @param bayes_iter An integer value describing number of optimization rounds
#' used by Bayesian optimization.
#' @param random_iter An integer value describing number of optimization rounds
#' used by random search.
#' @param advanced_preprocessing A logical value describing, whether the user wants to use
#' advanced preprocessing methods (ex. deleting correlated values).
#'
#' @return A list of all necessary objects for other functions.
#' @export
#'
#' @examples
#' library(forester)
#' data('lisbon')
#' train_output <- train(lisbon, 'Price')
#' train_output$ranked_list
train <- function(data,
                  y,
                  type = 'guess',
                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                  loss = 'default',
                  validation = 'default',
                  tuning = 'default',
                  keep = FALSE,
                  verbose = TRUE,
                  bayes_iter = 10,
                  random_iter = 10,
                  advanced_preprocessing = FALSE) {
  if (type == 'guess') {
    type <- guess_type(data, y)
  }
  verbose_cat('Type guessed as: ', type, '\n', verbose = verbose)

  check_report      <- check_data(data, y, verbose)

  preprocessed_data <- preprocessing(data, y, advanced = advanced_preprocessing)
  verbose_cat('Data preprocessed. \n', verbose = verbose)

  split_data        <- train_test_balance(preprocessed_data$data, y, type,
                                   balance = TRUE)
  verbose_cat('Data splitted and balanced \n', verbose = verbose)

  train_data        <- prepare_data(split_data$train, y, engine)
  test_data         <- prepare_data(split_data$test, y, engine, predict = TRUE,
                             split_data$train)
  valid_data        <- prepare_data(split_data$valid, y, engine, predict = TRUE,
                             split_data$train)
  # For creating VS plot and predicting on train (catboost, lgbm).
  raw_train         <- prepare_data(split_data$train, y, engine, predict = TRUE,
                                    split_data$train)

  verbose_cat('Correct formats prepared \n', verbose = verbose)

  model             <- train_models(train_data, y, engine, type)
  verbose_cat('Models sucsesfully trained \n', verbose = verbose)

  predictions       <- predict_models(model, test_data, y, engine, type)
  verbose_cat('Predicted Successfully \n', verbose = verbose)

  score_basic       <- score_models(model, predictions, test_data$ranger_data[[y]], type, metrics = 'all')

  random_best       <- random_search(train_data,
                               test_data,
                               y = y,
                               engine = engine,
                               type = type,
                               max_evals = random_iter,
                               nr_return_models = 'all')

  bayes_best  <- train_models_bayesopt(train_data,
                                      y,
                                      test_data,
                                      engine = engine,
                                      type = type,
                                      iters.n = bayes_iter,
                                      verbose = verbose)

  score_bayes <- score_models(bayes_best, predictions, test_data$ranger_data[[y]], type, metrics = 'all')

  ranked_list <- create_ranked_list(score_basic, random_best, score_bayes, type)

  models_list <- append(model, random_best$best_models)

  models_list <- append(models_list, bayes_best)

  # Remove NULL object from recent steps
  models_list[sapply(models_list, is.null)] <- NULL

  predictions_all  <- predict_models_all(models_list, test_data, y, engine, type)
  verbose_cat('Ranked and models list created. \n', verbose = verbose)

  test_observed    <- split_data$test[[y]]
  train_observed   <- split_data$train[[y]]
  if (type == 'binary_clf') {
    test_observed  <- test_observed - 1 # [0, 1]
    train_observed <- train_observed - 1
  }
  best_models      <- choose_best_models(models_list, ranked_list,  min(10, length(models_list)))

  predictions_best <- predict_models_all(best_models, test_data, y, engine, type)

  verbose_cat('Best models list created. \n', verbose = verbose)

  return(
    list(
      type              = type,
      deleted_columns   = preprocessed_data$colnames,
      preprocessed_data = preprocessed_data$data,
      bin_labels        = preprocessed_data$bin_labels,
      train_data        = train_data,
      test_data         = test_data,
      valid_data        = valid_data,
      predictions       = predictions,
      ranked_list       = ranked_list,
      models_list       = models_list,
      data              = data,
      y                 = y,
      test_observed     = test_observed,
      train_observed    = train_observed,
      best_models       = best_models,
      engine            = engine,
      predictions_all   = predictions_all,
      predictions_best  = predictions_best,
      raw_train         = raw_train
    )
  )
}
