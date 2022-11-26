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
#' @param random_evals An integer value describing number of trained models
#' with different parameters byrandom search.
#' @param advanced_preprocessing A logical value describing, whether the user wants to use
#' advanced preprocessing methods (ex. deleting correlated values).
#' @param metrics A vector of metrics names. By default param set for `auto`, most important metrics are returned.
#' For `all` all metrics are returned. For `NULL` no metrics returned but still sorted by `sort_by`.
#' @param sort_by A string with a name of metric to sort by.
#' For `auto` models going to be sorted by `mse` for regression and `f1` for classification.
#' @param metric_function The self-created function.
#' It should look like name(predictions, observed) and return the numeric value.
#' In case of using `metrics` param with a value other than `auto` or `all`, is needed to use a value `metric_function`
#' in order to see given metric in report. If `sort_by` is equal to `auto` models are sorted by `metric_function`.
#' @param metric_function_name The name of the column with values of `metric_function` parameter.
#' By default `metric_function_name` is `metric_function`.
#' @param metric_function_decreasing A logical value indicating how metric_function should be sorted. `TRUE` by default.
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
                  type = 'auto',
                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                  loss = 'default',
                  validation = 'default',
                  tuning = 'default',
                  keep = FALSE,
                  verbose = TRUE,
                  bayes_iter = 10,
                  random_evals = 10,
                  advanced_preprocessing = FALSE,
                  metrics = 'auto',
                  sort_by = 'auto',
                  metric_function = NULL,
                  metric_function_name = NULL,
                  metric_function_decreasing = TRUE,
                  best_model_number = 1) {
  if (type == 'auto') {
    type <- guess_type(data, y)
  }
  verbose_cat('Type guessed as: ', type, '\n', verbose = verbose)

  if(verbose) {
    check_report <- check_data(data, y, verbose)
  } else {
    check_report <- NULL
  }

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

  model_basic       <- train_models(train_data, y, engine, type)
  verbose_cat('Models sucsesfully trained \n', verbose = verbose)

  preds_basic       <- predict_models_all(model_basic, test_data, y, engine, type)
  verbose_cat('Predicted Successfully \n', verbose = verbose)

  test_observed    <- split_data$test[[y]]
  train_observed   <- split_data$train[[y]]
  valid_observed   <- split_data$valid[[y]]

  model_random      <- random_search(train_data,
                               test_data,
                               y = y,
                               engine = engine,
                               type = type,
                               max_evals = random_evals)
  if (!is.null(model_random)) {
  preds_random      <- predict_models_all(model_random$models,
                                       test_data,
                                       y,
                                       engine = models_random$engine,
                                       type = type)
  }

  model_bayes       <- train_models_bayesopt(train_data,
                                      y,
                                      test_data,
                                      engine = engine,
                                      type = type,
                                      iters.n = bayes_iter,
                                      verbose = verbose)
  if (!is.null(model_bayes)) {
  preds_bayes       <- predict_models_all(model_bayes, test_data, y, engine, type)
  }

  models_all <- c(model_basic, model_random$models, model_bayes)
  engine_all <- c(engine, model_random$engine, engine)
  preds_all  <- c(preds_basic, preds_random, preds_bayes)

  tuning <- c(rep('basic', length(engine)),
              rep('reandom_search', length(model_random$engine)),
              rep('bayes_opt', length(engine)))

    score  <- score_models(models_all,
                           preds_all,
                           test_observed,
                           type,
                           metrics = metrics,
                           sort_by = sort_by,
                           metric_function = metric_function,
                           metric_function_name = metric_function_name,
                           metric_function_decreasing = metric_function_decreasing,
                           engine = engine_all,
                           tuning = tuning)
  predictions_all  <- predict_models_all(models_all, test_data, y, engine, type)
  verbose_cat('Ranked and models list created. \n', verbose = verbose)

  if (type == 'binary_clf') {
    test_observed  <- test_observed - 1 # [0, 1]
    train_observed <- train_observed - 1
  }
  best_models      <- choose_best_models(models_all, engine_all, score, best_model_number)
  predictions_best <- predict_models_all(best_models$models, test_data, y, best_models$engine, type = type)
  predict_valid    <- predict_models_all(models_all, valid_data, y, engine, type = type)

  score_valid      <- score_models(models_all,
                                   predict_valid,
                                   valid_data$ranger_data[[y]],
                                   type,
                                   metrics = metrics,
                                   sort_by = sort_by,
                                   metric_function = metric_function,
                                   metric_function_decreasing = metric_function_decreasing,
                                   engine = engine_all,
                                   tuning = tuning)

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
      score_test        = score,
      score_valid       = score_valid,
      models_list       = models_all,
      data              = data,
      y                 = y,
      test_observed     = test_observed,
      train_observed    = train_observed,
      best_models       = best_models,
      engine            = engine,
      predictions_all   = predictions_all,
      predictions_best  = predictions_best,
      raw_train         = raw_train,
      check_report      = check_report
    )
  )
}
