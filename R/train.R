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
#' @param type A character, one of `binary_clf`/`regression`/`guess` that
#' sets the type of the task. If `guess` (the default option) then
#' forester will figure out `type` based on the number of unique values
#' in the `y` variable.
#' @param engine A vector of tree-based models that shall be tested.
#' Possible values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#' All models from this vector will be trained and the best one will be returned.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' training process, if FALSE gives none.
#' @param train_test_split A 3-value vector, describing the proportions of train,
#' test, validation subsets to original data set. Default values are: c(0.6, 0.2, 0.2).
#' @param bayes_iter An integer value describing number of optimization rounds
#' used by the Bayesian optimization.
#' @param random_evals An integer value describing number of trained models
#' with different parameters by random search.
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
#' @param best_model_number Number best models to be chosen as element of the return.
#' All trained models will be returned as different element of the return.
#'
#' @return A list of all necessary objects for other functions. It contains:
#' `type` The type of the ML task. If the user did not specify a type in the
#' input parameters, the algorithm recognizes, uses and returns the same type.
#' It could be 'regression' or 'classification'.
#' `deleted_columns` Column names from the original data frame that have been
#' removed in the data preprocessing process, e.g. due to too high correlation
#' with other columns.
#' `preprocessed_data` The data frame after the preprocessing process - that
#' means: removing columns with one value for all rows, binarizing the target
#' column, managing missing values and in advanced preprocessing: deleting
#' correlated values, deleting columns that are ID-like columns and performing
#' Boruta algorithm for selecting most important features.
#' `bin_labels` Labels of binarized target value - {1, 2} values for binary
#' classification and NULL for regression.
#' `train_data` The training dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and validation
#' datasets.
#' `test_data` The test dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and
#' validation datasets.
#' `valid_data` The validation dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and validation
#' datasets.
#' `predictions` Prediction list for all trained models based on the training
#' dataset.
#' `ranked_list` The list of metrics for all trained models. For regression task
#' there are: mse, r2 and mad metrics. For the classification task there are:
#' f1, auc, recall, precision and accuracy.
#' `models_list` The list of all trained models.
#' `data` The original data.
#' `y` The original target column name.
#' `test_observed` Values of y column from the test dataset.
#' `train_observed` Values of y column from the training dataset.
#' `valid_observed` Values of y column from the validation dataset.
#' `test_observed_labels` Values of y column from the test dataset as text labels
#' (for classification task only).
#' `train_observed_labels` Values of y column from the training dataset as text
#' labels (for classification task only).
#' `valid_observed_labels` Values of y column from the validation dataset as text
#' labels (for classification task only).
#' `best_models` Ranking list of top 10 trained models - with default
#' parameters, with parameters optimized with the Bayesian optimization
#' algorithm and with parameters optimized with the random search algorithm.
#' `engine` The list of names of all types of trained models. Possible
#' values: 'ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'.
#' `predictions_all` Predictions for all trained models.
#' `predictions_best` Predictions for models from best_models list.
#' `predictions_all_labels` Predictions for all trained models as text labels
#' (for classification task only).
#' `predictions_best_labels` Predictions for models from best_models list as
#' labels (for classification task only).
#' `raw_train` The another form of the training dataset (useful for creating
#' VS plot and predicting on training dataset for catboost and lightgbm models).
#' `outliers` The vector of possible outliers detected by the `check_data()`.
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
                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm'),
                  verbose = TRUE,
                  train_test_split = c(0.6, 0.2, 0.2),
                  bayes_iter = 10,
                  random_evals = 10,
                  advanced_preprocessing = FALSE,
                  metrics = 'auto',
                  sort_by = 'auto',
                  metric_function = NULL,
                  metric_function_name = NULL,
                  metric_function_decreasing = TRUE,
                  best_model_number = 5) {
  tryCatch({
    if ('catboost' %in% engine) {
      find.package('catboost')
    }
  },
  error = function(cond) {
    verbose_cat(crayon::red('\u2716'), 'Package not found: catboost, to use it please ',
                'follow guides for installation from GitHub repository README.',
                'Otherwise remove it from the engine \n\n', verbose = verbose)
    return(NULL)
  })

  if (type == 'auto') {
    type <- guess_type(data, y)
    verbose_cat(crayon::green('\u2714'), 'Type guessed as: ', type, '\n\n', verbose = verbose)
  } else if (!type %in% c('regression', 'binary_clf')) {
    verbose_cat(crayon::red('\u2716'), 'Invalid value. Correct task types are: `binary_clf`, `regression`, and `auto` for automatic task identification \n\n', verbose = verbose)
  } else {
    verbose_cat(crayon::green('\u2714'), 'Type provided as: ', type, '\n\n', verbose = verbose)
  }


  if (verbose) {
    check_report <- check_data(data, y, verbose)
  } else {
    check_report          <- c()
    check_report$str      <- NULL
    check_report$outliers <- NULL
  }

 preprocessed_data <- preprocessing(data, y, advanced = advanced_preprocessing)

 if (advanced_preprocessing) {
   verbose_cat(crayon::red('\u2716'), 'Columns deleted during the advanced preprocessing: \n',
               preprocessed_data$colnames, '\n\n', verbose = verbose)
 }


  verbose_cat(crayon::green('\u2714'), 'Data preprocessed. \n', verbose = verbose)

  split_data <- train_test_balance(preprocessed_data$data, y, balance = TRUE,
                                   fractions = train_test_split)
  verbose_cat(crayon::green('\u2714'), 'Data split and balanced. \n', verbose = verbose)

  train_data <- prepare_data(split_data$train, y, engine)

  test_data  <- prepare_data(split_data$test, y, engine, predict = TRUE,
                             split_data$train)
  valid_data <- prepare_data(split_data$valid, y, engine, predict = TRUE,
                             split_data$train)
  # For creating VS plot and predicting on train (catboost, lgbm).
  raw_train  <- prepare_data(split_data$train, y, engine, predict = TRUE,
                             split_data$train)

  verbose_cat(crayon::green('\u2714'), 'Correct formats prepared. \n', verbose = verbose)

  model_basic       <- train_models(train_data, y, engine, type)
  verbose_cat(crayon::green('\u2714'), 'Models successfully trained. \n', verbose = verbose)

  preds_basic       <- predict_models_all(model_basic, test_data, y, type)
  verbose_cat(crayon::green('\u2714'), 'Predicted successfully. \n', verbose = verbose)

  test_observed    <- split_data$test[[y]]
  train_observed   <- split_data$train[[y]]
  valid_observed   <- split_data$valid[[y]]

  model_random <- random_search(train_data,
                               test_data,
                               y = y,
                               engine = engine,
                               type = type,
                               max_evals = random_evals)
  if (!is.null(model_random)) {
    preds_random <- predict_models_all(model_random$models,
                                       test_data,
                                       y,
                                       type = type)
  }

  model_bayes <- train_models_bayesopt(train_data,
                                       y,
                                       test_data,
                                       engine = engine,
                                       type = type,
                                       iters.n = bayes_iter,
                                       verbose = verbose)
  preds_bayes <- NULL
  if (!is.null(model_bayes)) {
    preds_bayes <- predict_models_all(model_bayes, test_data, y, type)
  }

  models_all <- c(model_basic, model_random$models, model_bayes)
  engine_all <- c(engine, model_random$engine, engine)
  preds_all  <- c(preds_basic, preds_random, preds_bayes)

  tuning <- c(rep('basic', length(engine)),
              rep('random_search', length(model_random$engine)),
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
  predictions_all  <- predict_models_all(models_all, test_data, y, type)
  verbose_cat(crayon::green('\u2714'), 'Ranked and models list created. \n', verbose = verbose)

  if (type == 'binary_clf') {
    test_observed  <- test_observed - 1 # [0, 1]
    train_observed <- train_observed - 1
    valid_observed <- valid_observed - 1

    test_observed_labels  <- test_observed
    train_observed_labels <- train_observed
    valid_observed_labels <- valid_observed

    # human-readable observed vaues with text labels
    for (i in 1:length(test_observed)) {
      if (test_observed[i] < 0.5) {
        test_observed_labels[i] <- preprocessed_data$bin_labels[1]
      } else {
        test_observed_labels[i] <- preprocessed_data$bin_labels[2]
      }
    }
    for (i in 1:length(train_observed)) {
      if (train_observed[i] < 0.5) {
        train_observed_labels[i] <- preprocessed_data$bin_labels[1]
      } else {
        train_observed_labels[i] <- preprocessed_data$bin_labels[2]
      }
    }
    for (i in 1:length(valid_observed)) {
      if (train_observed[i] < 0.5) {
        valid_observed_labels[i] <- preprocessed_data$bin_labels[1]
      } else {
        valid_observed_labels[i] <- preprocessed_data$bin_labels[2]
      }
    }
  }

  best_models      <- choose_best_models(models_all, engine_all, score, best_model_number)
  predictions_best <- predict_models_all(best_models$models, test_data, y, type = type)
  predict_valid    <- predict_models_all(models_all, valid_data, y, type = type)

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

  verbose_cat(crayon::green('\u2714'), 'Best models list created. \n', verbose = verbose)

  # Easy human-readable predictions
  if (type == 'binary_clf') {
    predictions_all_labels  <- predictions_all
    predictions_best_labels <- predictions_best
    for (i in 1:length(predictions_all)) {
      for (j in 1:length(predictions_all[[i]])) {
        if (predictions_all[[i]][j] < 0.5) {
          predictions_all_labels[[i]][j] <- preprocessed_data$bin_labels[1]
        } else {
          predictions_all_labels[[i]][j] <- preprocessed_data$bin_labels[2]
        }
      }
    }
    for (i in 1:length(predictions_best)) {
      for (j in 1:length(predictions_best[[i]])) {
        if (predictions_best[[i]][j] < 0.5) {
          predictions_best_labels[[i]][j] <- preprocessed_data$bin_labels[1]
        } else {
          predictions_best_labels[[i]][j] <- preprocessed_data$bin_labels[2]
        }
      }
    }
  }
  if (type == 'binary_clf') {
    return(
      list(
        type                    = type,
        deleted_columns         = preprocessed_data$colnames,
        preprocessed_data       = preprocessed_data$data,
        bin_labels              = preprocessed_data$bin_labels,
        train_data              = train_data,
        test_data               = test_data,
        valid_data              = valid_data,
        predictions             = predictions_all,
        score_test              = score,
        score_valid             = score_valid,
        models_list             = models_all,
        data                    = data,
        y                       = y,
        test_observed           = test_observed,
        train_observed          = train_observed,
        valid_observed          = valid_observed,
        test_observed_labels    = test_observed_labels,
        train_observed_labels   = train_observed_labels,
        valid_observed_labels   = valid_observed_labels,
        best_models             = best_models,
        engine                  = engine,
        predictions_all         = predictions_all,
        predictions_best        = predictions_best,
        predictions_all_labels  = predictions_all_labels,
        predictions_best_labels = predictions_best_labels,
        raw_train               = raw_train,
        check_report            = check_report$str,
        outliers                = check_report$outliers
      )
    )
  } else {
    return(
      list(
        type                    = type,
        deleted_columns         = preprocessed_data$colnames,
        preprocessed_data       = preprocessed_data$data,
        bin_labels              = preprocessed_data$bin_labels,
        train_data              = train_data,
        test_data               = test_data,
        valid_data              = valid_data,
        predictions             = predictions_all,
        score_test              = score,
        score_valid             = score_valid,
        models_list             = models_all,
        data                    = data,
        y                       = y,
        test_observed           = test_observed,
        train_observed          = train_observed,
        valid_observed          = valid_observed,
        best_models             = best_models,
        engine                  = engine,
        predictions_all         = predictions_all,
        predictions_best        = predictions_best,
        raw_train               = raw_train,
        check_report            = check_report,
        outliers                = check_report$outliers
      )
    )
  }
}
