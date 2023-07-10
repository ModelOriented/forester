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
#' @param split_seed An integer value describing the seed for the split into
#' train, test, and validation datasets. By default no seed is set and the split
#' is performed randomly. Default value is NULL.
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
#' \itemize{
#' \item \code{`data`} The original data.
#' \item \code{`y`} The original target column name.
#' \item \code{`type`} The type of the ML task. If the user did not specify a type in the
#' input parameters, the algorithm recognizes, uses and returns the same type.
#' It could be 'regression' or 'classification'.
#'
#' \item \code{`deleted_columns`} Column names from the original data frame that have been
#' removed in the data preprocessing process, e.g. due to too high correlation
#' with other columns.
#' \item \code{`preprocessed_data`} The data frame after the preprocessing process - that
#' means: removing columns with one value for all rows, binarizing the target
#' column, managing missing values and in advanced preprocessing: deleting
#' correlated values, deleting columns that are ID-like columns and performing
#' Boruta algorithm for selecting most important features.
#' \item \code{`bin_labels`} Labels of binarized target value - {1, 2} values for binary
#' classification and NULL for regression.
#'
#' \item \code{`models_list`} The list of all trained models.
#' \item \code{`check_report`} Data check report held as a list of strings. It is used
#' by the `report()` function.
#' \item \code{`outliers`} The vector of possible outliers detected by the `check_data()`.
#'
#' \item \code{`best_models_on_valid`} The object containing the best performing models
#' on the valdiation dataset.
#' #' \item \code{`engine`} The list of names of all types of trained models. Possible
#' values: 'ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'.
#' \item \code{`raw_train`} The another form of the training dataset (useful for creating
#' VS plot and predicting on training dataset for catboost and lightgbm models).
#'
#' \item \code{`train_data`} The training dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and validation
#' datasets.
#' \item \code{`test_data`} The test dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and
#' validation datasets.
#' \item \code{`valid_data`} The validation dataset - the part of the source dataset after
#' preprocessing, balancing and splitting into the training, test and validation
#' datasets.
#'
#' \item \code{`train_inds`} The vector of integers describing the observation indexes from
#' the original data frame that went to the training set.
#' \item \code{`test_inds`} The vector of integers describing the observation indexes from
#' the original data frame that went to the testing set.
#' \item \code{`valid_inds`} The vector of integers describing the observation indexes from
#' the original data frame that went to the validation set.
#'
#' \item \code{`predictions_train`} Predictions for all trained models on a train dataset.
#' \item \code{`predictions_test`} Predictions for all trained models on a test dataset.
#' \item \code{`predictions_valid`} Predictions for all trained models on a validation dataset.
#'
#' \item \code{`predictions_train_labels`} Predictions for all trained models on a
#' train dataset with human readable labels.
#' \item \code{`predictions_test_labels`} Predictions for all trained models on a
#' test dataset with human readable labels.
#' \item \code{`predictions_valid_labels`} Predictions for all trained models on a
#' validation dataset with human readable labels.
#'
#' \item \code{`predictions_best_train`} Predictions for best trained models on a train dataset.
#' \item \code{`predictions_best_test`} Predictions for best trained models on a test dataset.
#' \item \code{`predictions_best_valid`} Predictions for best trained models on a validation dataset.
#'
#' \item \code{`predictions_best_train_labels`} Predictions for best trained models on a
#' train dataset with human readable labels.
#' \item \code{`predictions_best_test_labels`} Predictions for best trained models on a
#' test dataset with human readable labels.
#' \item \code{`predictions_best_valid_labels`} Predictions for best trained models on a
#' validation dataset with human readable labels.
#'
#' \item \code{`score_train`} The list of metrics for all trained models calculated on a train
#' dataset. For regression task there are: mse, r2 and mad metrics. For the
#' classification task there are: f1, auc, recall, precision and accuracy.
#' \item \code{`score_test`} The list of metrics for all trained models calculated on a test
#' dataset. For regression task there are: mse, r2 and mad metrics. For the
#' classification task there are: f1, auc, recall, precision and accuracy.
#' \item \code{`score_valid`} The list of metrics for all trained models calculated on a validation
#' dataset. For regression task there are: mse, r2 and mad metrics. For the
#' classification task there are: f1, auc, recall, precision and accuracy.
#'
#' \item \code{`test_observed`} Values of y column from the test dataset.
#' \item \code{`train_observed`} Values of y column from the training dataset.
#' \item \code{`valid_observed`} Values of y column from the validation dataset.
#'
#' \item \code{`test_observed_labels`} Values of y column from the test dataset as text labels
#' (for classification task only).
#' \item \code{`train_observed_labels`} Values of y column from the training dataset as text
#' labels (for classification task only).
#' \item \code{`valid_observed_labels`} Values of y column from the validation dataset as text
#' labels (for classification task only).
#' }
#' @export
#'
#' @examples
#' library(forester)
#' data('lisbon')
#' train_output <- train(lisbon, 'Price')
#' train_output$score_valid
train <- function(data,
                  y,
                  type = 'auto',
                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm'),
                  verbose = TRUE,
                  train_test_split = c(0.6, 0.2, 0.2),
                  split_seed = NULL,
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

  if ('tbl' %in% class(data)) {
    data <- as.data.frame(data)
    verbose_cat(crayon::red('\u2716'), 'Provided dataset is a tibble and not a',
                'data.frame or matrix. Casting the dataset to data.frame format. \n\n',
                verbose = verbose)
  }

  if (type == 'auto') {
    type <- guess_type(data, y)
    verbose_cat(crayon::green('\u2714'), 'Type guessed as: ', type, '\n\n', verbose = verbose)
  } else if (!type %in% c('regression', 'binary_clf')) {
    verbose_cat(crayon::red('\u2716'), 'Invalid value. Correct task types are: `binary_clf`, `regression`, and `auto` for automatic task identification \n\n', verbose = verbose)
  } else {
    verbose_cat(crayon::green('\u2714'), 'Type provided as: ', type, '\n\n', verbose = verbose)
  }

  # Move target to be the last feature, for easier integrity.
  #X       <- data[names(data) != y]
  #Y       <- data[y]
  #data    <- X
  #data[y] <- Y

  check_report <- check_data(data, y, verbose)

  preprocessed_data <- preprocessing(data, y, type, advanced = advanced_preprocessing)

  if (advanced_preprocessing) {
    verbose_cat(crayon::red('\u2716'), 'Columns deleted during the advanced preprocessing: \n',
                preprocessed_data$colnames, '\n\n', verbose = verbose)
  }

  verbose_cat(crayon::green('\u2714'), 'Data preprocessed. \n', verbose = verbose)

  # Data splitting and recording observed variables in each dataset.
  split_data <- train_test_balance(preprocessed_data$data, y, balance = TRUE,
                                   fractions = train_test_split, seed = split_seed)

  train_observed <- split_data$train[[y]]
  test_observed  <- split_data$test[[y]]
  valid_observed <- split_data$valid[[y]]

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

  model_basic    <- train_models(train_data, y, engine, type)
  verbose_cat(crayon::green('\u2714'), 'Models with default parameters successfully trained. \n', verbose = verbose)

  # preds_basic    <- predict_models_all(model_basic, test_data, y, type)
  # verbose_cat(crayon::green('\u2714'), 'Predicted successfully. \n', verbose = verbose)

  model_random   <- random_search(train_data,
                                  test_data,
                                  y = y,
                                  engine = engine,
                                  type = type,
                                  max_evals = random_evals)

  verbose_cat(crayon::green('\u2714'), 'Models optimized with random search successfully trained. \n', verbose = verbose)

  # preds_random <- NULL
  #
  # if (!is.null(model_random)) {
  #   preds_random <- predict_models_all(model_random$models,
  #                                      test_data,
  #                                      y,
  #                                      type = type)
  # }

  model_bayes <- train_models_bayesopt(train_data,
                                       y,
                                       test_data,
                                       engine = engine,
                                       type = type,
                                       iters.n = bayes_iter,
                                       verbose = verbose)

  verbose_cat(crayon::green('\u2714'), 'Models optimized with Bayesian Optimization successfully trained. \n', verbose = verbose)

  # preds_bayes <- NULL
  # if (!is.null(model_bayes)) {
  #   preds_bayes <- predict_models_all(model_bayes, test_data, y, type)
  # }

  models_all <- c(model_basic, model_random$models, model_bayes)
  engine_all <- c(engine, model_random$engine, engine)
  # preds_all  <- c(preds_basic, preds_random, preds_bayes)

  tuning <- c(rep('basic', length(engine)),
              rep('random_search', length(model_random$engine)),
              rep('bayes_opt', length(engine)))

  # predictions_all  <- predict_models_all(models_all, test_data, y, type)
  # verbose_cat(crayon::green('\u2714'), 'Ranked and models list created. \n', verbose = verbose)


  predict_train    <- predict_models_all(models_all, raw_train, y, type = type)
  predict_test     <- predict_models_all(models_all, test_data, y, type = type)
  predict_valid    <- predict_models_all(models_all, valid_data, y, type = type)

  verbose_cat(crayon::green('\u2714'), 'Created the predictions for all models. \n', verbose = verbose)

  score_train <- score_models(models_all,
                              predict_train,
                              train_data$ranger_data[[y]],
                              type,
                              metrics = metrics,
                              sort_by = sort_by,
                              metric_function = metric_function,
                              metric_function_name = metric_function_name,
                              metric_function_decreasing = metric_function_decreasing,
                              engine = engine_all,
                              tuning = tuning)

  score_test  <- score_models(models_all,
                              predict_test,
                              test_data$ranger_data[[y]],
                              type,
                              metrics = metrics,
                              sort_by = sort_by,
                              metric_function = metric_function,
                              metric_function_name = metric_function_name,
                              metric_function_decreasing = metric_function_decreasing,
                              engine = engine_all,
                              tuning = tuning)

  score_valid <- score_models(models_all,
                              predict_valid,
                              valid_data$ranger_data[[y]],
                              type,
                              metrics = metrics,
                              sort_by = sort_by,
                              metric_function = metric_function,
                              metric_function_name = metric_function_name,
                              metric_function_decreasing = metric_function_decreasing,
                              engine = engine_all,
                              tuning = tuning)

  verbose_cat(crayon::green('\u2714'), 'Created the score boards for all models. \n', verbose = verbose)

  best_models_on_valid   <- choose_best_models(models_all, engine_all, score_valid, best_model_number)
  predictions_best_train <- predict_models_all(best_models_on_valid$models, raw_train, y, type = type)
  predictions_best_test  <- predict_models_all(best_models_on_valid$models, test_data, y, type = type)
  predictions_best_valid <- predict_models_all(best_models_on_valid$models, valid_data, y, type = type)

  verbose_cat(crayon::green('\u2714'), 'Created the predctions for best models. \n', verbose = verbose)

  # Providing the original labels to the target.
  if (type == 'binary_clf') {
    test_observed  <- as.numeric(test_observed) - 1 # [0, 1]
    train_observed <- as.numeric(train_observed) - 1
    valid_observed <- as.numeric(valid_observed) - 1

    test_observed_labels           <- test_observed
    train_observed_labels          <- train_observed
    valid_observed_labels          <- valid_observed

    predict_train_labels           <- predict_train
    predict_test_labels            <- predict_test
    predict_valid_labels           <- predict_valid

    predictions_best_train_labels  <- predictions_best_train
    predictions_best_test_labels   <- predictions_best_test
    predictions_best_valid_labels  <- predictions_best_valid

    labels <- preprocessed_data$bin_labels

    # Human-readable observed values with text labels.
    # For the observed values.
    for (i in 1:length(train_observed)) {
      if (train_observed[i] < 0.5) {
        train_observed_labels[i] <- labels[1]
      } else {
        train_observed_labels[i] <- labels[2]
      }
    }
    for (i in 1:length(test_observed)) {
      if (test_observed[i] < 0.5) {
        test_observed_labels[i] <- labels[1]
      } else {
        test_observed_labels[i] <- labels[2]
      }
    }
    for (i in 1:length(valid_observed)) {
      if (valid_observed[i] < 0.5) {
        valid_observed_labels[i] <- labels[1]
      } else {
        valid_observed_labels[i] <- labels[2]
      }
    }
    # For the all models predictions.
    for (j in 1:length(predict_train)){
      for (i in 1:length(predict_train[[j]])) {
        if (predict_train[[j]][i] < 0.5) {
          predict_train_labels[[j]][i] <- labels[1]
        } else {
          predict_train_labels[[j]][i] <- labels[2]
        }
      }
      for (i in 1:length(predict_test[[j]])) {
        if (predict_test[[j]][i] < 0.5) {
          predict_test_labels[[j]][i] <- labels[1]
        } else {
          predict_test_labels[[j]][i] <- labels[2]
        }
      }
      for (i in 1:length(predict_valid[[j]])) {
        if (predict_valid[[j]][i] < 0.5) {
          predict_valid_labels[[j]][i] <- labels[1]
        } else {
          predict_valid_labels[[j]][i] <- labels[2]
        }
      }
    }

    # For the best models predictions.
    for (j in 1:length(predictions_best_train)){
      for (i in 1:length(predictions_best_train[[j]])) {
        if (predictions_best_train[[j]][i] < 0.5) {
          predictions_best_train_labels[[j]][i] <- labels[1]
        } else {
          predictions_best_train_labels[[j]][i] <- labels[2]
        }
      }
      for (i in 1:length(predictions_best_test[[j]])) {
        if (predictions_best_test[[j]][i] < 0.5) {
          predictions_best_test_labels[[j]][i] <- labels[1]
        } else {
          predictions_best_test_labels[[j]][i] <- labels[2]
        }
      }
      for (i in 1:length(predictions_best_valid[[j]])) {
        if (predictions_best_valid[[j]][i] < 0.5) {
          predictions_best_valid_labels[[j]][i] <- labels[1]
        } else {
          predictions_best_valid_labels[[j]][i] <- labels[2]
        }
      }
    }
  }

  verbose_cat(crayon::green('\u2714'), 'Created human-readable labels for observables and predictions. \n', verbose = verbose)

  if (type == 'binary_clf') {
    return(
      list(
        data                    = data,
        y                       = y,
        type                    = type,

        deleted_columns         = preprocessed_data$colnames,
        preprocessed_data       = preprocessed_data$data,
        bin_labels              = preprocessed_data$bin_labels,

        models_list             = models_all,
        check_report            = check_report$str,
        outliers                = check_report$outliers,

        best_models_on_valid    = best_models_on_valid,
        engine                  = engine,
        raw_train               = raw_train,

        train_data              = train_data,
        test_data               = test_data,
        valid_data              = valid_data,

        train_inds              = split_data$train_inds,
        test_inds               = split_data$test_inds,
        valid_inds              = split_data$valid_inds,

        predictions_train              = predict_train,
        predictions_test               = predict_test,
        predictions_valid              = predict_valid,

        predictions_train_labels       = predict_train_labels,
        predictions_test_labels        = predict_test_labels,
        predictions_valid_labels       = predict_valid_labels,

        predictions_best_train         = predictions_best_train,
        predictions_best_test          = predictions_best_test,
        predictions_best_valid         = predictions_best_valid,

        predictions_best_train_labels  = predictions_best_train_labels,
        predictions_best_test_labels   = predictions_best_test_labels,
        predictions_best_valid_labels  = predictions_best_valid_labels,

        score_test              = score_test,
        score_train             = score_train,
        score_valid             = score_valid,

        test_observed           = test_observed,
        train_observed          = train_observed,
        valid_observed          = valid_observed,

        test_observed_labels    = test_observed_labels,
        train_observed_labels   = train_observed_labels,
        valid_observed_labels   = valid_observed_labels
      )
    )
  } else {
    return(
      list(
        type                    = type,
        deleted_columns         = preprocessed_data$colnames,
        preprocessed_data       = preprocessed_data$data,
        bin_labels              = preprocessed_data$bin_labels,

        models_list             = models_all,
        data                    = data,
        y                       = y,

        raw_train               = raw_train,
        check_report            = check_report$str,
        outliers                = check_report$outliers,

        best_models_on_valid    = best_models_on_valid,
        engine                  = engine,

        train_data              = train_data,
        test_data               = test_data,
        valid_data              = valid_data,

        train_inds              = split_data$train_inds,
        test_inds               = split_data$test_inds,
        valid_inds              = split_data$valid_inds,

        predict_train           = predict_train,
        predict_test            = predict_test,
        predict_valid           = predict_valid,

        predictions_best_train  = predictions_best_train,
        predictions_best_test   = predictions_best_test,
        predictions_best_valid  = predictions_best_valid,

        score_test              = score_test,
        score_train             = score_train,
        score_valid             = score_valid,

        test_observed           = test_observed,
        train_observed          = train_observed,
        valid_observed          = valid_observed
      )
    )
  }
}
