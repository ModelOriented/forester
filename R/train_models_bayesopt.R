#' Train models with Bayesian Optimization algorithm
#'
#' @param train_data A training data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param test_data A test data for models created by `prepare_data()` function.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#' @param type A string which determines if Machine Learning task is the
#' `classification` or `regression`.
#' @param return_params If TRUE, returns optimized model params.
#' @param iters.n The number of iterations of BayesOpt function.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return Trained models with optimized parameters. If `retun_params` is `TRUE`, then
#' returns also training parameters in the one list with models.
#' @export
#'
#' @examples
#' # Binary classification
#' data(iris)
#' iris_bin          <- iris[1:100, ]
#' type              <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species')
#' preprocessed_data <- preprocessed_data$data
#' split_data <-
#'   train_test_balance(preprocessed_data, 'Species', type = type, balance = FALSE)
#' train_data <-
#'   prepare_data(split_data$train,
#'                'Species',
#'                c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
#' test_data <-
#'   prepare_data(split_data$test,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                predict = TRUE,
#'                train = split_data$train)
#'
#' models <- train_models_bayesopt(train_data,
#'                                'Species',
#'                                test_data,
#'                                engine = c('ranger', 'xgboost', 'decision_tree',
#'                                'lightgbm', 'catboost'),
#'                                type = type,
#'                                iters.n = 1,)
#'
#' # Regression
#' type              <- guess_type(lisbon, 'Price')
#' preprocessed_data <- preprocessing(lisbon, 'Price')
#' preprocessed_data <- preprocessed_data$data
#' split_data2 <-
#'   train_test_balance(preprocessed_data,
#'                      y = 'Price',
#'                      type = type,
#'                      balance = FALSE)
#' train_data2 <- prepare_data(split_data2$train,
#'                      y = 'Price',
#'                      engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
#' )
#' test_data2 <-
#'   prepare_data(split_data2$test,
#'                'Price',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                predict = TRUE,
#'                train = split_data2$train)
#'
#'
#' models2 <-
#'    train_models_bayesopt(train_data2,
#'                         'Price',
#'                          test_data2,
#'                          engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                          type = type,
#'                          iters.n = 1)
train_models_bayesopt <- function(train_data,
                                  y,
                                  test_data,
                                  engine,
                                  type,
                                  iters.n = 7,
                                  return_params = FALSE,
                                  verbose = TRUE) {
  ranger_model        <- NULL
  xgboost_model       <- NULL
  decision_tree_model <- NULL
  lightgbm_model      <- NULL
  catboost_model      <- NULL

  models_params                      <- NULL
  models_params$ranger_params        <- NULL
  models_params$xgboost_params       <- NULL
  models_params$decision_tree_params <- NULL
  models_params$lightgbm_params      <- NULL
  models_params$catboost_params      <- NULL

  if (type == 'multi_clf') {
    stop('Multilabel classification is not supported currently!')
  }
  for (i in 1:length(engine)) {
    if (engine[i] == 'ranger') {
      if (type == 'regression') {
        classification <- NULL
        probability    <- FALSE
      } else if (type == 'binary_clf') {
        classification <- TRUE
        probability    <- TRUE
      }

      fitness_fun <- function(num.trees, minprop, alpha) {
        model <- ranger::ranger(
          dependent.variable.name = y,
          data = train_data$ranger_data,
          num.trees = num.trees,
          minprop = minprop,
          alpha = alpha
        )
        preds    <- predict(model, test_data$ranger_data)$predictions
        observed <- test_data$ranger_data[, y]

        max_metric <- NULL
        if (type == 'binary_clf') {
          y_levels <- levels(factor(train_data$ranger_data[, y]))
          preds    <- factor(1 * (preds > 1.5), levels = c(0, 1), labels = y_levels)

          max_metric <-  mean(preds == observed) # accuracy
        }
        else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }
      bounds <- list(num.trees = c(100, 500),
                     minprop = c(0.01, 0.5),
                     alpha = c(0.1, 0.9))

      set.seed(123)
      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 0)
          }
        },
        error = function(e) {

        }
      )

      if (is.null(bayes)) {
        ranger_model <- ranger::ranger(
          dependent.variable.name = y,
          data = train_data$ranger_data
        )
        verbose_cat('- ranger: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$ranger_params$num.trees <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num.trees)
          models_params$ranger_params$minprop   <- as.integer(ParBayesianOptimization::getBestPars(bayes)$minprop)
          models_params$ranger_params$alpha     <- ParBayesianOptimization::getBestPars(bayes)$alpha
        }
        ranger_model <- ranger::ranger(
          dependent.variable.name = y,
          data = train_data$ranger_data,
          num.trees = as.integer(ParBayesianOptimization::getBestPars(bayes)$num.trees),
          minprop = as.integer(ParBayesianOptimization::getBestPars(bayes)$minprop),
          alpha = ParBayesianOptimization::getBestPars(bayes)$alpha,
          classification = classification,
          probability = probability
        )
        verbose_cat('+ ranger: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'xgboost') {
      fitness_fun <- function(nrounds, eta, gamma, min_child_weight) {
        capture.output(
          model <- xgboost::xgboost(
            data = train_data$xgboost_data,
            label = as.vector(train_data$ranger_data[[y]]),
            nrounds = nrounds,
            verbose = 1,
            eta = eta,
            gamma = gamma,
            min_child_weight = min_child_weight))

        preds    <- predict(model, test_data$xgboost_data)

        observed <- test_data$ranger_data[, y]

        max_metric <- NULL
        if (type == 'binary_clf') {
          y_levels <- levels(factor(train_data$ranger_data[, y]))
          preds    <- factor(1 * (preds > 1), levels = c(0, 1), labels = y_levels)

          max_metric <-  mean(preds == observed) # accuracy
        }
        else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(nrounds = c(1, 30),
                     eta = c(0, 1),
                     gamma = c(0, 50),
                     min_child_weight = c(0, 10))
      set.seed(1)
      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 0)
          }
        },
        error = function(e) {

        }
      )

      if (is.null(bayes)) {
        capture.output(xgboost_model <- xgboost::xgboost(train_data$xgboost_data,
                                                        label = as.vector(train_data$ranger_data[[y]]),
                                                        nrounds = 20,
                                                        verbose = 1))
        verbose_cat('- xgboost: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$xgboost_params$nrounds          <- as.integer(ParBayesianOptimization::getBestPars(bayes)$nrounds)
          models_params$xgboost_params$eta              <- ParBayesianOptimization::getBestPars(bayes)$eta
          models_params$xgboost_params$gamma            <- ParBayesianOptimization::getBestPars(bayes)$gamma
          models_params$xgboost_params$min_child_weight <- ParBayesianOptimization::getBestPars(bayes)$min_child_weight
        }
        capture.output(
          xgboost_model <- xgboost::xgboost(train_data$xgboost_data,
                                            label = as.vector(train_data$ranger_data[[y]]),
                                            verbose = 1,
                                            nrounds = as.integer(ParBayesianOptimization::getBestPars(bayes)$nrounds),
                                            eta = ParBayesianOptimization::getBestPars(bayes)$eta,
                                            gamma = ParBayesianOptimization::getBestPars(bayes)$gamma,
                                            min_child_weight = ParBayesianOptimization::getBestPars(bayes)$min_child_weight))
        verbose_cat('+ xgboost: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'decision_tree') {
      form = as.formula(paste0(y, ' ~.'))
      fitness_fun <- function(minsplit, minprob, maxdepth, nresample) {
        model    <- partykit::ctree(form, data = train_data$decision_tree_data,
                                    minsplit = minsplit,
                                    minprob = minprob,
                                    maxdepth = maxdepth,
                                    nresample = nresample
        )

        preds    <- predict(model, test_data$decision_tree_data)

        observed <- test_data$ranger_data[, y]

        max_metric <- NULL
        if (type == 'binary_clf') {
          preds    <- unname(preds)

          max_metric <-  mean(preds == observed) # accuracy
        }
        else {
          max_metric <- - model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(minsplit = c(1, 30),
                     minprob = c(0.01, 0.1),
                     maxdepth = c(2, 20),
                     nresample = c(5000, 9999)
      )

      set.seed(1)
      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (is.null(bayes)) {
        decision_tree_model <- partykit::ctree(form, data = train_data$decision_tree_data)
        verbose_cat('- decision_tree: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$decision_tree_params$minsplit  <- as.integer(ParBayesianOptimization::getBestPars(bayes)$minsplit)
          models_params$decision_tree_params$minprob   <- ParBayesianOptimization::getBestPars(bayes)$minprob
          models_params$decision_tree_params$maxdepth  <- as.integer(ParBayesianOptimization::getBestPars(bayes)$maxdepth)
          models_params$decision_tree_params$nresample <- as.integer(ParBayesianOptimization::getBestPars(bayes)$nresample)
        }
        decision_tree_model <- partykit::ctree(form, data = train_data$decision_tree_data,
                                               minsplit = ParBayesianOptimization::getBestPars(bayes)$minsplit,
                                               minprob = ParBayesianOptimization::getBestPars(bayes)$minprob,
                                               maxdepth = ParBayesianOptimization::getBestPars(bayes)$maxdepth,
                                               nresample = ParBayesianOptimization::getBestPars(bayes)$nresample)
        verbose_cat('+ decision_tree: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'lightgbm') {
      fitness_fun <- function(learning_rate, num_leaves, num_iterations) {
        if (type == 'binary_clf') {
          obj = 'binary'
          params <- list(objective = obj)
        } else if(type == 'multi_clf') {
          obj = 'multiclass'
          params <- list(objective = obj)
        } else if(type == 'regression') {
          obj = 'regression'
          params <- list(objective = obj)
        }

        params = append(params, c(
          learning_rate = learning_rate,
          num_leaves = as.integer(num_leaves),
          num_iterations = as.integer(num_iterations)))

        model    <- lightgbm::lgb.train(params = params,
                                        data = train_data$lightgbm_data,
                                        verbose = -1)

        preds    <- predict(model, test_data$lightgbm_data)

        observed <- test_data$ranger_data[, y]

        max_metric <- NULL
        if (type == 'binary_clf') {
          y_levels <- levels(factor(train_data$ranger_data[, y]))
          preds    <- factor(1 * (preds > 0), levels = c(0, 1), labels = y_levels)

          max_metric <-  mean(preds == observed) # accuracy
        }
        else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(learning_rate = c(0.01, 0.5),
                     num_leaves = c(2, 50),
                     num_iterations = c(1, 200))
      set.seed(1)
      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (type == 'binary_clf') {
        obj = 'binary'
        params <- list(objective = obj)
      } else if (type == 'multi_clf') {
        obj = 'multiclass'
        params <- list(objective = obj)
      } else if (type == 'regression') {
        obj = 'regression'
        params <- list(objective = obj)
      }
      if (is.null(bayes)) {
        lightgbm_model <- lightgbm::lgb.train(params = params,
                                              data = train_data$lightgbm_data,
                                              verbose = -1)
        verbose_cat('- lightgbm: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$lightgbm_params$learning_rate  <- ParBayesianOptimization::getBestPars(bayes)$learning_rate
          models_params$lightgbm_params$num_leaves     <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num_leaves)
          models_params$lightgbm_params$num_iterations <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num_iterations)
        }
        params = append(params, c(
          learning_rate = ParBayesianOptimization::getBestPars(bayes)$learning_rate,
          num_leaves = as.integer(ParBayesianOptimization::getBestPars(bayes)$num_leaves),
          num_iterations = as.integer(ParBayesianOptimization::getBestPars(bayes)$num_iterations)))

        lightgbm_model <- lightgbm::lgb.train(params = params,
                                              data = train_data$lightgbm_data,
                                              verbose = -1)
        verbose_cat('+ lightgbm: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'catboost') {
      fitness_fun <- function(iterations, border_count, depth, learning_rate) {
        if (type == 'binary_clf') {
          obj = 'Logloss'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'multi_clf') {
          obj = 'MultiClass'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'regression') {
          obj = 'MAE'
          params <- list(loss_function = obj, logging_level = 'Silent')
        }

        params = append(params, c(
          iterations = as.integer(iterations),
          border_count = as.integer(border_count),
          depth = as.integer(depth),
          learning_rate = learning_rate))

        capture.output(model <- catboost::catboost.train(train_data$catboost_data, params = params))

        if (type == 'binary_clf') {
          preds <- (catboost::catboost.predict(model,
                                               test_data$catboost_data,
                                               prediction_type = 'Probability'))
        } else {
          preds <- (catboost::catboost.predict(model,
                                               test_data$catboost_data,
                                               prediction_type = 'RawFormulaVal'))
        }

        observed <- test_data$ranger_data[, y]

        max_metric <- NULL
        if (type == 'binary_clf') {
          y_levels <- levels(factor(train_data$ranger_data[, y]))
          preds    <- factor(1 * (preds > 0), levels = c(0, 1), labels = y_levels)

          max_metric <-  mean(preds == observed) # accuracy
        }
        else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(iterations = c(10, 150),
                     border_count = c(10, 50),
                     depth = c(2, 12),
                     learning_rate = c(0.01, 0.3))
      set.seed(1)
      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN = fitness_fun,
                                                       bounds = bounds,
                                                       initPoints = length(bounds) + 2,
                                                       iters.n = iters.n,
                                                       verbose = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (type == 'binary_clf') {
        obj = 'Logloss'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'multi_clf') {
        obj = 'MultiClass'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'regression') {
        obj = 'MAE'
        params <- list(loss_function = obj, logging_level = 'Silent')
      }

      if (is.null(bayes)) {
        capture.output(catboost_model <- catboost::catboost.train(train_data$catboost_data, params = params))
        verbose_cat('- catboost: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$catboost_params$iterations    <- as.integer(ParBayesianOptimization::getBestPars(bayes)$iterations)
          models_params$catboost_params$border_count  <- as.integer(ParBayesianOptimization::getBestPars(bayes)$border_count)
          models_params$catboost_params$depth         <- as.integer(ParBayesianOptimization::getBestPars(bayes)$depth)
          models_params$catboost_params$learning_rate <- ParBayesianOptimization::getBestPars(bayes)$learning_rate
        }
        params = append(params, c(
          iterations = as.integer(ParBayesianOptimization::getBestPars(bayes)$iterations),
          border_count = as.integer(ParBayesianOptimization::getBestPars(bayes)$border_count),
          depth = as.integer(ParBayesianOptimization::getBestPars(bayes)$depth),
          learning_rate = ParBayesianOptimization::getBestPars(bayes)$learning_rate))

        capture.output(catboost_model <- catboost::catboost.train(train_data$catboost_data, params = params))
        verbose_cat('+ catboost: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
  }

  if (return_params == TRUE) {
    return(
      list(
        ranger_bayes        = ranger_model,
        xgboost_bayes       = xgboost_model,
        decision_tree_bayes = decision_tree_model,
        lightgbm_bayes      = lightgbm_model,
        catboost_bayes      = catboost_model,
        models_params       = models_params
      )
    )
  }
  else {
    return(
      list(
        ranger_bayes        = ranger_model,
        xgboost_bayes       = xgboost_model,
        decision_tree_bayes = decision_tree_model,
        lightgbm_bayes      = lightgbm_model,
        catboost_bayes      = catboost_model
      )
    )
  }
}
