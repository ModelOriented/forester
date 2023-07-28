#' Train models with Bayesian Optimization algorithm
#'
#' Bayesian Optimization takes relatively a long time - the bigger `iters.n` param,
#' the more time (but if you want to get model parameters better than default params,
#' it is suggested to set `iters.n` equals 20 at least.
#' Also the bigger dataset, the more time takes Bayesian Optimization.
#'
#' @param train_data A training data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param test_data A test data for models created by `prepare_data()` function.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`, `decision_tree`, `lightgbm`, `catboost`.
#' @param type A string which determines if Machine Learning task is the
#' `binary_clf` or `regression`.
#' @param return_params A logical value, if set to TRUE, returns optimized model parameters.
#' @param iters.n The number of iterations of BayesOpt function.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none.
#'
#' @return Trained models with optimized parameters. If `return_params` is `TRUE`, then
#' returns also training parameters in the one list with models.
#' @export
train_models_bayesopt <- function(train_data,
                                  y,
                                  test_data,
                                  engine,
                                  type,
                                  iters.n = 7,
                                  return_params = FALSE,
                                  verbose = TRUE) {
  if (iters.n <= 0) {
    return(NULL)
  }
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
        classification <- FALSE
        probability    <- FALSE
      } else if (type == 'binary_clf') {
        classification <- TRUE
        probability    <- TRUE
      }

      fitness_fun_ranger <- function(num.trees, min.node.size, max.depth, sample.fraction) {

        model <- ranger::ranger(
          dependent.variable.name = y,
          data            = train_data$ranger_data,
          num.trees       = num.trees,
          min.node.size   = min.node.size,
          max.depth       = max.depth,
          sample.fraction = sample.fraction,
          classification  = classification,
          probability     = probability
        )

        if (type == 'regression') {
          preds    <- ranger::predictions(predict(model, test_data$ranger_data))
        } else if (type == 'binary_clf') {
          preds    <- predict(model, test_data$ranger_data)$predictions[, 2]
        }
        observed   <- test_data$ranger_data[, y]
        max_metric <- NULL

        if (type == 'binary_clf') {
          y_levels   <- levels(factor(train_data$ranger_data[, y]))
          preds      <- factor(1 * (preds > 0.5), levels = c(0, 1), labels = y_levels)
          max_metric <- mean(preds == observed) # accuracy
        } else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(num.trees       = c(5L, 1000L),
                     min.node.size   = c(1L, 10L),
                     max.depth       = c(1L, 100L),
                     sample.fraction = c(0.25, 0.75))

      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_ranger,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_ranger,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (is.null(bayes)) {
        ranger_model <- ranger::ranger(
          dependent.variable.name = y,
          data            = train_data$ranger_data,
          classification  = classification,
          probability     = probability
        )
        verbose_cat(crayon::red('\u2716'), 'ranger: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$ranger_params$num.trees       <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num.trees)
          models_params$ranger_params$min.node.size   <- as.integer(ParBayesianOptimization::getBestPars(bayes)$min.node.size)
          models_params$ranger_params$max.depth       <- as.integer(ParBayesianOptimization::getBestPars(bayes)$max.depth)
          models_params$ranger_params$sample.fraction <- ParBayesianOptimization::getBestPars(bayes)$sample.fraction
        }
        ranger_model <- ranger::ranger(
          dependent.variable.name = y,
          data            = train_data$ranger_data,
          num.trees       = as.integer(ParBayesianOptimization::getBestPars(bayes)$num.trees),
          min.node.size   = as.integer(ParBayesianOptimization::getBestPars(bayes)$min.node.size),
          max.depth       = as.integer(ParBayesianOptimization::getBestPars(bayes)$max.depth),
          sample.fraction = ParBayesianOptimization::getBestPars(bayes)$sample.fraction,
          classification  = classification,
          probability     = probability
        )
        verbose_cat(crayon::green('\u2714'), 'ranger: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'xgboost') {

      if (type == 'binary_clf') {
        obj <- 'binary:logistic'
      } else if (type == 'regression') {
        obj <- 'reg:squarederror'
      } else {
        verbose_cat('Incorrect task type.', verbose = verbose)
      }

      fitness_fun_xgboost <- function(nrounds, eta, subsample, gamma, max_depth) {
        capture.output(
          model <- xgboost::xgboost(
            train_data$xgboost_data,
            label     = as.vector(as.numeric(train_data$ranger_data[[y]])) - 1,
            objective = obj,
            nrounds   = nrounds,
            verbose   = 1,
            eta       = eta,
            subsample = subsample,
            gamma     = gamma,
            max_depth = max_depth))

        preds      <- predict(model, test_data$xgboost_data, type = 'prob')
        observed   <- test_data$ranger_data[, y]
        max_metric <- NULL

        if (type == 'binary_clf') {
          y_levels   <- levels(factor(train_data$ranger_data[, y]))
          preds      <- factor(1 * (preds > 0.5), levels = c(0, 1), labels = y_levels)
          max_metric <- mean(preds == observed) # accuracy
        } else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }
      bounds <- list(nrounds   = c(5L, 100L),
                     eta       = c(0.01, 0.5),
                     subsample = c(0.7, 1),
                     gamma     = c(0, 10),
                     max_depth = c(1L, 10L))

      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_xgboost,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_xgboost,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (is.null(bayes)) {
        capture.output(xgboost_model <- xgboost::xgboost(train_data$xgboost_data,
                                                        label     = as.vector(as.numeric(train_data$ranger_data[[y]])) - 1,
                                                        objective = obj,
                                                        nrounds   = 5000,
                                                        verbose   = 1))
        verbose_cat(crayon::red('\u2716'), 'xgboost: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$xgboost_params$nrounds   <- as.integer(ParBayesianOptimization::getBestPars(bayes)$nrounds)
          models_params$xgboost_params$eta       <- ParBayesianOptimization::getBestPars(bayes)$eta
          models_params$xgboost_params$subsample <- ParBayesianOptimization::getBestPars(bayes)$subsample
          models_params$xgboost_params$gamma     <- ParBayesianOptimization::getBestPars(bayes)$gamma
          models_params$xgboost_params$max_depth <- as.integer(ParBayesianOptimization::getBestPars(bayes)$max_depth)
        }
        capture.output(
          xgboost_model <- xgboost::xgboost(train_data$xgboost_data,
                                            label     = as.vector(as.numeric(train_data$ranger_data[[y]])) - 1,
                                            verbose   = 1,
                                            objective = obj,
                                            nrounds   = as.integer(ParBayesianOptimization::getBestPars(bayes)$nrounds),
                                            eta       = ParBayesianOptimization::getBestPars(bayes)$eta,
                                            subsample = ParBayesianOptimization::getBestPars(bayes)$subsample,
                                            gamma     = ParBayesianOptimization::getBestPars(bayes)$gamma,
                                            max_depth = as.integer(ParBayesianOptimization::getBestPars(bayes)$max_depth)))
        verbose_cat(crayon::green('\u2714'), 'xgboost: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'decision_tree') {
      form        <- as.formula(paste0(y, ' ~.'))
      fitness_fun_decision_tree <- function(minsplit, minprob, maxdepth, nresample) {
        model    <- partykit::ctree(form, data = train_data$decision_tree_data,
                                    minsplit   = minsplit,
                                    minprob    = minprob,
                                    maxdepth   = maxdepth,
                                    nresample  = nresample
        )
        preds      <- predict(model, test_data$decision_tree_data)
        observed   <- test_data$ranger_data[, y]
        max_metric <- NULL

        if (type == 'binary_clf') {
          preds      <- unname(preds)
          max_metric <- mean(preds == observed) # accuracy
        } else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }
        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(minsplit  = c(1L, 60L),
                     minprob   = c(0.01, 1),
                     maxdepth  = c(1L, 20L),
                     nresample = c(1L, 1000L))

      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_decision_tree,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_decision_tree,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (is.null(bayes)) {
        decision_tree_model <- partykit::ctree(form, data = train_data$decision_tree_data)
        verbose_cat(crayon::red('\u2716'), 'decision_tree: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$decision_tree_params$minsplit  <- as.integer(ParBayesianOptimization::getBestPars(bayes)$minsplit)
          models_params$decision_tree_params$minprob   <- ParBayesianOptimization::getBestPars(bayes)$minprob
          models_params$decision_tree_params$maxdepth  <- as.integer(ParBayesianOptimization::getBestPars(bayes)$maxdepth)
          models_params$decision_tree_params$nresample <- as.integer(ParBayesianOptimization::getBestPars(bayes)$nresample)
        }
        decision_tree_model <- partykit::ctree(form,
                                               data      = train_data$decision_tree_data,
                                               minsplit  = as.integer(ParBayesianOptimization::getBestPars(bayes)$minsplit),
                                               minprob   = ParBayesianOptimization::getBestPars(bayes)$minprob,
                                               maxdepth  = as.integer(ParBayesianOptimization::getBestPars(bayes)$maxdepth),
                                               nresample = as.integer(ParBayesianOptimization::getBestPars(bayes)$nresample))
        verbose_cat(crayon::green('\u2714'), 'decision_tree: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'lightgbm') {

      fitness_fun_lightgbm <- function(learning_rate, num_leaves, num_iterations) {

        if (type == 'binary_clf') {
          obj    <- 'binary'
          metric <- 'accuracy'
          params <- list(objective = obj, metric = metric, boosting = 'gbdt')
        } else if (type == 'multi_clf') {
          obj    <- 'multiclass'
          params <- list(objective = obj)
        } else if (type == 'regression') {
          obj    <- 'regression'
          params <- list(objective = obj)
        }

        params = append(params, c(
          learning_rate  = learning_rate,
          num_leaves     = as.integer(num_leaves),
          num_iterations = as.integer(num_iterations)
          ))

        model      <- lightgbm::lgb.train(params  = params,
                                          data    = train_data$lightgbm_data,
                                          verbose = 0)
        preds      <- predict(model, test_data$lightgbm_data)
        observed   <- test_data$ranger_data[, y]
        max_metric <- NULL

        if (type == 'binary_clf') {
          y_levels   <- levels(factor(train_data$ranger_data[, y]))
          preds      <- factor(1 * (preds > 0.5), levels = c(0, 1), labels = y_levels)
          max_metric <- mean(preds == observed) # Accuracy.
        } else {
          max_metric <- -model_performance_rmse(preds, observed) # RMSE.
        }
        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(learning_rate  = c(0.01, 0.5),
                     num_leaves     = c(2L, 50L),
                     num_iterations = c(5L, 100L))

      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_lightgbm,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_lightgbm,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (type == 'binary_clf') {
        obj    <- 'binary'
        params <- list(objective = obj)
      } else if (type == 'multi_clf') {
        obj    <- 'multiclass'
        params <- list(objective = obj)
      } else if (type == 'regression') {
        obj    <- 'regression'
        params <- list(objective = obj)
      }
      if (is.null(bayes)) {
        lightgbm_model <- lightgbm::lgb.train(params  = params,
                                              data    = train_data$lightgbm_data,
                                              verbose = -1)
        verbose_cat(crayon::red('\u2716'), 'lightgbm: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$lightgbm_params$learning_rate  <- ParBayesianOptimization::getBestPars(bayes)$learning_rate
          models_params$lightgbm_params$num_leaves     <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num_leaves)
          models_params$lightgbm_params$num_iterations <- as.integer(ParBayesianOptimization::getBestPars(bayes)$num_iterations)
        }
        params = append(params, c(
          learning_rate  = ParBayesianOptimization::getBestPars(bayes)$learning_rate,
          num_leaves     = as.integer(ParBayesianOptimization::getBestPars(bayes)$num_leaves),
          num_iterations = as.integer(ParBayesianOptimization::getBestPars(bayes)$num_iterations)))

        lightgbm_model <- lightgbm::lgb.train(params  = params,
                                              data    = train_data$lightgbm_data,
                                              verbose = -1)
        verbose_cat(crayon::green('\u2714'), 'lightgbm: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
    else if (engine[i] == 'catboost') {
      fitness_fun_catboost <- function(iterations, border_count, depth, learning_rate, min_data_in_leaf) {
        if (type == 'binary_clf') {
          obj    <- 'Logloss'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'multi_clf') {
          obj    <- 'MultiClass'
          params <- list(loss_function = obj, logging_level = 'Silent')
        } else if (type == 'regression') {
          obj    <- 'RMSE'
          params <- list(loss_function = obj, logging_level = 'Silent')
        }

        params = append(params, c(
          iterations       = as.integer(iterations),
          border_count     = as.integer(border_count),
          depth            = as.integer(depth),
          learning_rate    = learning_rate,
          min_data_in_leaf = as.integer(min_data_in_leaf)))

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

        observed     <- test_data$ranger_data[, y]
        max_metric   <- NULL
        if (type == 'binary_clf') {
          y_levels   <- levels(factor(train_data$ranger_data[, y]))
          preds      <- factor(1 * (preds > 0.5), levels = c(0, 1), labels = y_levels)
          max_metric <- mean(preds == observed) # accuracy
        } else {
          max_metric <- -model_performance_rmse(preds, observed) # rmse
        }

        return(list(Score = as.numeric(max_metric)))
      }

      bounds <- list(iterations       = c(5L, 100L),
                     border_count     = c(64L, 1024L),
                     depth            = c(2L, 16L),
                     learning_rate    = c(0.01, 0.5),
                     min_data_in_leaf = c(1L, 10L))

      bayes <- NULL
      tryCatch(
        expr = {
          if (verbose) {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_catboost,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 1)
          } else {
            bayes <- ParBayesianOptimization::bayesOpt(FUN        = fitness_fun_catboost,
                                                       bounds     = bounds,
                                                       initPoints = length(bounds) + 5,
                                                       iters.n    = iters.n,
                                                       verbose    = 0)
          }
        },
        error = function(e) {
          print(e)
        }
      )

      if (type == 'binary_clf') {
        obj    <- 'Logloss'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'multi_clf') {
        obj    <- 'MultiClass'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'regression') {
        obj    <- 'MAE'
        params <- list(loss_function = obj, logging_level = 'Silent')
      }

      if (is.null(bayes)) {
        capture.output(catboost_model <- catboost::catboost.train(train_data$catboost_data, params = params))
        verbose_cat(crayon::red('\u2716'), 'catboost: Bayesian Optimization failed! The model has default parameters.\n', verbose = verbose)
      } else {
        if (return_params == TRUE) {
          models_params$catboost_params$iterations       <- as.integer(ParBayesianOptimization::getBestPars(bayes)$iterations)
          models_params$catboost_params$border_count     <- as.integer(ParBayesianOptimization::getBestPars(bayes)$border_count)
          models_params$catboost_params$depth            <- as.integer(ParBayesianOptimization::getBestPars(bayes)$depth)
          models_params$catboost_params$learning_rate    <- ParBayesianOptimization::getBestPars(bayes)$learning_rate
          models_params$catboost_params$min_data_in_leaf <- as.integer(ParBayesianOptimization::getBestPars(bayes)$min_data_in_leaf)
        }
        params = append(params, c(
          iterations       = as.integer(ParBayesianOptimization::getBestPars(bayes)$iterations),
          border_count     = as.integer(ParBayesianOptimization::getBestPars(bayes)$border_count),
          depth            = as.integer(ParBayesianOptimization::getBestPars(bayes)$depth),
          learning_rate    = ParBayesianOptimization::getBestPars(bayes)$learning_rate,
          min_data_in_leaf = as.integer(ParBayesianOptimization::getBestPars(bayes)$min_data_in_leaf)))

        capture.output(catboost_model <- catboost::catboost.train(train_data$catboost_data, params = params))
        verbose_cat(crayon::green('\u2714'), 'catboost: Bayesian Optimization was successful!\n', verbose = verbose)
      }
    }
  }

  if (return_params == TRUE) {
    # To remove models that are NULL.
    return_list <- list(
      ranger_bayes        = ranger_model,
      xgboost_bayes       = xgboost_model,
      decision_tree_bayes = decision_tree_model,
      lightgbm_bayes      = lightgbm_model,
      catboost_bayes      = catboost_model,
      models_params       = models_params
    )

    to_rm <- c()
    for (i in 1:length(return_list)) {
      if (is.null(return_list[[i]])) {
        to_rm <- c(to_rm, i)
      }
    }
    if (!is.null(to_rm)) {
      return_list <- return_list[-to_rm]
    }
    return(return_list)
  }
  else {
    # To remove models that are NULL.
    return_list <- list(
      ranger_bayes        = ranger_model,
      xgboost_bayes       = xgboost_model,
      decision_tree_bayes = decision_tree_model,
      lightgbm_bayes      = lightgbm_model,
      catboost_bayes      = catboost_model
    )

    to_rm <- c()
    for (i in 1:length(return_list)) {
      if (is.null(return_list[[i]])) {
        to_rm <- c(to_rm, i)
      }
    }
    if (!is.null(to_rm)) {
      return_list <- return_list[-to_rm]
    }
    return(return_list)
  }
}
