#' Random optimalization of hiperparameters
#'
#' @param train_data A training data for models created by `prepare_data()` function.
#' @param test_data A test data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name.
#' @param models A list of models trained by `train_models()` function.
#' They will be compered with models trained with different hiperparameters.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`, `desicion tree`, `lightgbm`, `catboost`.
#' @param type A string that determines if Machine Learning task is the
#' `classification` or `regression` task.
#' @param max_evals The number of trained models for each model type in `engine`.
#' @param nr_return_models The number of the best models to return, `all` value is possible.
#'
#' @return A list consisting of models created via random search and ranked list
#' of models scores.
#' @export
#'
#' @examples
#' data(iris)
#' iris_bin          <- iris[1:100, ]
#' type              <- guess_type(iris_bin, 'Species')
#' preprocessed_data <- preprocessing(iris_bin, 'Species')
#' preprocessed_data <- preprocessed_data$data
#' split_data <-
#'   train_test_balance(preprocessed_data,
#'                      'Species',
#'                      type = type,
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
#'
#' model <-
#'   train_models(train_data,
#'                'Species',
#'                engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                type = type)
#'
#' predictions <-
#'   predict_models(model,
#'                  test_data,
#'                  'Species',
#'                  engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
#'                  type = type)
#'
#' score <- score_models(model, predictions, test_data$ranger_data$Species, type)
#'
#' random_best <- random_search(train_data,
#'                              test_data,
#'                              y = 'Species',
#'                              engine = c('ranger', 'xgboost', 'decision_tree',
#'                              'lightgbm', 'catboost'),
#'                              type = type,
#'                              max_evals = 4,
#'                              nr_return_models = 'all')
random_search <- function(train_data,
                          y,
                          models = NULL,
                          engine,
                          type,
                          max_evals = 10) {
  if(max_evals <= 0){
    return(NULL)
  }

  ranger_grid <- list(
    num.trees     = list(50, 100, 200),
    mtry          = list(NULL),
    min.node.size = list(NULL, 2, 5),
    max.depth     = list(NULL, 2, 5)
  )

  xgboost_grid <- list(
    booster   = list('gbtree', 'dart'),
    eta       = list(0.1, 0.3, 0.5),
    max_depth = list(2, 6, 20)
  )

  tree_grid <- list(
    minsplit     = c(3, 10, 20),
    minprob      = c(0.01, 0.1),
    maxsurrogate = c(0, 1, 2),
    maxdepth     = c(5, 10, Inf),
    nresample    = c(100L, 9999L)
  )

  lightgbm_grid <- list(
    learning_rate  = c(0.05, 0.1, 0.2),
    num_leaves     = c(7, 15, 31),
    num_iterations = c(20, 100, 300)
  )

  catboost_grid <- list(
    iterations    = c(20, 100, 300),
    border_count  = c(1, 10, 100),
    depth         = c(4, 7, 10),
    learning_rate = c(0.05, 0.1, 0.3)
  )

  search_models <- list()
  search_engine <- c()
  if ('ranger' %in% engine) {
    if (type == 'regression') {
      classification <- NULL
      probability    <- FALSE
    } else if (type == 'binary_clf') {
      classification <- TRUE
      probability    <- TRUE
    }
    ranger_models        <- list()
    expanded_ranger_grid <- expand.grid(ranger_grid)
    max_ranger_evals     <- min(max_evals, dim(expanded_ranger_grid)[1])
    sample_ranger_grid   <- expanded_ranger_grid[sample(1:dim(expanded_ranger_grid)[1], max_ranger_evals), ]

    for (i in 1:max_ranger_evals) {
      ranger_models[i] <-
        list(
          ranger::ranger(
            data = train_data$ranger_data,
            dependent.variable.name = y,
            num.trees = unlist(sample_ranger_grid[i, 'num.trees']),
            mtry = unlist(sample_ranger_grid[i, 'try']),
            min.node.size = unlist(sample_ranger_grid[i, 'min.node.size']),
            max.depth = unlist(sample_ranger_grid[i, 'max.depth']),
            classification = classification,
            probability = probability
          )
        )
    }
    names(ranger_models) <- paste('ranger_RS_', 1:max_ranger_evals, sep = '')
    search_models        <- c(search_models, ranger_models)
    search_engine        <- c(search_engine, rep('ranger', max_ranger_evals))
  }
  if ('xgboost' %in% engine) {
    if (type == 'regression') {
      objective <- 'reg:squarederror'
      label     <- as.vector(train_data$ranger_data[[y]])
    } else if (type == 'binary_clf') {
      objective <- 'binary:logistic'
      label     <- as.vector(train_data$ranger_data[[y]] - 1)
    }
    xgboost_models        <- list()
    expanded_xgboost_grid <- expand.grid(xgboost_grid)
    max_xgboost_evals     <- min(max_evals, dim(expanded_xgboost_grid)[1])
    sample_xgboost_grid   <- expanded_xgboost_grid[sample(1:dim(expanded_xgboost_grid)[1], max_xgboost_evals), ]

    for (i in 1:max_xgboost_evals) {
      xgboost_models[i] <-
        list(
          xgboost::xgboost(
            data = train_data$xgboost_data,
            label = label,
            objective = objective,
            nrounds = 2,
            verbose = 0,
            booster = unlist(sample_xgboost_grid[i, 'booster']),
            eta = unlist(sample_xgboost_grid[i, 'eta']),
            max_depth = unlist(sample_xgboost_grid[i, 'max_depth'])
          )
        )
    }
    names(xgboost_models) <- paste('xgboost_RS_', 1:max_xgboost_evals, sep = '')
    search_models         <- c(search_models, xgboost_models)
    search_engine         <- c(search_engine, rep('xgboost', max_xgboost_evals))
  }
  if ('decision_tree' %in% engine) {
    tree_models        <- list()
    expanded_tree_grid <- expand.grid(tree_grid)
    max_tree_evals     <- min(max_evals, dim(expanded_tree_grid)[1])
    sample_tree_grid   <- expanded_tree_grid[sample(1:dim(expanded_tree_grid)[1], max_tree_evals), ]

    form = as.formula(paste0(y, ' ~.'))
    for (i in 1:max_tree_evals) {
      tree_contr <-
        partykit::ctree_control(
          minsplit = unlist(sample_tree_grid[i, 'minsplit']),
          minprob = unlist(sample_tree_grid[i, 'minprob']),
          maxsurrogate = unlist(sample_tree_grid[i, 'maxsurrogate']),
          maxdepth = unlist(sample_tree_grid[i, 'maxdepth']),
          nresample = unlist(sample_tree_grid[i, 'nresample'])
        )
      tree_models[i] <- list(partykit::ctree(
        formula = form,
        data = train_data$decision_tree_data
      ))
    }
    names(tree_models) <-
      paste('decision_tree_RS_', 1:max_tree_evals, sep = '')
    search_models      <- c(search_models, tree_models)
    search_engine      <- c(search_engine, rep('decision_tree', max_tree_evals))
  }
  if ('lightgbm' %in% engine) {
    lightgbm_models        <- list()
    expanded_lightgbm_grid <- expand.grid(lightgbm_grid)
    max_lightgbm_evals     <- min(max_evals, dim(expanded_lightgbm_grid)[1])
    sample_lightgbm_grid   <- expanded_lightgbm_grid[sample(1:dim(expanded_lightgbm_grid)[1], max_lightgbm_evals), ]

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

    for (i in 1:max_lightgbm_evals) {
      parameters <- c(
        params,
        list(
          learning_rate  = unlist(sample_lightgbm_grid[i, 'learning_rate']),
          num_leaves     = unlist(sample_lightgbm_grid[i, 'num_leaves']),
          num_iterations = unlist(sample_lightgbm_grid[i, 'num_iterations'])
        )
      )
      lightgbm_models[i] <-
        list(
          lightgbm::lgb.train(
            params = parameters,
            data = train_data$lightgbm_data,
            verbose = -1
          )
        )

    }
    names(lightgbm_models) <-
      paste('lightgbm_RS_', 1:max_lightgbm_evals, sep = '')
    search_models          <- c(search_models, lightgbm_models)
    search_engine          <- c(search_engine, rep('lightgbm', max_lightgbm_evals))
  }
  if ('catboost' %in% engine) {
    catboost_models        <- list()
    expanded_catboost_grid <- expand.grid(catboost_grid)
    max_catboost_evals     <- min(max_evals, dim(expanded_catboost_grid)[1])
    sample_catboost_grid   <- expanded_catboost_grid[sample(1:dim(expanded_catboost_grid)[1], max_catboost_evals), ]

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

    for (i in 1:max_catboost_evals) {
      parameters <- c(
        params,
        list(
          iterations    = unlist(sample_catboost_grid[i, 'iterations']),
          border_count  = unlist(sample_catboost_grid[i, 'border_count']),
          depth         = unlist(sample_catboost_grid[i, 'depth']),
          learning_rate = unlist(sample_catboost_grid[i, 'learning_rate'])
        )
      )
      capture.output(catboost_models[i] <- list(catboost::catboost.train(train_data$catboost_data, params = parameters)))
    }
    names(catboost_models) <- paste('catboost_RS_', 1:max_catboost_evals, sep = '')
    search_models          <- c(search_models, catboost_models)
    search_engine          <- c(search_engine, rep('catboost', max_catboost_evals))
  }

  return(list(
      models = search_models,
      engine = search_engine
      ))
}
