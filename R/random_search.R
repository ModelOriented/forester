#' Random optimization of hyperparameters
#'
#' @param train_data A training data for models created by `prepare_data()` function.
#' @param y A string that indicates a target column name for regression or classification.
#' Either y, or pair: time, status can be used.
#' @param time A string that indicates a time column name for survival analysis task.
#' Either y, or pair: time, status can be used.
#' @param status A string that indicates a status column name for survival analysis task.
#' Either y, or pair: time, status can be used.
#' @param engine A vector of tree-based models that shall be created. Possible
#' values are: `ranger`, `xgboost`,`decision_tree`, `lightgbm`, `catboost`. Doesn't
#' matter for survival analysis.
#' @param type A string that determines if Machine Learning task is the
#' `binary_clf`, `regression`, `survival`, or `multiclass` task.
#' @param max_evals The number of trained models for each model type in `engine`.
#' @param parallel A logical value, if set to TRUE, the function will use parallel computing.
#' By default set to FALSE.
#' @param verbose A logical value, if set to TRUE, provides all information about
#' the process, if FALSE gives none. Set to FALSE by default.
#'
#' @return A list consisting of models created via random search and ranked list
#' of models scores.
#' @export
random_search <- function(train_data,
                          y      = NULL,
                          time   = NULL,
                          status = NULL,
                          engine,
                          type,
                          max_evals = 10,
                          parallel  = FALSE,
                          verbose   = FALSE) {
  if (!is.numeric(max_evals) | as.integer(max_evals) != max_evals ) {
    verbose_cat(crayon::green('\u2714'), 'The number of random search evaluations must be an integer. \n\n', verbose = verbose)
    stop('The number of bayesian optimization iterations must be an integer.')
  }
  if (max_evals <= 0) {
    verbose_cat(crayon::green('\u2714'), 'Random search was turned off. \n', verbose = verbose)
    return(NULL)
  }

  ranger_grid <- list(
    num.trees       = c(5, 100, 1000),
    min.node.size   = c(1, 4, 10),
    max.depth       = c(1, 10, 100),
    sample.fraction = c(0.25, 0.5, 0.75)
  )

  xgboost_grid <- list(
    nrounds   = c(5, 20, 100),
    eta       = c(0.01, 0.1, 5),
    subsample = c(0.7, 0.9, 1),
    gamma     = c(0, 1, 10),
    max_depth = c(1, 3, 10)
  )

  tree_grid <- list(
    minsplit     = c(3, 10, 20),
    minprob      = c(0.01, 0.1),
    maxsurrogate = c(0, 1, 2),
    maxdepth     = c(5, 10, Inf),
    nresample    = c(100, 9999)
  )

  lightgbm_grid <- list(
    learning_rate  = c(0.01, 0.1, 0.5),
    num_leaves     = c(2, 25, 50),
    num_iterations = c(5, 20, 100)
  )

  catboost_grid <- list(
    iterations       = c(5, 20, 100),
    border_count     = c(64, 256, 1024),
    depth            = c(2, 8, 16),
    learning_rate    = c(0.01, 0.1, 0.5),
    min_data_in_leaf = c(1, 3, 10)
  )

  rfsrc_grid <- list(
    ntree    = c(50, 500, 1000),
    nodesize = c(5, 15, 30),
    nsplit   = c(1, 10, 100)
  )

  search_models <- list()
  search_engine <- c()
  # Survival analysis.
  if (is.null(y)) {
    verbose_cat('   ', crayon::green('\u2714'), 'rfsrc: Starting training procedure.\n', verbose = verbose)
    t0 <- as.numeric(Sys.time())
    rfsrc_models        <- list()
    expanded_rfsrc_grid <- expand.grid(rfsrc_grid)
    max_rfsrc_evals     <- min(max_evals, dim(expanded_rfsrc_grid)[1])
    sample_rfsrc_grid   <- expanded_rfsrc_grid[sample(1:dim(expanded_rfsrc_grid)[1], max_rfsrc_evals), ]

    for (i in 1:max_rfsrc_evals) {
      rfsrc_models[i] <-
        list(
          randomForestSRC::rfsrc(
            formula   = as.formula(paste0('Surv(',time,',', status,') ~ .')),
            # We use ranger data, as rfsrc doesn't need preprocessing.
            data      = train_data$ranger_data,
            na.action = 'na.omit',
            ntree     = unlist(sample_rfsrc_grid[i, 'ntree']),
            nodesize  = unlist(sample_rfsrc_grid[i, 'nodesize']),
            nsplit    = unlist(sample_rfsrc_grid[i, 'nsplit']),
            splitrule = 'logrankscore'
          )
        )
    }
    names(rfsrc_models) <- paste('rfsrc_RS_', 1:max_rfsrc_evals, sep = '')
    search_models       <- c(search_models, rfsrc_models)
    search_engine       <- c(search_engine, rep('rfsrc', max_rfsrc_evals))
    verbose_cat('   ', crayon::green('\u2714'), 'rfsrc: Tuning with random search was successful!\n', verbose = verbose)
    t1 <- as.numeric(Sys.time())
    verbose_cat('   ', crayon::green('\u2714'), 'rfsrc: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)

    return(list(
      models = search_models,
      engine = search_engine
    ))
  # Regression and classification.
  } else {
    if ('ranger' %in% engine) {
      verbose_cat('   ', crayon::green('\u2714'), 'ranger: Starting training procedure.\n', verbose = verbose)
      t0 <- as.numeric(Sys.time())
      if (type == 'regression') {
        classification <- NULL
        probability    <- FALSE
      } else if (type %in% c('binary_clf', 'multiclass')) {
        classification <- TRUE
        probability    <- TRUE
      }
      ranger_models        <- list()
      expanded_ranger_grid <- expand.grid(ranger_grid)
      max_ranger_evals     <- min(max_evals, dim(expanded_ranger_grid)[1])
      sample_ranger_grid   <- expanded_ranger_grid[sample(1:dim(expanded_ranger_grid)[1], max_ranger_evals), ]

      train_ranger <- function(i) {
        return(ranger::ranger(
            data            = train_data$ranger_data,
            dependent.variable.name = y,
            num.trees       = unlist(sample_ranger_grid[i, 'num.trees']),
            sample.fraction = unlist(sample_ranger_grid[i, 'sample.fraction']),
            min.node.size   = unlist(sample_ranger_grid[i, 'min.node.size']),
            max.depth       = unlist(sample_ranger_grid[i, 'max.depth']),
            classification  = classification,
            probability     = probability
          ))
      }
      if (parallel) {
        ranger_models <- foreach::`%dopar%`(foreach::foreach(i = 1:max_ranger_evals), train_ranger(i))
      } else {
        for (i in 1:max_ranger_evals) {
          ranger_models[i] <- list(train_ranger(i))
        }
      }
      names(ranger_models) <- paste('ranger_RS_', 1:max_ranger_evals, sep = '')
      search_models        <- c(search_models, ranger_models)
      search_engine        <- c(search_engine, rep('ranger', max_ranger_evals))
      verbose_cat('   ', crayon::green('\u2714'), 'ranger: Tuning with random search was successful!\n', verbose = verbose)
      t1 <- as.numeric(Sys.time())
      verbose_cat('   ', crayon::green('\u2714'), 'ranger: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)
    }
    if ('xgboost' %in% engine) {
      verbose_cat('   ', crayon::green('\u2714'), 'xgboost: Starting training procedure.\n', verbose = verbose)
      t0 <- as.numeric(Sys.time())
      if (type == 'regression') {
        objective   <- 'reg:squarederror'
        eval_metric <- 'rmse'
        label       <- as.vector(train_data$ranger_data[[y]])
        params      <- list(objective = objective, eval_metric = eval_metric)
      } else if (type == 'binary_clf') {
        if (any(train_data$ranger_data[[y]] == 2)) {
          label <- as.numeric(train_data$ranger_data[[y]]) - 1
        } else {
          label <- as.numeric(train_data$ranger_data[[y]])
        }
        objective   <- 'binary:logistic'
        eval_metric <- 'auc'
        label       <- as.vector(label)
        num_class   <- 1
        params      <- list(objective = objective, eval_metric = eval_metric, num_class = num_class)
      } else if (type == 'multiclass') {
        if (any(train_data$ranger_data[[y]] == 2)) {
          label <- as.numeric(train_data$ranger_data[[y]]) - 1
        } else {
          label <- as.numeric(train_data$ranger_data[[y]])
        }
        objective   <- 'multi:softprob'
        eval_metric <- 'merror'
        label       <- as.vector(label)
        num_class   <- length(unique(as.vector(label)))
        params      <- list(objective = objective, eval_metric = eval_metric, num_class = num_class)
      }
      xgboost_models        <- list()
      expanded_xgboost_grid <- expand.grid(xgboost_grid)
      max_xgboost_evals     <- min(max_evals, dim(expanded_xgboost_grid)[1])
      sample_xgboost_grid   <- expanded_xgboost_grid[sample(1:dim(expanded_xgboost_grid)[1], max_xgboost_evals), ]

      train_xgboost <- function(i) {
        return(
          xgboost::xgboost(
            data        = train_data$xgboost_data,
            label       = label,
            verbose     = 0,
            params      = params,
            nrounds     = unlist(sample_xgboost_grid[i, 'nrounds']),
            subsample   = unlist(sample_xgboost_grid[i, 'subsample']),
            gamma       = unlist(sample_xgboost_grid[i, 'gamma']),
            eta         = unlist(sample_xgboost_grid[i, 'eta']),
            max_depth   = unlist(sample_xgboost_grid[i, 'max_depth'])
          )
        )
      }
      if (parallel) {
        xgboost_models <- foreach::`%dopar%`(foreach::foreach(i = 1:max_xgboost_evals), train_xgboost(i))
      } else {
        for (i in 1:max_xgboost_evals) {
          xgboost_models[i] <- list(train_xgboost(i))
        }
      }

      names(xgboost_models) <- paste('xgboost_RS_', 1:max_xgboost_evals, sep = '')
      search_models         <- c(search_models, xgboost_models)
      search_engine         <- c(search_engine, rep('xgboost', max_xgboost_evals))
      verbose_cat('   ', crayon::green('\u2714'), 'xgboost: Tuning with random search was successful!\n', verbose = verbose)
      t1 <- as.numeric(Sys.time())
      verbose_cat('   ', crayon::green('\u2714'), 'xgboost: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)
    }
    if ('decision_tree' %in% engine) {
      verbose_cat('   ', crayon::green('\u2714'), 'decision_tree: Starting training procedure.\n', verbose = verbose)
      t0 <- as.numeric(Sys.time())
      tree_models        <- list()
      expanded_tree_grid <- expand.grid(tree_grid)
      max_tree_evals     <- min(max_evals, dim(expanded_tree_grid)[1])
      sample_tree_grid   <- expanded_tree_grid[sample(1:dim(expanded_tree_grid)[1], max_tree_evals), ]
      form               <- as.formula(paste0(y, ' ~.'))

      train_tree <- function(i) {
        tree_contr <- partykit::ctree_control(
          minsplit  = unlist(sample_tree_grid[i, 'minsplit']),
          minprob   = unlist(sample_tree_grid[i, 'minprob']),
          maxdepth  = unlist(sample_tree_grid[i, 'maxdepth']),
          nresample = unlist(sample_tree_grid[i, 'nresample'])
        )
        return(
          partykit::ctree(
            formula = form,
            data    = train_data$decision_tree_data
          )
        )
      }
      if (parallel) {
        # It makes it slower instead of faster.
        #tree_models <- foreach::`%dopar%`(foreach::foreach(i = 1:max_tree_evals), train_tree(i))
        for (i in 1:max_tree_evals) {
          tree_models[i] <- list(train_tree(i))
        }
      } else {
        for (i in 1:max_tree_evals) {
          tree_models[i] <- list(train_tree(i))
        }
      }

      names(tree_models) <- paste('decision_tree_RS_', 1:max_tree_evals, sep = '')
      search_models      <- c(search_models, tree_models)
      search_engine      <- c(search_engine, rep('decision_tree', max_tree_evals))
      verbose_cat('   ', crayon::green('\u2714'), 'decision_tree: Tuning with random search was successful!\n', verbose = verbose)
      t1 <- as.numeric(Sys.time())
      verbose_cat('   ', crayon::green('\u2714'), 'decision_tree: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)
    }
    if ('lightgbm' %in% engine) {
      verbose_cat('   ', crayon::green('\u2714'), 'lightgbm: Starting training procedure.\n', verbose = verbose)
      t0 <- as.numeric(Sys.time())
      lightgbm_models        <- list()
      expanded_lightgbm_grid <- expand.grid(lightgbm_grid)
      max_lightgbm_evals     <- min(max_evals, dim(expanded_lightgbm_grid)[1])
      sample_lightgbm_grid   <- expanded_lightgbm_grid[sample(1:dim(expanded_lightgbm_grid)[1], max_lightgbm_evals), ]

      if (type == 'binary_clf') {
        obj = 'binary'
        params <- list(objective = obj)
      } else if (type == 'multiclass') {
        obj = 'multiclass'
        params <- list(objective = obj, num_class = length(unique(as.vector(train_data$ranger_data[[y]]))))
      } else if (type == 'regression') {
        obj = 'regression'
        params <- list(objective = obj)
      }

      train_lightgbm <- function(i) {
        parameters <- c(
          params,
          list(
            learning_rate  = unlist(sample_lightgbm_grid[i, 'learning_rate']),
            num_leaves     = unlist(sample_lightgbm_grid[i, 'num_leaves']),
            num_iterations = unlist(sample_lightgbm_grid[i, 'num_iterations']),
            num_threds     = (parallel::detectCores()-2)/2
          )
        )
        return(
          lightgbm::lightgbm(
            params  = parameters,
            data    = train_data$lightgbm_data,
            verbose = -1
          )
        )
      }
      if (parallel) {
        # Does not work properly
        lightgbm_models <- foreach::`%do%`(foreach::foreach(i = 1:max_lightgbm_evals), train_lightgbm(i))
      } else {
        for (i in 1:max_lightgbm_evals) {
          lightgbm_models[i] <- list(train_lightgbm(i))
        }
      }

      names(lightgbm_models) <- paste('lightgbm_RS_', 1:max_lightgbm_evals, sep = '')
      search_models          <- c(search_models, lightgbm_models)
      search_engine          <- c(search_engine, rep('lightgbm', max_lightgbm_evals))
      verbose_cat('   ', crayon::green('\u2714'), 'lightgbm: Tuning with random search was successful!\n', verbose = verbose)
      t1 <- as.numeric(Sys.time())
      verbose_cat('   ', crayon::green('\u2714'), 'lightgbm: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)
    }
    if ('catboost' %in% engine) {
      verbose_cat('   ', crayon::green('\u2714'), 'catboost: Starting training procedure.\n', verbose = verbose)
      t0 <- as.numeric(Sys.time())
      catboost_models        <- list()
      expanded_catboost_grid <- expand.grid(catboost_grid)
      max_catboost_evals     <- min(max_evals, dim(expanded_catboost_grid)[1])
      sample_catboost_grid   <- expanded_catboost_grid[sample(1:dim(expanded_catboost_grid)[1], max_catboost_evals), ]

      if (type == 'binary_clf') {
        obj    <- 'Logloss'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'multiclass') {
        obj    <- 'MultiClass'
        params <- list(loss_function = obj, logging_level = 'Silent')
      } else if (type == 'regression') {
        obj    <- 'MAE'
        params <- list(loss_function = obj, logging_level = 'Silent')
      }

      train_catboost <- function(i, data) {
        parameters <- c(
          params,
          list(
            iterations    = unlist(sample_catboost_grid[i, 'iterations']),
            border_count  = unlist(sample_catboost_grid[i, 'border_count']),
            depth         = unlist(sample_catboost_grid[i, 'depth']),
            learning_rate = unlist(sample_catboost_grid[i, 'learning_rate'])
          )
        )
        return(
          catboost::catboost.train(data, params = parameters)
        )
      }
      if (parallel) {
        catboost_models <- foreach::`%do%`(foreach::foreach(i = 1:max_catboost_evals), train_catboost(i, data = train_data$catboost_data))
      } else {
        for (i in 1:max_catboost_evals) {
          capture.output(catboost_models[i] <- list(train_catboost(i, data = train_data$catboost_data)))
        }
      }

      names(catboost_models) <- paste('catboost_RS_', 1:max_catboost_evals, sep = '')
      search_models          <- c(search_models, catboost_models)
      search_engine          <- c(search_engine, rep('catboost', max_catboost_evals))
      verbose_cat('   ', crayon::green('\u2714'), 'catboost: Tuning with random search was successful!\n', verbose = verbose)
      t1 <- as.numeric(Sys.time())
      verbose_cat('   ', crayon::green('\u2714'), 'catboost: It took', round(t1 - t0, 2), 'seconds. \n', verbose = verbose)
    }

    return(list(
      models = search_models,
      engine = search_engine
    ))
  }

}
