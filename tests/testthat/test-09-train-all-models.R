test_that('test-train-all-models', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_prepared_data.RData', '/testing_data_prepared_data.RData',
               '/iris_prepared_data.RData',   '/compas_prepared_data.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  # No catboost due to the error with loading the learn_pool.
  engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  prepared_data <- list(lisbon_prepared_data, testing_data_prepared_data, iris_prepared_data, compas_prepared_data)
  models_basic  <- list()
  models_random <- list()
  models_bayes  <- list()

  # Train basic models.
  for (i in 1:length(prepared_data)) {
    models_basic[[i]] <- train_models(data   = prepared_data[[i]]$train_data,
                                      y      = targets[i],
                                      engine = engine,
                                      type   = types[i])
  }
  for (i in 1:length(models_basic)) {
    expect_equal(length(models_basic[[i]]), 4)
    expect_equal(length(unique(names(models_basic[[i]]))), 4)
    expect_true('ranger' %in% class(models_basic[[i]]$ranger_model))
    expect_true('xgb.Booster' %in% class(models_basic[[i]]$xgboost_model))
    expect_true('constparty'  %in% class(models_basic[[i]]$decision_tree_model))
    expect_true('lgb.Booster' %in% class(models_basic[[i]]$lightgbm_model))
  }


  # Train random search models
  for (i in 1:length(prepared_data)) {
    capture.output(suppressWarnings(suppressMessages(
    models_random[[i]] <- random_search(train_data = prepared_data[[i]]$train_data,
                                        y          = targets[i],
                                        engine     = engine,
                                        type       = types[i],
                                        max_evals  = 4,
                                        verbose    = FALSE))))
  }
  for (i in 1:length(models_random)) {
    expect_equal(length(models_random[[i]]$models), 16)
    expect_equal(length(models_random[[i]]$engine), 16)
    expect_equal(length(unique(names(models_random[[i]]$models))), 16)
    expect_true('ranger' %in% class(models_random[[i]]$models$ranger_RS_1))
    expect_true('xgb.Booster' %in% class(models_random[[i]]$models$xgboost_RS_1))
    expect_true('constparty'  %in% class(models_random[[i]]$models$decision_tree_RS_1))
    expect_true('lgb.Booster' %in% class(models_random[[i]]$models$lightgbm_RS_1))
  }


  # Train models with Bayesian optimization.
  for (i in 1:length(prepared_data)) {
    capture.output(suppressWarnings(suppressMessages(
    models_bayes[[i]] <- train_models_bayesopt(train_data = prepared_data[[i]]$train_data,
                                               y          = targets[i],
                                               test_data  = prepared_data[[i]]$test_data,
                                               engine     = engine,
                                               type       = types[i],
                                               iters.n    = 2,
                                               verbose    = FALSE))))
  }
  for (i in 1:length(models_bayes)) {
    expect_equal(length(models_bayes[[i]]), 4)
    expect_equal(length(models_bayes[[i]]), 4)
    expect_equal(length(unique(names(models_bayes[[i]]))), 4)
    expect_true('ranger' %in% class(models_bayes[[i]]$ranger_bayes))
    expect_true('xgb.Booster' %in% class(models_bayes[[i]]$xgboost_bayes))
    expect_true('constparty'  %in% class(models_bayes[[i]]$decision_tree_bayes))
    expect_true('lgb.Booster' %in% class(models_bayes[[i]]$lightgbm_bayes))
  }


  models_all <- list()
  engine_all <- list()

  for (i in 1:length(models_bayes)) {
    models_all[[i]] <- c(models_basic[[i]], models_random[[i]]$models, models_bayes[[i]])
    engine_all[[i]] <- c(engine, models_random[[i]]$engine, engine)
    expect_equal(length(models_all[[i]]), 24)
    expect_equal(length(engine_all[[i]]), 24)
  }


  tuning <- c(rep('basic', length(engine)),
              rep('random_search', length(models_random[[i]]$engine)),
              rep('bayes_opt', length(engine)))
  expect_equal(length(tuning), 24)

  lisbon_models       <- list(models_basic = models_basic[[1]], models_random = models_random[[1]],
                              models_bayes = models_bayes[[1]], models_all    = models_all[[1]],
                              engine_all   = engine_all[[1]])
  testing_data_models <- list(models_basic = models_basic[[2]], models_random = models_random[[2]],
                              models_bayes = models_bayes[[2]], models_all    = models_all[[2]],
                              engine_all   = engine_all[[2]])
  iris_models         <- list(models_basic = models_basic[[3]], models_random = models_random[[3]],
                              models_bayes = models_bayes[[3]], models_all    = models_all[[3]],
                              engine_all   = engine_all[[3]])
  compas_models       <- list(models_basic = models_basic[[4]], models_random = models_random[[4]],
                              models_bayes = models_bayes[[4]], models_all    = models_all[[4]],
                              engine_all   = engine_all[[4]])


  save(lisbon_models, file = capture.output(cat(folder, '/lisbon_models.RData', sep='')))
  save(testing_data_models, file = capture.output(cat(folder, '/testing_data_models.RData', sep='')))
  save(iris_models, file = capture.output(cat(folder, '/iris_models.RData', sep='')))
  save(compas_models, file = capture.output(cat(folder, '/compas_models.RData', sep='')))
  save(tuning, file = capture.output(cat(folder, '/tuning.RData', sep='')))

})
