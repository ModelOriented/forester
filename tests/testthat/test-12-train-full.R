test_that('test-train-full', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_custom_prep.RData', '/testing_data_custom_prep.RData',
               '/iris_custom_prep.RData',   '/compas_custom_prep.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  custom_data <- list(lisbon_custom_prep, testing_data_custom_prep, iris_custom_prep, compas_custom_prep)
  trained     <- list()
  for (i in 1:length(custom_data)) {
    expect_no_error(
    trained[[i]] <- train(data             = custom_data[[i]]$data,
                          y                = targets[i],
                          type             = 'auto',
                          engine           = engine,
                          verbose          = FALSE,
                          train_test_split = c(0.6, 0.2, 0.2),
                          split_seed       = NULL,
                          bayes_iter       = 0,
                          random_evals     = 1,
                          metrics          = 'auto',
                          sort_by          = 'auto',
                          custom_preprocessing = custom_data[[i]]))
  }

  lisbon_train       <- trained[[1]]
  testing_data_train <- trained[[2]]
  iris_train         <- trained[[3]]
  compas_train       <- trained[[4]]

  save(lisbon_train, file = capture.output(cat(folder, '/lisbon_train.RData', sep='')))
  save(testing_data_train, file = capture.output(cat(folder, '/testing_data_train.RData', sep='')))
  save(iris_train, file = capture.output(cat(folder, '/iris_train.RData', sep='')))
  save(compas_train, file = capture.output(cat(folder, '/compas_train.RData', sep='')))
})
