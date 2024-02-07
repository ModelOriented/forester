test_that('test-plot', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_train.RData', '/testing_data_train.RData',
               '/iris_train.RData',   '/compas_train.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  # No catboost due to the error with loading the learn_pool.
  engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  train_outputs <- list(lisbon_train, testing_data_train, iris_train, compas_train)
  plot_types  <- list(list('residuals', 'train-test-observed-predicted', 'train-test'),
                      list('residuals', 'train-test-observed-predicted', 'train-test'),
                      list('comparison', 'confusion-matrix', 'train-test'),
                      list('comparison', 'roc', 'confusion-matrix', 'train-test'))
  for (i in 1:length(train_outputs)) {
    for (type in plot_types[[i]]) {
      expect_no_error(plot(x = train_outputs[[i]], type = type))
    }
  }
})
