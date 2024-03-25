test_that('test-train-split-and-prepare-data', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_custom_prep.RData', '/testing_data_custom_prep.RData',
               '/iris_custom_prep.RData',   '/compas_custom_prep.RData',
               '/lisbon_prep.RData', '/testing_data_prep.RData',
               '/iris_prep.RData',   '/compas_prep.RData')
  targets <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types   <- c('regression', 'regression', 'multiclass', 'binary_clf')
  tryCatch({ # For CRAN, we need to omit catboost, as it is not there.
    find.package('catboost')
    engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
    classes <- c("data.frame", "matrix", "data.frame", "lgb.Dataset", "catboost.Pool")
  },
  error = function(cond) {
    engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm')
    classes <- c("data.frame", "matrix", "data.frame", "lgb.Dataset")
  })

  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  custom_data <- list(lisbon_custom_prep, testing_data_custom_prep, iris_custom_prep, compas_custom_prep)
  #prep_data   <- list(lisbon_prep, testing_data_prep, iris_prep, compas_prep)

  # Input tests for train_test_balance.
  ok_fractions  <- list(c(0.6, 0.2, 0.2), c(0.5, 0.25, 0.25), c(0.4, 0.3, 0.3))
  bad_fractions <- list(1, c('0.2', '0.4', '0.4'), c(0.4, 0.6), c(0.4, 0.2, 0.2), c(-0.5, 0.5, 1))
  for (fraction in ok_fractions) {
    expect_no_error(train_test_balance(data = custom_data[[3]]$data,
                                       y    = targets[3],
                                       balance   = TRUE,
                                       fractions = fraction,
                                       seed      = NULL))
  }
  for (fraction in bad_fractions) {
    expect_error(train_test_balance(data = custom_data[[3]]$data,
                                    y    = targets[3],
                                    balance   = TRUE,
                                    fractions = fraction,
                                    seed      = NULL))
  }
  bad_seeds <- c(1.5, '1.5')
  for (seed in bad_seeds) {
    expect_error(train_test_balance(data = custom_data[[3]]$data,
                                    y    = targets[3],
                                    balance   = TRUE,
                                    fractions = c(0.6, 0.2, 0.2),
                                    seed      = seed))
  }


  # Split and prepare data tests for all tasks
  train_datas <- list()
  test_datas  <- list()
  valid_datas <- list()
  raw_trains  <- list()
  for (i in 1:length(custom_data)) {
    data <- custom_data[[i]]
    split_data <- train_test_balance(data = data$data,
                                     y    = targets[i],
                                     balance   = TRUE,
                                     fractions = c(0.6, 0.2, 0.2),
                                     seed      = 8)

    # train_observed <- split_data$train[[targets[i]]]
    # test_observed  <- split_data$test[[targets[i]]]
    # valid_observed <- split_data$valid[[targets[i]]]

    expect_equal(round(nrow(split_data$train) / nrow(data$data), 1), 0.6)
    expect_equal(round(nrow(split_data$test)  / nrow(data$data), 1), 0.2)
    expect_equal(round(nrow(split_data$valid) / nrow(data$data), 1), 0.2)

    train_data <- prepare_data(data    = split_data$train,
                               type    = types[i],
                               y       = targets[i],
                               engine  = engine)
    test_data  <- prepare_data(data    = split_data$test,
                               type    = types[i],
                               y       = targets[i],
                               engine  = engine,
                               predict = TRUE,
                               train   = split_data$train)
    valid_data <- prepare_data(data    = split_data$valid,
                               type    = types[i],
                               y       = targets[i],
                               engine  = engine,
                               predict = TRUE,
                               train   = split_data$train)
    raw_train  <- prepare_data(data    = split_data$train,
                               type    = types[i],
                               y       = targets[i],
                               engine  = engine,
                               predict = TRUE,
                               train   = split_data$train)

    train_datas[[i]] <- train_data
    test_datas[[i]]  <- test_data
    valid_datas[[i]] <- valid_data
    raw_trains[[i]]  <- raw_train
    # Are the outputs long enough.
    expect_true(length(train_data) == 5)
    expect_true(length(test_data)  == 5)
    expect_true(length(valid_data) == 5)
    expect_true(length(raw_train)  == 5)

    # Are there all rows preserved.
    for (j in c(1, 2, 3, 5)) {
      expect_equal(nrow(train_data[[j]]), nrow(split_data$train))
      expect_equal(nrow(test_data[[j]]),  nrow(split_data$test))
      expect_equal(nrow(valid_data[[j]]), nrow(split_data$valid))
      expect_equal(nrow(raw_train[[j]]),  nrow(split_data$train))
    }
    # Are there correct classes created.
    for (j in c(1, 2, 3, 5)) {
      expect_true(classes[j] %in% class(train_data[[j]]))
      expect_true(classes[j] %in% class(test_data[[j]]))
      expect_true(classes[j] %in% class(valid_data[[j]]))
      expect_true(classes[j] %in% class(raw_train[[j]]))
    }

    # Are proper levels created (especially for xgboost).
    for (j in c(1, 2, 3)) {
      for (k in 1:ncol(test_data[[j]])) {
        expect_true(all(levels(test_data[[j]][[k]])  %in% levels(train_data[[j]][[k]])))
      }
      for (k in 1:ncol(valid_data[[j]])) {
        expect_true(all(levels(valid_data[[j]][[k]]) %in% levels(train_data[[j]][[k]])))
      }
    }
  }
  lisbon_prepared_data       <- list(train_data = train_datas[[1]], test_data = test_datas[[1]],
                                     valid_data = valid_datas[[1]], raw_train = raw_trains[[1]])
  testing_data_prepared_data <- list(train_data = train_datas[[2]], test_data = test_datas[[2]],
                                     valid_data = valid_datas[[2]], raw_train = raw_trains[[2]])
  iris_prepared_data         <- list(train_data = train_datas[[3]], test_data = test_datas[[3]],
                                     valid_data = valid_datas[[3]], raw_train = raw_trains[[3]])
  compas_prepared_data       <- list(train_data = train_datas[[4]], test_data = test_datas[[4]],
                                     valid_data = valid_datas[[4]], raw_train = raw_trains[[4]])

  save(lisbon_prepared_data, file = capture.output(cat(folder, '/lisbon_prepared_data.RData', sep='')))
  save(testing_data_prepared_data, file = capture.output(cat(folder, '/testing_data_prepared_data.RData', sep='')))
  save(iris_prepared_data, file = capture.output(cat(folder, '/iris_prepared_data.RData', sep='')))
  save(compas_prepared_data, file = capture.output(cat(folder, '/compas_prepared_data.RData', sep='')))

})
