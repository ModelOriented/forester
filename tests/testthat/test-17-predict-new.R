test_that('test-predict-new', {
  data     <- list(lisbon[1:200, ], testing_data[1:200, ], iris[1:110, ], compas[1:200, ])
  new_data <- list(lisbon[201:246, ], testing_data[201:250, ], iris[111:150, ], compas[201:250, ])
  targets  <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  tryCatch({ # For CRAN, we need to omit catboost, as it is not there.
    find.package('catboost')
    engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')
  },
  error = function(cond) {
    engine  <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm')
  })

  for (i in 1:length(data)) {
    output <- train(data             = data[[i]],
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
                    parallel         = FALSE,
                    custom_preprocessing = NULL)

    expect_no_error(
      predictions <- predict_new(train_out = output,
                                 data      = new_data[[i]],
                                 verbose   = FALSE))
    # Part of select models tests.
    expect_no_error(
      new_output <- select_models(train_output = output,
                                  model_names = names(output$models_list)[c(1, 2, 3, 4)]))
    expect_no_error(
      predictions_new <- predict_new(train_out = new_output,
                                 data      = new_data[[i]],
                                 verbose   = FALSE))

    expect_equal(length(predictions), 10)

    for (j in 1:(length(predictions) - 1)) {
      expect_equal(length(as.vector(predictions[[j]])), length(as.vector(predictions[[j + 1]])))
    }

  }
})
