test_that('test-predict-new', {
  data     <- list(lisbon[1:200, ], testing_data[1:200, ], iris[1:110, ], compas[1:200, ])
  new_data <- list(lisbon[201:246, ], testing_data[201:250, ], iris[111:150, ], compas[201:250, ])
  targets  <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  engine   <- c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost')

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
                    custom_preprocessing = NULL)

    expect_no_error(
      predictions <- predict_new(train_out = output,
                                 data      = new_data[[i]],
                                 verbose   = FALSE))

    expect_equal(length(predictions), 10)

    for (j in 1:(length(predictions) - 1)) {
      expect_equal(length(as.vector(predictions[[j]])), length(as.vector(predictions[[j + 1]])))
    }

  }
})
