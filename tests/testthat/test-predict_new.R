test_that('test-predict_new', {
  # lTests for lisbon dataset.
  data        <- lisbon
  y           <- 'Price'
  suppressWarnings(
    train_out <- train(data, y, verbose = FALSE, random_evals = 1, bayes_iter = 2)
  )

  new_obs              <- lisbon[1, ]
  new_obs['Condition'] <- 'BrandNew'
  suppressWarnings(
    preds <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 1)

  new_obs['Bedrooms']      <- NULL
  expect_error(predict_new(train_out, new_obs, verbose = FALSE))

  new_obs                  <- lisbon[1:30, ]
  new_obs['District'][1]   <- 'Havananananana'
  suppressWarnings(
    new_obs['Parish'][2]   <- 'PWMINI'
    )
  new_obs['Bedrooms'][3, ] <- NaN
  suppressWarnings(
    preds <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 30)

  # Tests for adult dataset.
  data        <- adult[1:100, ]
  y           <- 'salary'
  suppressWarnings(
    train_out <- train(data, y, verbose = FALSE, random_evals = 1, bayes_iter = 2)
  )

  new_obs              <- adult[30, ]
  new_obs['education'] <- 'XDDD'

  suppressWarnings(
    preds <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 1)

  new_obs['education'] <- NULL
  expect_error(predict_new(train_out, new_obs, verbose = FALSE))

  new_obs                            <- adult[1:30, ]
  new_obs['education'][1]            <- 'XDDD'
  suppressWarnings(new_obs['age'][2] <- 'PWMINI')
  new_obs['marital_status'][3, ]     <- NA

  suppressWarnings(
    preds <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 30)

  # Tests for testing_data.
  data        <- testing_data[1:100, ]
  y           <- 'y'
  suppressWarnings(
    train_out <- train(data, y, verbose = FALSE, random_evals = 1, bayes_iter = 2)
  )

  new_obs        <- testing_data[1, ]
  new_obs['X11'] <- 'XDDD'

  suppressWarnings(
    preds        <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 1)

  new_obs['X4']      <- NULL
  expect_error(predict_new(train_out, new_obs, verbose = FALSE))

  new_obs            <- testing_data[1:30, ]
  new_obs['X11'][1]  <- 'XDDD'
  new_obs['X7'][3, ] <- NA

  suppressWarnings(
    preds            <- predict_new(train_out, new_obs, verbose = FALSE)
  )

  expect_true(length(preds) == 15)
  expect_true(length(preds[[1]]) == 30)
  })
