test_that('test-train', {
  set.seed(123)
  df_iris   <- iris[1:100, ]
  df_lisbon <- lisbon
  df_compas <- compas[1:100, ]
  df_adult  <- adult[1:100, ]
  df_test   <- testing_data[1:100, ]

  y_iris   <- 'Species'
  y_lisbon <- 'Price'
  y_compas <- 'Two_yr_Recidivism'
  y_adult  <- 'salary'
  y_test   <- 'y'

  suppressWarnings(output_iris   <- train(df_iris, y_iris, verbose = FALSE, random_iter = 2, bayes_iter = 1, advanced_preprocessing = FALSE))
  suppressWarnings(output_lisbon <- train(df_lisbon, y_lisbon, verbose = FALSE, random_iter = 2, bayes_iter = 1, advanced_preprocessing = FALSE))
  suppressWarnings(output_compas <- train(df_compas, y_compas, verbose = FALSE, random_iter = 2, bayes_iter = 1, advanced_preprocessing = FALSE))
  suppressWarnings(output_adult  <- train(df_adult, y_adult, verbose = FALSE, random_iter = 2, bayes_iter = 1, advanced_preprocessing = FALSE))
  suppressWarnings(output_test   <- train(df_test, y_test, verbose = FALSE, random_iter = 2, bayes_iter = 1, advanced_preprocessing = FALSE))

  bin <- 'binary_clf'
  reg <- 'regression'
  expect_true(output_iris$type == bin)
  expect_true(output_lisbon$type == reg)
  expect_true(output_compas$type == bin)
  expect_true(output_adult$type == bin)
  expect_true(output_test$type == reg)

  expect_true(is.null(output_iris$deleted_columns))
  expect_true(length(output_lisbon$deleted_columns) == 3)
  expect_true(is.null(output_compas$deleted_columns))
  expect_true(is.null(output_adult$deleted_columns))
  expect_true(output_test$deleted_columns == 'X3')

  expect_equal(dim(output_iris$preprocessed_data), c(100, 5))
  expect_equal(dim(output_lisbon$preprocessed_data), c(246, 14))
  expect_equal(dim(output_compas$preprocessed_data), c(100, 7))
  expect_equal(dim(output_adult$preprocessed_data), c(100, 15))
  expect_equal(dim(output_test$preprocessed_data), c(100, 11))

  expect_false(list(NULL) %in% output_iris$train_data)
  expect_false(list(NULL) %in% output_lisbon$train_data)
  expect_false(list(NULL) %in% output_compas$train_data)
  expect_false(list(NULL) %in% output_adult$train_data)
  expect_false(list(NULL) %in% output_test$train_data)

  expect_false(list(NULL) %in% output_iris$test_data)
  expect_false(list(NULL) %in% output_lisbon$test_data)
  expect_false(list(NULL) %in% output_compas$test_data)
  expect_false(list(NULL) %in% output_adult$test_data)
  expect_false(list(NULL) %in% output_test$test_data)

  expect_false(list(NULL) %in% output_iris$valid_data)
  expect_false(list(NULL) %in% output_lisbon$valid_data)
  expect_false(list(NULL) %in% output_compas$valid_data)
  expect_false(list(NULL) %in% output_adult$valid_data)
  expect_false(list(NULL) %in% output_test$valid_data)

  expect_false(list(NULL) %in% output_iris$predictions)
  expect_false(list(NULL) %in% output_lisbon$predictions)
  expect_false(list(NULL) %in% output_compas$predictions)
  expect_false(list(NULL) %in% output_adult$predictions)
  expect_false(list(NULL) %in% output_test$predictions)
  })
