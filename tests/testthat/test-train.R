test_that('test-train', {
  df_iris   <- iris[1:100, ]
  df_lisbon <- lisbon
  df_compas <- compas
  df_adult  <- adult[1:100, ]
  df_test   <- testing_data

  y_iris   <- 'Species'
  y_lisbon <- 'Price'
  y_compas <- 'Two_yr_Recidivism'
  y_adult  <- 'salary'
  y_test   <- 'y'

  suppressWarnings(output_iris   <- train(df_iris, y_iris))
  suppressWarnings(output_lisbon <- train(df_lisbon, y_lisbon))
  suppressWarnings(output_compas <- train(df_compas, y_compas))
  suppressWarnings(output_adult  <- train(df_adult, y_adult))
  suppressWarnings(output_test   <- train(df_test, y_test))

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
  expect_equal(dim(output_compas$preprocessed_data), c(6172, 7))
  expect_equal(dim(output_adult$preprocessed_data), c(100, 15))
  expect_equal(dim(output_test$preprocessed_data), c(1000, 11))

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

  expect_true(nrow(output_iris$ranked_list)   == 30)
  expect_true(nrow(output_lisbon$ranked_list) == 30)
  expect_true(nrow(output_compas$ranked_list) == 30)
  expect_true(nrow(output_adult$ranked_list)  == 30)
  expect_true(nrow(output_test$ranked_list)   == 30)

  })

