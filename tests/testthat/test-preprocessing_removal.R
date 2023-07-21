test_that('test-preprocessing_removal', {
  active_modules <- c(duplicate_cols = TRUE, id_like_cols = TRUE, static_cols = TRUE, sparse_cols = TRUE, corrupt_rows = TRUE, correlated_cols = TRUE)
  data           <- lisbon
  y              <- 'Price'
  id_names       <- c('id', 'nr', 'number', 'idx', 'identification', 'index')
  static_threshold           <- 0.99
  sparse_columns_threshold   <- 0.7
  sparse_rows_threshold      <- 0.7
  na_indicators              <- c('')
  high_correlation_threshold <- 0.7

  to_rm_col <- c()
  to_rm_row <- c()

  if (active_modules[[1]]) {
    rm_dup    <- rm_duplicate_columns(data, y)[[2]]
    expect_true(rm_dup == 14 || rm_dup == 13)
    to_rm_col <- rm_dup
  }
  if (active_modules[[2]]) {
    rm_id     <- rm_id_like_columns(data, id_names)[[2]]
    expect_true(rm_id == 1)
    to_rm_col <- c(to_rm_col, rm_id)
  }
  if (active_modules[[3]]) {
    rm_static <- rm_static_cols(data, y, threshold = static_threshold)[[2]]
    expect_equal(c(12, 13, 14), rm_static)
    to_rm_col <- c(to_rm_col, rm_static)
  }
  if (active_modules[[4]]) {
    rm_sparse <- rm_sparse_columns(data, y, threshold = sparse_columns_threshold, na_indicators = na_indicators)[[2]]
    expect_true(is.null(rm_sparse))
    to_rm_col <- c(to_rm_col, rm_sparse)
  }

  data <- data.frame(data[, -unique(to_rm_col)])
  expect_true(length(names(data)) == 13)

  if (active_modules[[5]]) {
    rm_corrupt <- rm_corrupted_observations(data, y, threshold = sparse_rows_threshold, na_indicators = na_indicators)[[2]]
    expect_true(is.null(rm_corrupt))
    to_rm_row  <- c(to_rm_row, rm_corrupt)
    data       <- data[-to_rm_col, ]
  }
  if (active_modules[[6]]) {
    to_rm_col <- unique(to_rm_col)
    to_rm_cor <- rm_correlated_columns(data, y, threshold = high_correlation_threshold)[[2]]
    data      <- data[, -to_rm_cor]
    for (i in 1:length(to_rm_cor)) {
      k <- sum(to_rm_col <= to_rm_cor[i])
      to_rm_cor[i] <- to_rm_cor[i] + k
    }
    expect_equal(to_rm_cor, c(7, 8, 3))
  }

  to_rm_col <- c(to_rm_col, to_rm_cor)
  to_rm_col <- unique(to_rm_col)

  expect_equal(c(13, 1, 12, 14, 7, 8, 3), to_rm_col)

  data <- preprocessing_removal(lisbon, 'Price')
  expect_equal(c(13, 1, 12, 14, 7, 8, 3), data$rm_col)
  expect_equal(NULL, data$rm_row)

  data <- preprocessing_removal(compas, 'Two_yr_Recidivism')
  expect_equal(NULL, data$rm_col)
  expect_equal(NULL, data$rm_row)

  data <- preprocessing_removal(testing_data, 'y')
  expect_equal(3, data$rm_col)
  expect_equal(378, data$rm_row)
})
