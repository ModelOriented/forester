test_that('test-custom-preprocessing-removal', {

  # Duplicates removal
  duplicate <- rm_duplicate_columns(data = lisbon, y = 'Price')
  expect_equal(ncol(lisbon), 17)
  expect_equal(duplicate$idx, 13)
  expect_equal(ncol(duplicate$data), 16)

  # Id-like removal.
  id_like <- rm_id_like_columns(data = lisbon, id_names = c('condition'))
  expect_equal(id_like$idx, c(1, 2))
  id_like <- rm_id_like_columns(data = lisbon, id_names = c('kappa'))
  expect_equal(id_like$idx, c(1))
  id_like <- rm_id_like_columns(data = lisbon, id_names = c('Condition'))
  expect_equal(id_like$idx, c(1))
  expect_error(rm_id_like_columns(data = lisbon, id_names = as.matrix(c('', 'kappa'))))

  # Static columns removal.
  static <- rm_static_cols(data = lisbon, y = 'Price', threshold = 0.99)
  expect_equal(static$idx, c(12, 13, 14))
  static <- rm_static_cols(data = lisbon, y = 'Price', threshold = 1)
  expect_equal(static$idx, NULL)
  static <- rm_static_cols(data = lisbon, y = 'Price', threshold = 0.5)
  expect_equal(static$idx, c(3, 4, 9, 12, 13, 14))
  expect_error(rm_static_cols(data = lisbon, y = 'Price', threshold =  1.5))
  expect_error(rm_static_cols(data = lisbon, y = 'Price', threshold =  -0.5))

  # Sparse columns removal.
  sparse <- rm_sparse_columns(data = lisbon, y = 'Price', threshold = 0.3, na_indicators = c(''))
  expect_equal(sparse$idx, NULL)
  sparse <- rm_sparse_columns(data = lisbon, y = 'Price', threshold = 0.3, na_indicators = c('New'))
  expect_equal(sparse$idx, 2)
  sparse <- rm_sparse_columns(data = lisbon, y = 'Price', threshold = 0.3, na_indicators = c('Used'))
  expect_equal(sparse$idx, NULL)
  expect_error(rm_sparse_columns(data = lisbon, y = 'Price', threshold =  1.5, na_indicators = c('')))
  expect_error(rm_sparse_columns(data = lisbon, y = 'Price', threshold =  -0.5, na_indicators = c('')))
  expect_error(rm_sparse_columns(data = lisbon, y = 'Price', threshold =  -0.5, na_indicators = as.matrix(c('', 'kappa'))))

  # Corrupted observations.
  corrupted_rows <- rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  0.3, na_indicators = c(''))
  expect_equal(corrupted_rows$idx, NULL)
  corrupted_rows <- rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  0.05, na_indicators = c('Used', 'Homes', 'Apartment'))
  expect_equal(length(corrupted_rows$idx), 241)
  corrupted_rows <- rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  0.1, na_indicators = c('Used', 'Homes', 'Apartment'))
  expect_equal(length(corrupted_rows$idx), 227)
  corrupted_rows <- rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  0.15, na_indicators = c('Used', 'Homes', 'Apartment'))
  expect_equal(length(corrupted_rows$idx), 48)
  expect_error(rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  1.5, na_indicators = c('')))
  expect_error(rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  -0.5, na_indicators = c('')))
  expect_error(rm_corrupted_observations(data = lisbon, y = 'Price', threshold =  -0.5, na_indicators = as.matrix(c('', 'kappa'))))

  # Correlated columns.
  correlation <- rm_correlated_columns(data = lisbon, y = 'Price', threshold =  0.5)
  expect_equal(correlation$idx, c(5, 6, 7, 10, 2, 3))
  expect_equal(c(correlation$rm_num, correlation$rm_cat), c(5, 6, 7, 10, 2, 3))
  correlation <- rm_correlated_columns(data = lisbon, y = 'Price', threshold =  0.7)
  expect_equal(correlation$idx, c(7, 8, 3))
  expect_equal(c(correlation$rm_num, correlation$rm_cat), c(7, 8, 3))
  correlation <- rm_correlated_columns(data = lisbon, y = 'Price', threshold =  0.9)
  expect_equal(correlation$idx, c(7, 3))
  expect_equal(c(correlation$rm_num, correlation$rm_cat), c(7, 3))
  expect_error(rm_correlated_columns(data = lisbon, y = 'Price', threshold =  1.5))
  expect_error(rm_correlated_columns(data = lisbon, y = 'Price', threshold =  -0.5))

  full <- preprocessing_removal(data = lisbon,
                        y    = 'Price',
                        active_modules = c(duplicate_cols = TRUE, id_like_cols    = TRUE,
                                           static_cols    = TRUE, sparse_cols     = TRUE,
                                           corrupt_rows   = TRUE, correlated_cols = TRUE),
                        id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                        static_threshold           = 0.99,
                        sparse_columns_threshold   = 0.3,
                        sparse_rows_threshold      = 0.3,
                        na_indicators              = c(''),
                        high_correlation_threshold = 0.7)

  expect_equal(full$rm_col, c(13, 1, 12, 14, 7, 8, 3))
  expect_equal(full$rm_row, NULL)
})
