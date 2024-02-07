test_that('test-custom_preprocessing', {
  prep <- custom_preprocessing(data = lisbon,
                               y = 'Price',
                               na_indicators = c(''),
                               removal_parameters = list(
                                 active_modules = c(duplicate_cols = TRUE, id_like_cols = TRUE,
                                                    static_cols = TRUE, sparse_cols = TRUE,
                                                    corrupt_rows = TRUE, correlated_cols = TRUE),
                                 id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                                 static_threshold = 0.99,
                                 sparse_columns_threshold = 0.7,
                                 sparse_rows_threshold = 0.7,
                                 high_correlation_threshold = 0.7
                               ),
                               imputation_parameters = list(
                                 imputation_method = 'median-other',
                                 k = 10,
                                 m = 5
                               ),
                               feature_selection_parameters = list(
                                 feature_selection_method = 'VI',
                                 max_features = 5,
                                 nperm = 1,
                                 cutoffPermutations = 20,
                                 threadsNumber = NULL
                               ),
                               verbose = FALSE)

  expect_true(length(prep$rm_colnames) == 11)
  expect_true(length(colnames(prep$data)) == 6)

  out <- train(prep$data, 'Price', bayes_iter = 0, split_seed = 123, custom_preprocessing = prep, verbose = FALSE)

  prep2 <- custom_preprocessing(data = testing_data,
                               y = 'y',
                               na_indicators = c(''),
                               removal_parameters = list(
                                 active_modules = c(duplicate_cols = TRUE, id_like_cols = TRUE,
                                                    static_cols = TRUE, sparse_cols = TRUE,
                                                    corrupt_rows = TRUE, correlated_cols = TRUE),
                                 id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                                 static_threshold = 0.99,
                                 sparse_columns_threshold = 0.7,
                                 sparse_rows_threshold = 0.7,
                                 high_correlation_threshold = 0.7
                               ),
                               imputation_parameters = list(
                                 imputation_method = 'median-other',
                                 k = 10,
                                 m = 5
                               ),
                               feature_selection_parameters = list(
                                 feature_selection_method = 'VI',
                                 max_features = 5,
                                 nperm = 1,
                                 cutoffPermutations = 20,
                                 threadsNumber = NULL
                               ),
                               verbose = FALSE)

  expect_true(length(prep2$rm_colnames) == 6)
  expect_true(length(colnames(prep2$data)) == 6)

  out2 <- train(prep2$data, 'y', bayes_iter = 0, split_seed = 123, custom_preprocessing = prep2, verbose = FALSE)

})
