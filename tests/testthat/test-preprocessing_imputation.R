test_that('test-preprocessing_imputation', {
  median_other <- preprocessing_imputation(testing_data, na_indicators = c(''),
                                           imputation_method = 'median-other',
                                           k = 10, m = 5)
  expect_true(sum(is.na(median_other)) == 0)
  median_frequency <- preprocessing_imputation(testing_data, na_indicators = c(''),
                                               imputation_method = 'median-frequency',
                                               k = 10, m = 5)
  expect_true(sum(is.na(median_frequency)) == 0)
  knn <- preprocessing_imputation(testing_data, na_indicators = c(''),
                                  imputation_method = 'knn',
                                  k = 2, m = 5)
  expect_true(sum(is.na(knn)) == 0)
  mice <- preprocessing_imputation(testing_data, na_indicators = c(''),
                                   imputation_method = 'mice',
                                   k = 10, m = 2)
  expect_true(sum(is.na(mice)) == 0)
})
