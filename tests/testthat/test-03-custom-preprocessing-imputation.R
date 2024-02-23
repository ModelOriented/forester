test_that('test-custom-preprocessing-imputation', {
  expect_equal(sum(is.na(testing_data)), 1361)

  # Does all methods work?
  methods <- c('median-other', 'median-frequency', 'knn', 'mice')
  suppressWarnings(
  for (method in methods) {
    expect_equal(sum(is.na(
    preprocessing_imputation(data = testing_data,
                             na_indicators = c(''),
                             imputation_method = method,
                             k = 10,
                             m = 5)
    )), 0)
  })

  # Does na indicators work?
  expect_equal(nrow(lisbon[lisbon$Parish == 'Estrela', ]), 17)
  suppressWarnings(
  for (method in methods) {
    impute <- preprocessing_imputation(data = lisbon,
                                       na_indicators = c('', 'Estrela'),
                                       imputation_method = method,
                                       k = 10,
                                       m = 5)
    expect_equal(nrow(impute[impute$Parish == 'Estrela', ]), 0)
  })

  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'method'))
  expect_error(preprocessing_imputation(data = lisbon, na_indicators = as.matrix(c('', 'Estrela'))))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'knn', k = 1.5))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'knn', k = -1))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'knn', k = '1.5'))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'mice', m = 1.5))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'mice', m = -1))
  expect_error(preprocessing_imputation(data = lisbon, imputation_method = 'mice', m = '1.5'))
})
