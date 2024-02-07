test_that('test-custom-preprocessing-full', {
  prepdata <- custom_preprocessing(data = lisbon,
                                   y = 'Price',
                                   type = 'auto',
                                   na_indicators = c('Used', 3, 'Estrela'),
                                   removal_parameters = list(
                                     active_modules = c(duplicate_cols = TRUE, id_like_cols    = TRUE,
                                                        static_cols    = TRUE, sparse_cols     = TRUE,
                                                        corrupt_rows   = TRUE, correlated_cols = TRUE),
                                     id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                                     static_threshold           = 0.99,
                                     sparse_columns_threshold   = 0.3,
                                     sparse_rows_threshold      = 0.1,
                                     high_correlation_threshold = 0.7
                                   ),
                                   imputation_parameters = list(
                                     imputation_method = 'median-other'
                                   ),
                                   feature_selection_parameters = list(
                                     feature_selection_method = 'BORUTA',
                                     max_features = 'default'
                                   ),
                                   verbose = FALSE)
  expect_equal(nrow(prepdata$data), 232)
  expect_equal(ncol(prepdata$data), 9)
  expect_equal(length(prepdata$rm_colnames), 8)
  expect_equal(length(prepdata$rm_rows), 14)
  expect_equal(length(prepdata$bin_labels), 0)

  lisbon_custom_prep <- custom_preprocessing(data = lisbon, y = 'Price',
                                      feature_selection_parameters = list(feature_selection_method = 'none'))

  expect_equal(nrow(lisbon_custom_prep$data), 246)
  expect_equal(ncol(lisbon_custom_prep$data), 10)
  expect_equal(length(lisbon_custom_prep$rm_colnames), 7)
  expect_equal(length(lisbon_custom_prep$rm_rows), 0)
  expect_equal(length(lisbon_custom_prep$bin_labels), 0)

  testing_data_custom_prep <- custom_preprocessing(data = testing_data, y = 'y',
                                            feature_selection_parameters = list(feature_selection_method = 'none'))


  expect_equal(nrow(testing_data_custom_prep$data), 1000)
  expect_equal(ncol(testing_data_custom_prep$data), 12)
  expect_equal(length(testing_data_custom_prep$rm_colnames), 0)
  expect_equal(length(testing_data_custom_prep$rm_rows), 0)
  expect_equal(length(testing_data_custom_prep$bin_labels), 0)

  iris_custom_prep <- custom_preprocessing(data = iris, y = 'Species',
                                    feature_selection_parameters = list(feature_selection_method = 'none'))

  expect_equal(nrow(iris_custom_prep$data), 150)
  expect_equal(ncol(iris_custom_prep$data), 3)
  expect_equal(length(iris_custom_prep$rm_colnames), 2)
  expect_equal(length(iris_custom_prep$rm_rows), 0)
  expect_equal(length(iris_custom_prep$bin_labels), 3)

  compas_custom_prep <- custom_preprocessing(data = compas, y = 'Two_yr_Recidivism',
                                      feature_selection_parameters = list(feature_selection_method = 'none'))

  expect_equal(nrow(compas_custom_prep$data), 6172)
  expect_equal(ncol(compas_custom_prep$data), 7)
  expect_equal(length(compas_custom_prep$rm_colnames), 0)
  expect_equal(length(compas_custom_prep$rm_rows), 0)
  expect_equal(length(compas_custom_prep$bin_labels), 2)

  folder <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  save(lisbon_custom_prep, file = capture.output(cat(folder, '/lisbon_custom_prep.RData', sep='')))
  save(testing_data_custom_prep, file = capture.output(cat(folder, '/testing_data_custom_prep.RData', sep='')))
  save(iris_custom_prep, file = capture.output(cat(folder, '/iris_custom_prep.RData', sep='')))
  save(compas_custom_prep, file = capture.output(cat(folder, '/compas_custom_prep.RData', sep='')))
})
