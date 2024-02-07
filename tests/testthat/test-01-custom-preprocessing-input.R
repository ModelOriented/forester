test_that('test-custom-preprocessing-input', {

  # Data as vector, list, matrix, and data.frame.
  data_types <- list(as.vector(lisbon[1:80, ]), as.list(lisbon[1:80, ]),
                     as.matrix(lisbon[1:80, ]), as.data.frame(lisbon[1:80, ]))

  for (data in data_types) {
    expect_no_error(custom_preprocessing(
      data = data,
      y    = 'Price',
      type = 'auto',
      na_indicators = c(''),
      feature_selection_parameters = list(feature_selection_method = 'none')))
  }

  # Different wrong inputs as y.
  ok_targets  <- list('Price', 'Condition', 'Parish')
  bad_targets <- list('y', NULL, NA, 0, NaN)
  for (target in ok_targets) {
    suppressWarnings(
    expect_no_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = target,
      type = 'auto',
      na_indicators = c(''),
      feature_selection_parameters = list(feature_selection_method = 'none'))))
  }
  for (target in bad_targets) {
    expect_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = target,
      type = 'auto',
      na_indicators = c(''),
      feature_selection_parameters = list(feature_selection_method = 'none')))
  }

  # Wrong task types.
  ok_types  <- list('binary_clf', 'multiclass', 'regression', 'survival', 'auto')
  bad_types <- list('type', NULL, NA, 0, NaN)
  for (type in ok_types) {
    expect_no_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = 'Price',
      type = type,
      na_indicators = c(''),
      feature_selection_parameters = list(feature_selection_method = 'none')))
  }
  for (type in bad_types) {
    expect_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = 'Price',
      type = type,
      na_indicators = c(''),
      feature_selection_parameters = list(feature_selection_method = 'none')))
  }

  # Wrong parameters
  bad_parameters <- list(NULL, NA, NaN, 'none')
  for (parameters in bad_parameters) {
    expect_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = 'Price',
      type = 'auto',
      na_indicators = c(''),
      feature_selection_parameters = parameters))
    expect_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = 'Price',
      type = 'auto',
      na_indicators = c(''),
      removal_parameters = parameters,
      feature_selection_parameters = list(feature_selection_method = 'none')))
    expect_error(custom_preprocessing(
      data = lisbon[1:80, ],
      y    = 'Price',
      type = 'auto',
      na_indicators = c(''),
      imputation_parameters = parameters,
      feature_selection_parameters = list(feature_selection_method = 'none')))
  }
})
