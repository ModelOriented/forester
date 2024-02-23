test_that('test-custom-preprocessing-feature-selection', {
  # Does all the methods work?
  vi <- preprocessing_feature_selection(data = iris,
                                        y    = 'Species',
                                        feature_selection_method = 'VI',
                                        max_features = 2)
  expect_equal(ncol(vi$data), 3)
  expect_equal(vi$rm_col, c(1, 2))

  mi <- preprocessing_feature_selection(data = iris,
                                        y    = 'Species',
                                        feature_selection_method = 'VI',
                                        max_features = 2,
                                        method       = 'estevez')
  expect_equal(ncol(vi$data), 3)
  expect_equal(mi$rm_col, c(1, 2))


  mcfs <- preprocessing_feature_selection(data = lisbon[1:100, ],
                                          y    = 'Price',
                                          feature_selection_method = 'MCFS',
                                          max_features       = 2,
                                          cutoffPermutations = 20,
                                          threadsNumber      = 12)
  expect_equal(ncol(mcfs$data), 3)
  expect_equal(length(mcfs$rm_col), 14)


  boruta <- preprocessing_feature_selection(data = iris,
                                            y    = 'Species',
                                            feature_selection_method = 'BORUTA')
  expect_equal(ncol(boruta$data), 5)
  expect_equal(boruta$rm_col, integer())

  # Checking the input.
  # For nperm parameter.
  perms <- c(-1, 1.5)
  for (perm in perms) {
    expect_error(
      preprocessing_feature_selection(data = iris,
                                      y    = 'Species',
                                      feature_selection_method = 'VI',
                                      nperm = perm))
  }
  # For max_features parameter.
  features <- c(-1, 1.5, 'str')
  for (feature in features) {
    expect_error(
      preprocessing_feature_selection(data = iris,
                                      y    = 'Species',
                                      feature_selection_method = 'VI',
                                      max_features = feature))
    expect_error(
      preprocessing_feature_selection(data = iris,
                                      y    = 'Species',
                                      feature_selection_method = 'MI',
                                      max_features = feature))
    expect_error(
      preprocessing_feature_selection(data = lisbon,
                                      y    = 'Price',
                                      feature_selection_method = 'MCFS',
                                      max_features = feature))
  }

  # For threadsNumber parameter.
  threadsNumbers <- c(-1, 1.5, 300000, NA)
  for (threads in threadsNumbers) {
    expect_error(
      preprocessing_feature_selection(data = lisbon,
                                      y    = 'Price',
                                      feature_selection_method = 'MCFS',
                                      threadsNumber = threads))
  }

  # For cutoffPermutations parameter.
  cutoffPermutations <- c(-1, 1, 2, 1.5)
  for (perms in cutoffPermutations) {
    expect_error(
      preprocessing_feature_selection(data = lisbon,
                                      y    = 'Price',
                                      feature_selection_method = 'MCFS',
                                      cutoffPermutations = perms))
  }

  expect_error(
    preprocessing_feature_selection(data = iris,
                                    y    = 'Species',
                                    feature_selection_method = 'MCFS',
                                    cutoffPermutations = perms))
})
