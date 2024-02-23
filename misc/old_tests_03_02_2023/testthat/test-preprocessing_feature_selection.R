test_that('test-preprocessing_feature_selection', {
  VI <- preprocessing_feature_selection(lisbon, 'Price', feature_selection_method = 'VI',
                                        max_features = 'default', nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(VI$rm_col) == 6)
  expect_true(length(names(VI$data)) == 11)
  MI <- preprocessing_feature_selection(lisbon, 'Price', feature_selection_method = 'MI',
                                        max_features = 'default', nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(MI$rm_col) == 6)
  expect_true(length(names(MI$data)) == 11)
  BORUTA <- preprocessing_feature_selection(lisbon, 'Price', feature_selection_method = 'BORUTA',
                                            max_features = 'default', nperm = 1,
                                            cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(names(BORUTA$data)) < length(names(lisbon)))


  VI <- preprocessing_feature_selection(adult[1:500, ], 'salary', feature_selection_method = 'VI',
                                        max_features = 7, nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(VI$rm_col) == 7)
  expect_true(length(names(VI$data)) == 8)
  MI <- preprocessing_feature_selection(adult[1:500, ], 'salary', feature_selection_method = 'MI',
                                        max_features = 7, nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(MI$rm_col) == 7)
  expect_true(length(names(MI$data)) == 8)
  BORUTA <- preprocessing_feature_selection(adult[1:500, ], 'salary', feature_selection_method = 'BORUTA',
                                            max_features = 'default', nperm = 1,
                                            cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(names(BORUTA$data)) < length(names(adult)))


  VI <- preprocessing_feature_selection(compas[1:500, ], 'Two_yr_Recidivism', feature_selection_method = 'VI',
                                        max_features = 5, nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(VI$rm_col) == 1)
  expect_true(length(names(VI$data)) == 6)
  MI <- preprocessing_feature_selection(compas[1:500, ], 'Two_yr_Recidivism', feature_selection_method = 'MI',
                                        max_features = 5, nperm = 1,
                                        cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(MI$rm_col) == 1)
  expect_true(length(names(MI$data)) == 6)
  BORUTA <- preprocessing_feature_selection(compas[1:500, ], 'Two_yr_Recidivism', feature_selection_method = 'BORUTA',
                                            max_features = 'default', nperm = 1,
                                            cutoffPermutations = 20, threadsNumber = NULL)
  expect_true(length(names(BORUTA$data)) < length(names(compas)))
})
