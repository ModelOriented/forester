test_that('test-train-input', {

  # Target variables.
  expect_error(train(lisbon, y = NULL, time = NULL, status = NULL, verbose = FALSE))
  expect_error(train(lisbon, y = NULL, time = 'Price', status = NULL, verbose = FALSE))
  expect_error(train(lisbon, y = NULL, time = NULL, status = 'Price', verbose = FALSE))
  expect_error(train(lisbon, y = 'Price', time = NULL, status = 'Parish', verbose = FALSE))
  expect_error(train(lisbon, y = 'Price', time = 'Parish', status = NULL, verbose = FALSE))
  expect_error(train(lisbon, y = 'Price', time = 'Parish', status = 'Id', verbose = FALSE))
  expect_error(train(lisbon, y = 'kappa', time = NULL, status = NULL, verbose = FALSE))
  expect_error(train(lisbon, y = NULL, time = 'kappa', status = 'Price', verbose = FALSE))
  expect_error(train(lisbon, y = NULL, time = 'kappa', status = 'kappa2', verbose = FALSE))
  expect_error(train(lisbon, y = NULL, time = 'Price', status = 'kappa', verbose = FALSE))

  # Data types.
  data_types <- list(as.vector(lisbon[1:160, ]), as.list(lisbon[1:160, ]), as.matrix(lisbon[1:160, ]), as.data.frame(lisbon[1:160, ]))
  for (data in data_types) {
    expect_no_error(train(
      data = data,
      y    = 'Price',
      engine  = c('decision_tree'),
      verbose = FALSE,
      bayes_iter   = 0,
      random_evals = 0))
  }
})
