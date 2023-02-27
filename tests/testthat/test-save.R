test_that('test-save', {
  train  <- suppressWarnings(train(iris, 'Sepal.Width', verbose = FALSE, random_evals = 2, bayes_iter = 0))
  name   <- save(train, return_name = TRUE)
  train2 <- readRDS(name)
  expect_true(compare(train2, train)$equal)
  file.remove(name)
})
