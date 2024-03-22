test_that('test-train-preprocessing', {

  # Custom preprocessing.
  folder <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  load(capture.output(cat(folder, '/lisbon_custom_prep.RData', sep ='')))
  load(capture.output(cat(folder, '/testing_data_custom_prep.RData', sep ='')))
  load(capture.output(cat(folder, '/iris_custom_prep.RData', sep ='')))
  load(capture.output(cat(folder, '/compas_custom_prep.RData', sep ='')))

  data_tasks <- list(lisbon_custom_prep, testing_data_custom_prep, iris_custom_prep, compas_custom_prep)
  targets    <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  for (i in 1:length(data_tasks)) {
    output <- train(
      data = data_tasks[[i]]$data,
      y    = targets[i],
      engine  = c("ranger", "xgboost", "decision_tree", "lightgbm"),
      verbose = FALSE,
      bayes_iter   = 0,
      random_evals = 0,
      parallel     = FALSE,
      custom_preprocessing = data_tasks[[i]])

    expect_equal(class(output$data), 'data.frame')
    expect_equal(class(output$y), 'character')
    expect_true(!is.null(output$preprocessed_data))
    expect_true(!is.null(output$models_list))
    expect_true(!is.null(output$check_report))
    expect_true(!is.null(output$best_models_on_valid))
    for (i in 22:length(output)) {
      expect_true(!is.null(output[[i]]))
    }
  }

  # Inner preprocessing.
  data_tasks <- list(lisbon, testing_data, iris, compas)
  types      <- c('regression', 'regression', 'multiclass', 'binary_clf')
  outputs    <- list()
  for (i in 1:length(data_tasks)) {
    outputs[[i]] <- preprocessing(data = data_tasks[[i]],
                                  y    = targets[i],
                                  type = types[i])
    output <- train(
      data = data_tasks[[i]],
      y    = targets[i],
      engine  = c("ranger", "xgboost", "decision_tree", "lightgbm"),
      verbose = FALSE,
      bayes_iter   = 0,
      random_evals = 0,
      parallel     = FALSE,
      custom_preprocessing = NULL)

    expect_equal(class(output$data), 'data.frame')
    expect_equal(class(output$y), 'character')
    expect_true(!is.null(output$preprocessed_data))
    expect_true(!is.null(output$models_list))
    expect_true(!is.null(output$check_report))
    expect_true(!is.null(output$best_models_on_valid))
    for (i in 22:length(output)) {
      expect_true(!is.null(output[[i]]))
    }
  }

  expect_equal(ncol(outputs[[1]]$data), 14)
  expect_equal(length(outputs[[1]]$rm_colnames), 3)
  expect_equal(length(outputs[[1]]$bin_labels), 0)

  expect_equal(ncol(outputs[[2]]$data), 11)
  expect_equal(length(outputs[[2]]$rm_colnames), 1)
  expect_equal(length(outputs[[2]]$bin_labels), 0)

  expect_equal(ncol(outputs[[3]]$data), 5)
  expect_equal(length(outputs[[3]]$rm_colnames), 0)
  expect_equal(length(outputs[[3]]$bin_labels), 3)

  expect_equal(ncol(outputs[[4]]$data), 7)
  expect_equal(length(outputs[[4]]$rm_colnames), 0)
  expect_equal(length(outputs[[4]]$bin_labels), 2)

  lisbon_prep       <- outputs[[1]]
  testing_data_prep <- outputs[[2]]
  iris_prep         <- outputs[[3]]
  compas_prep       <- outputs[[4]]

  save(lisbon_prep, file = capture.output(cat(folder, '/lisbon_prep.RData', sep='')))
  save(testing_data_prep, file = capture.output(cat(folder, '/testing_data_prep.RData', sep='')))
  save(iris_prep, file = capture.output(cat(folder, '/iris_prep.RData', sep='')))
  save(compas_prep, file = capture.output(cat(folder, '/compas_prep.RData', sep='')))
})
