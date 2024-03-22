test_that('test-18-select-models', {
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_train.RData', '/testing_data_train.RData',
               '/iris_train.RData', '/compas_train.RData')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  train_outputs <- list(compas_train, lisbon_train, testing_data_train, iris_train)
  for (output in train_outputs) {
    model_names_more <- names(output$models_list)[c(1, 2, 3, 4, 5)]
    model_names_less <- names(output$models_list)[c(1, 2, 3)]

    expect_no_error(output_more <- select_models(output, model_names_more))
    expect_no_error(output_less <- select_models(output, model_names_less))

    #expect_true(object.size(output) > object.size(output_more))
    #expect_true(object.size(output) > object.size(output_less))

    expect_equal(length(names(output)), length(names(output_more)))
    expect_equal(length(names(output)), length(names(output_less)))
    capture.output(
      suppressMessages(
        expect_no_error(
          report(train_output = output_more,
                 output_file  = capture.output(cat(folder, '/report-select', sep =''))))))
  }
})
