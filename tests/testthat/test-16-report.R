test_that('test-report', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_train.RData', '/testing_data_train.RData',
               '/iris_train.RData',   '/compas_train.RData')
  reports <- c('/lisbon_report', '/testing_data_report',
               '/iris_report',   '/compas_report')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  train_outputs <- list(lisbon_train, testing_data_train, iris_train, compas_train)
  for (i in 1:length(train_outputs)) {
    capture.output(
      suppressMessages(
        expect_no_error(
          report(train_output = train_outputs[[i]],
                 output_file  = capture.output(cat(folder, reports[[i]], sep =''))))))
  }
})
