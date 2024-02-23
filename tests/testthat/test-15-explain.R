test_that('test-explain', {
  # Loading the files.
  folder  <- capture.output(cat(getwd(), '/checkpoints', sep = ''))
  files   <- c('/lisbon_train.RData', '/testing_data_train.RData',
               '/iris_train.RData',   '/compas_train.RData')
  for (file in files) {
    load(capture.output(cat(folder, file, sep ='')))
  }
  train_outputs <- list(lisbon_train, testing_data_train, iris_train, compas_train)

  for (i in 1:length(train_outputs)) {
    engine <- NULL
    if (grepl('ranger', train_outputs[[i]]$score_valid$name[1])) {
      engine <- c('ranger')
    } else if (grepl('xgboost', train_outputs[[i]]$score_valid$name[1])) {
      engine <- c('xgboost')
    } else if (grepl('decision_tree', train_outputs[[i]]$score_valid$name[1])) {
      engine <- c('decision_tree')
    } else if (grepl('lightgbm', train_outputs[[i]]$score_valid$name[1])) {
      engine <- c('lightgbm')
    } else if (grepl('catboost', train_outputs[[i]]$score_valid$name[1])) {
      train_outputs[[i]]$score_valid$name[1]
      engine <- c('catboost')
    }

    if (engine != c('catboost') && !is.null(engine)) { # For catboost there is an error with DALEX::model_parts().
      suppressMessages(
      expect_no_error(
        draw_feature_importance(train_outputs[[i]]$models_list[[train_outputs[[i]]$score_valid$name[1]]],
                                train_outputs[[i]]$valid_data,
                                train_outputs[[i]]$y)))
      expect_no_error(
        explain(models    = train_outputs[[i]]$best_models_on_valid[[1]][1],
                test_data = train_outputs[[i]]$valid_data,
                y         = train_outputs[[i]]$y))
    }
  }

})
