test_that('test-explain', {
  iris_bin          <- iris[1:100, ]
  target = 'Species'
  type              <- guess_type(iris_bin, target)
  set.seed(123)
  preprocessed_data <- preprocessing(iris_bin, target, type = type)
  preprocessed_data <- preprocessed_data$data
  set.seed(123)
  split_data <-
    train_test_balance(preprocessed_data,
                       y       = target,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y       = target,
                 engine  = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train   = split_data$train)
  set.seed(123)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type   = type)
  )
  set.seed(123)
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y      = target,
                     engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                     type   = type)
  )
  set.seed(123)
  suppressWarnings(
    explainer <-
      explain(model,
              test_data,
              y = target
      )
  )

  suppressWarnings(FI_ranger        <- DALEX::model_parts(explainer$ranger))
  suppressWarnings(FI_xgboost       <- DALEX::model_parts(explainer$xgboost))
  suppressWarnings(FI_decision_tree <- DALEX::model_parts(explainer$decision_tree))
  suppressWarnings(FI_lightgbm      <- DALEX::model_parts(explainer$lightgbm))
  #suppressWarnings(FI_catboost     <- DALEX::model_parts(explainer$catboost))

  expect_true(length(explainer) == 5)
  expect_true(length(explainer$ranger) == 11)
  expect_true(length(explainer$xgboost) == 11)
  expect_true(length(explainer$decision_tree) == 11)
  expect_true(length(explainer$lightgbm) == 11)
  expect_true(length(explainer$catboost) == 11)

  expect_true(nrow(FI_ranger) == 77)
  expect_true(nrow(FI_xgboost) == 66)
  expect_true(nrow(FI_decision_tree) == 77)
  expect_true(nrow(FI_decision_tree) == 77)
  #expect_true(nrow(FI_catboost) == 77)

  # expect_equal(round(head(explainer$ranger$residuals, 2), 4), round(c(-0.002, 0.000), 4))
  # expect_equal(round(head(explainer$ranger$y_hat, 2), 4), round(c(1.002, 1.000), 4))
  # expect_equal(round(head(FI_ranger$dropout_loss, 2), 4), round(c(0.0351, 0.0351), 4))
  # expect_equal(head(FI_ranger$variable), c('_full_model_', 'Species', 'Sepal.Width', 'Sepal.Length', 'Petal.Width', 'Petal.Length'))
  #
  # expect_equal(round(head(explainer$xgboost$residuals), 4), round(c(0.0005298257 , 0.0005298257 , 0.0005298257 , 0.0005298257, 0.0005298257 , 0.0005298257), 4))
  # expect_equal(round(head(explainer$xgboost$y_hat), 4), round(c(0.9994702, 0.9994702, 0.9994702, 0.9994702, 0.9994702, 0.9994702), 4))
  # expect_equal(round(head(FI_xgboost$dropout_loss, 2), 4), round(c(0.0012, 0.0012), 4))
  # expect_equal(head(FI_xgboost$variable), c('_full_model_', 'Petal.Width', 'Sepal.Length', 'Sepal.Width', 'Petal.Length', '_baseline_'))
  #
  # expect_equal(round(as.double(head(explainer$decision_tree$residuals)), 4), round(as.double(c(0, 0, 0, 0, 0, 0)), 4))
  # expect_equal(round(as.double(head(explainer$decision_tree$y_hat)), 4), round(as.double(c(1, 1, 1, 1, 1, 1)), 4))
  # expect_equal(round(head(FI_decision_tree$dropout_loss), 4), round(c(0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.705 ), 4))
  # expect_equal(head(FI_decision_tree$variable), c('_full_model_', 'Sepal.Length', 'Sepal.Width', 'Petal.Width', 'Species', 'Petal.Length'))
  #
  # expect_equal(round(head(explainer$lightgbm$residuals), 4), round(c(-0.000, 0.000, 0.0000, -0.000, 0.0000, 0.0000), 4))
  # expect_equal(round(head(explainer$lightgbm$y_hat), 4), round(as.double(c(0, 0, 0, 0, 0, 0)), 4))
  # expect_equal(round(head(FI_lightgbm$dropout_loss, 2), 4), round(c(0, 0), 4))
  # expect_equal(head(FI_lightgbm$variable), c('_full_model_', 'Sepal.Length', 'Sepal.Width', 'Petal.Width', 'Petal.Length', '_baseline_'))
  #
  # expect_equal(round(head(explainer$catboost$residuals, 2), 4), round(c(-0.0066, -0.0043), 4))
  # expect_equal(round(head(explainer$catboost$y_hat, 2), 4), round(as.double(c(0.0066, 0.0043)), 4))
  # #expect_equal(round(head(FI_catboost$dropout_loss), 4), round(c(0.03019464, 0.03019464, 0.03630526, 0.04119495, 0.32254080, 0.37347181), 4))
  # #expect_equal(head(FI_catboost$variable), c('_full_model_', 'Species', 'Sepal.Width', 'Sepal.Length', 'Petal.Width', 'Petal.Length'))

  target            <- 'Two_yr_Recidivism'
  type              <- guess_type(compas, target)
  set.seed(123)
  preprocessed_data <- preprocessing(compas, target, type = type)
  preprocessed_data <- preprocessed_data$data
  set.seed(123)
  split_data <-
    train_test_balance(preprocessed_data,
                       y       = target,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y       = target,
                 engine  = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                 predict = TRUE,
                 train   = split_data$train)
  set.seed(123)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type   = type)
  )
  set.seed(123)
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y      = target,
                     engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                     type   = type)
  )
  set.seed(123)
  suppressWarnings(
    explainer <-
      explain(model,
              test_data,
              y = target
      )
  )

  suppressWarnings(FI_ranger        <- DALEX::model_parts(explainer$ranger))
  suppressWarnings(FI_xgboost       <- DALEX::model_parts(explainer$xgboost))
  suppressWarnings(FI_decision_tree <- DALEX::model_parts(explainer$decision_tree))
  suppressWarnings(FI_lightgbm      <- DALEX::model_parts(explainer$lightgbm))
  #suppressWarnings(FI_catboost     <- DALEX::model_parts(explainer$catboost))

  expect_true(length(explainer) == 5)
  expect_true(length(explainer$ranger) == 11)
  expect_true(length(explainer$xgboost) == 11)
  expect_true(length(explainer$decision_tree) == 11)
  expect_true(length(explainer$lightgbm) == 11)
  expect_true(length(explainer$catboost) == 11)

  expect_true(nrow(FI_ranger) == 99)
  expect_true(nrow(FI_xgboost) == 242)
  expect_true(nrow(FI_decision_tree) == 99)
  expect_true(nrow(FI_decision_tree) == 99)
  #expect_true(nrow(FI_catboost) == 77)

  # expect_equal(round(head(explainer$ranger$residuals, 2), 4), round(c(-0.2042, -0.3444), 4))
  # expect_equal(round(head(explainer$ranger$y_hat, 2), 4), round(c(1.2042, 1.3444), 4))
  # expect_equal(round(head(FI_ranger$dropout_loss, 2), 4), round(c(0.4614, 0.4614), 4))
  # expect_equal(head(FI_ranger$variable), c('_full_model_', 'Misdemeanor', 'Two_yr_Recidivism', 'Ethnicity', 'Sex', 'Age_Above_FourtyFive'))
  #
  # expect_equal(round(head(explainer$xgboost$residuals), 4), round(c(-0.1727 , -0.2922 , 0.8133 , 0.4006, 0.3808 , -0.3527), 4))
  # expect_equal(round(head(explainer$xgboost$y_hat), 4), round(c(1.1727, 1.2922, 1.1867, 1.5994, 1.6192, 1.3527), 4))
  # expect_equal(round(head(FI_xgboost$dropout_loss, 2), 4), round(c(0.4643, 0.4639), 4))
  # expect_equal(head(FI_xgboost$variable), c('_full_model_', 'Misdemeanor_0', 'Ethnicity_Hispanic', 'Age_Above_FourtyFive_other', 'Age_Below_TwentyFive_other', 'Ethnicity_other'))
  #
  # expect_equal(round(as.double(head(explainer$decision_tree$residuals)), 4), round(as.double(c(-0.2751, -0.2751, 0.8145, 0.3903, 0.4675, -0.2751)), 4))
  # expect_equal(round(as.double(head(explainer$decision_tree$y_hat)), 4), round(as.double(c(1.2751, 1.2751, 1.1855, 1.6097, 1.5325, 1.2751)), 4))
  # expect_equal(round(head(FI_decision_tree$dropout_loss, 2), 4), round(c(0.4641, 0.4641), 4))
  # expect_equal(head(FI_decision_tree$variable), c('_full_model_', 'Two_yr_Recidivism', 'Misdemeanor', 'Ethnicity', 'Sex', 'Age_Above_FourtyFive'))
  #
  # expect_equal(round(head(explainer$lightgbm$residuals), 4), round(c(-0.2340, -0.2775, 0.7755, 0.3715, 0.4300, -0.2947), 4))
  # expect_equal(round(head(explainer$lightgbm$y_hat), 4), round(as.double(c(0.2340, 0.2775, 0.2245, 0.6285, 0.5700, 0.2947)), 4))
  # expect_equal(round(head(FI_lightgbm$dropout_loss, 2), 4), round(c(0.4654, 0.4654), 4))
  # expect_equal(head(FI_lightgbm$variable), c('_full_model_', 'Ethnicity', 'Sex', 'Misdemeanor', 'Age_Above_FourtyFive', 'Age_Below_TwentyFive'))
  #
  # expect_equal(round(head(explainer$catboost$residuals, 2), 4), round(c(-0.1785, -0.2843), 4))
  # expect_equal(round(head(explainer$catboost$y_hat, 2), 4), round(as.double(c(0.1785, 0.2843)), 4))
  # #expect_equal(round(head(FI_catboost$dropout_loss), 4), round(c(0.03019464, 0.03019464, 0.03630526, 0.04119495, 0.32254080, 0.37347181), 4))
  # #expect_equal(head(FI_catboost$variable), c('_full_model_', 'Species', 'Sepal.Width', 'Sepal.Length', 'Petal.Width', 'Petal.Length'))


  target              <- 'Price'
  type                <- guess_type(lisbon, target)
  set.seed(123)
  suppressWarnings(
    preprocessed_data <- preprocessing(lisbon, target, type = type)
  )
  preprocessed_data   <- preprocessed_data$data
  set.seed(123)
  split_data <-
    train_test_balance(preprocessed_data,
                       y       = target,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y       = target,c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train   = split_data$train)
  set.seed(123)
  suppressWarnings(
    model <-
      train_models(train_data,
                   y      = target,
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'),
                   type   = type)
  )
  set.seed(123)
  suppressWarnings(
    predictions <-
      predict_models(model,
                     test_data,
                     y      = target,
                     engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                     type   = type)
  )
  set.seed(123)
  suppressWarnings(
    explainer <-
      explain(model,
              test_data,
              y = target)
  )

  suppressWarnings(FI_ranger        <- DALEX::model_parts(explainer$ranger))
  suppressWarnings(FI_xgboost       <- DALEX::model_parts(explainer$xgboost))
  suppressWarnings(FI_decision_tree <- DALEX::model_parts(explainer$decision_tree))
  suppressWarnings(FI_lightgbm      <- DALEX::model_parts(explainer$lightgbm))
  #suppressWarnings(FI_catboost     <- DALEX::model_parts(explainer$catboost))

  expect_true(length(explainer) == 5)
  expect_true(length(explainer$ranger) == 11)
  expect_true(length(explainer$xgboost) == 11)
  expect_true(length(explainer$decision_tree) == 11)
  expect_true(length(explainer$lightgbm) == 11)
  expect_true(length(explainer$catboost) == 11)

  expect_true(nrow(FI_ranger) == 176)
  expect_true(nrow(FI_xgboost) == 539)
  expect_true(nrow(FI_decision_tree) == 176)
  expect_true(nrow(FI_decision_tree) == 176)
  #expect_true(nrow(FI_catboost) == 77)

  # expect_equal(as.integer(head(explainer$ranger$residuals, 2)), as.integer(c(258100, -282663)))
  # expect_equal(as.integer(head(explainer$ranger$y_hat, 2)), as.integer(c(1011899, 1207663)))
  # expect_equal(as.integer(head(FI_ranger$dropout_loss, 2)), as.integer(c(112880, 112880)))
  # expect_equal(head(FI_ranger$variable), c('_full_model_', 'PropertyType', 'Price', 'PropertySubType', 'Parish', 'Parking'))
  #
  # expect_equal(as.integer(head(explainer$xgboost$residuals)), as.integer(c(350785, 144536, 35628, -6570, -51879, -115666)))
  # expect_equal(as.integer(head(explainer$xgboost$y_hat)), as.integer(c(919214, 780463, 134371, 446570, 166879, 364666)))
  # expect_equal(round(head(FI_xgboost$dropout_loss, 2)), round(c(121885, 121288)))
  # expect_equal(head(FI_xgboost$variable), c('_full_model_', 'PropertySubType_Duplex', 'Parish_Lumiar', 'Parking', 'Longitude', 'Parish_Campo de Ourique'))
  #
  # expect_equal(as.integer(head(explainer$decision_tree$residuals)), as.integer(c(291562, -53437, -9142, -39560, -64142, -230560)))
  # expect_equal(as.integer(head(explainer$decision_tree$y_hat)), as.integer(c(978437, 978437, 179142, 479560, 179142, 479560)))
  # expect_equal(as.integer(head(FI_decision_tree$dropout_loss)), as.integer(c(147958, 147958, 147958, 147958, 147958, 147958)))
  # expect_equal(head(FI_decision_tree$variable), c('_full_model_', 'PropertyType', 'PropertySubType', 'Bedrooms', 'AreaNet', 'Parking'))
  #
  # expect_equal(as.integer(head(explainer$lightgbm$residuals)), as.integer(c(-256549, -483276, -24042, 3055, -150065, -156015)))
  # expect_equal(as.integer(head(explainer$lightgbm$y_hat)), as.integer(c(1526549, 1408276, 194042, 436944, 265065, 405015)))
  # expect_equal(as.integer(head(FI_lightgbm$dropout_loss)), as.integer(c(147232, 138849, 147232, 147232, 147232, 147232)))
  # expect_equal(head(FI_lightgbm$variable), c('_full_model_', 'Bedrooms', 'Condition', 'PropertyType', 'PropertySubType', 'AreaGross'))
  #
  # expect_equal(as.integer(head(explainer$catboost$residuals, 2)), as.integer(c(339555, -58231)))
  # expect_equal(as.integer(head(explainer$catboost$y_hat, 2)), as.integer(c(930444, 983231)))
  # #expect_equal(round(head(FI_catboost$dropout_loss), 4), round(c(0.03019464, 0.03019464, 0.03630526, 0.04119495, 0.32254080, 0.37347181), 4))
  # #expect_equal(head(FI_catboost$variable), c('_full_model_', 'Species', 'Sepal.Width', 'Sepal.Length', 'Petal.Width', 'Petal.Length'))


  # Single explainers
  suppressWarnings(
    exp_ranger <-
      explain(model$ranger_model,
              test_data,
              y = target
      )
  )
  suppressWarnings(
    exp_xgboost <-
      explain(model$xgboost_model,
              test_data,
              y = target
      )
  )
  suppressWarnings(
    exp_desicion_tree <-
      explain(model$decision_tree_model,
              test_data,
              y = target
      )
  )
  suppressWarnings(
    exp_lightgbm <-
      explain(model$lightgbm_model,
              test_data,
              y = target
      )
  )
  suppressWarnings(
    exp_catboost <-
      explain(model$catboost_model,
              test_data,
              y = target
      )
  )
  suppressWarnings(FI_ranger        <- DALEX::model_parts(exp_ranger))
  suppressWarnings(FI_xgboost       <- DALEX::model_parts(exp_xgboost))
  suppressWarnings(FI_decision_tree <- DALEX::model_parts(exp_desicion_tree))
  suppressWarnings(FI_lightgbm      <- DALEX::model_parts(exp_lightgbm))
  #suppressWarnings(FI_catboost      <- DALEX::model_parts(exp_catboost$catboost_explainer))

  expect_true(length(FI_ranger$permutation) == 176)
  expect_true(length(FI_xgboost$permutation) == 539)
  expect_true(length(FI_decision_tree$permutation) == 176)
  expect_true(length(FI_lightgbm$permutation) == 165)
  #expect_true(length(FI_catboost$permutation) == 176)
  })
