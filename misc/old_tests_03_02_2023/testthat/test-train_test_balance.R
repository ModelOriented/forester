test_that('test-train_test_balance', {
  # Tests for train test valid split.
  set.seed(123)
  b_lisbon <-
    train_test_balance(
      lisbon,
      y = 'Price',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(length(b_lisbon) == 6)
  expect_true(nrow(b_lisbon$train) == 146)
  expect_true(nrow(b_lisbon$test) == 50)
  expect_true(nrow(b_lisbon$valid) == 50)
  expect_true(nrow(b_lisbon$valid) + nrow(b_lisbon$test) + nrow(b_lisbon$train) ==
                nrow(lisbon))
  set.seed(123)
  b_lisbon <-
    train_test_balance(
      lisbon,
      y = 'Price',
      balance = FALSE,
      fractions = c(0.5, 0.25, 0.25)
    )
  expect_true(nrow(b_lisbon$train) == 122)
  expect_true(nrow(b_lisbon$test) == 62)
  expect_true(nrow(b_lisbon$valid) == 62)
  expect_true(nrow(b_lisbon$valid) + nrow(b_lisbon$test) + nrow(b_lisbon$train) ==
                nrow(lisbon))

  set.seed(123)
  b_compas <-
    train_test_balance(
      compas,
      y = 'Two_yr_Recidivism',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(length(b_compas) == 6)
  expect_true(nrow(b_compas$train) == 3702)
  expect_true(nrow(b_compas$test) == 1235)
  expect_true(nrow(b_compas$valid) == 1235)
  expect_true(nrow(b_compas$valid) + nrow(b_compas$test) + nrow(b_compas$train) ==
                nrow(compas))

  set.seed(123)
  b_compas <-
    train_test_balance(
      compas,
      y = 'Two_yr_Recidivism',
      balance = FALSE,
      fractions = c(0.5, 0.25, 0.25)
    )
  expect_true(nrow(b_compas$train) == 3086)
  expect_true(nrow(b_compas$test) == 1543)
  expect_true(nrow(b_compas$valid) == 1543)
  expect_true(nrow(b_compas$valid) + nrow(b_compas$test) + nrow(b_compas$train) ==
                nrow(compas))

  set.seed(123)
  b_lymph <-
    train_test_balance(
      lymph,
      y = 'class',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(length(b_lymph) == 6)
  expect_true(nrow(b_lymph$train) == 88)
  expect_true(nrow(b_lymph$test) == 30)
  expect_true(nrow(b_lymph$valid) == 30)
  expect_true(nrow(b_lymph$valid) + nrow(b_lymph$test) + nrow(b_lymph$train) ==
                nrow(lymph))

  set.seed(123)
  b_lymph <-
    train_test_balance(
      lymph,
      y = 'class',
      balance = FALSE,
      fractions = c(0.5, 0.25, 0.25)
    )
  expect_true(nrow(b_lymph$train) == 74)
  expect_true(nrow(b_lymph$test) == 37)
  expect_true(nrow(b_lymph$valid) == 37)
  expect_true(nrow(b_lymph$valid) + nrow(b_lymph$test) + nrow(b_lymph$train) ==
                nrow(lymph))

  set.seed(123)
  b_test <-
    train_test_balance(
      testing_data,
      y = 'y',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(length(b_test) == 6)
  expect_true(nrow(b_test$train) == 600)
  expect_true(nrow(b_test$test) == 200)
  expect_true(nrow(b_test$valid) == 200)
  expect_true(nrow(b_test$valid) + nrow(b_test$test) + nrow(b_test$train) ==
                nrow(testing_data))

  set.seed(123)
  b_test <-
    train_test_balance(
      testing_data,
      y = 'y',
      balance = FALSE,
      fractions = c(0.5, 0.25, 0.25)
    )
  expect_true(nrow(b_test$train) == 500)
  expect_true(nrow(b_test$test) == 250)
  expect_true(nrow(b_test$valid) == 250)
  expect_true(nrow(b_test$valid) + nrow(b_test$test) + nrow(b_test$train) ==
                nrow(testing_data))


  # Tests for balancing.
  set.seed(123)
  b_lisbon <-
    train_test_balance(
      lisbon,
      y = 'Price',
      balance = TRUE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(round(mean(b_lisbon$train$Price), 1) == 551418.9)
  expect_true(round(mean(b_lisbon$valid$Price), 1)  == 567551)
  expect_true(round(mean(b_lisbon$test$Price), 1) == 537076.5)

  set.seed(123)
  b_lisbon <-
    train_test_balance(
      lisbon,
      y = 'Price',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(round(mean(b_lisbon$train$Price), 1) == 607065.6)
  expect_true(round(mean(b_lisbon$valid$Price), 1)  == 455758)
  expect_true(round(mean(b_lisbon$test$Price), 1) == 485771.6)

  set.seed(123)
  b_compas <-
    train_test_balance(
      compas,
      y = 'Two_yr_Recidivism',
      balance = TRUE,
      fractions = c(0.6, 0.2, 0.2)
    )
  expect_true(sum(as.integer(b_compas$train$Two_yr_Recidivism == 1)) == 1685)
  expect_true(sum(as.integer(b_compas$train$Two_yr_Recidivism == 0))  == 2018)
  expect_true(sum(as.integer(b_compas$test$Two_yr_Recidivism == 1)) == 562)
  expect_true(sum(as.integer(b_compas$test$Two_yr_Recidivism == 0)) == 673)
  expect_true(sum(as.integer(b_compas$valid$Two_yr_Recidivism == 1)) == 562)
  expect_true(sum(as.integer(b_compas$valid$Two_yr_Recidivism == 0)) == 672)


  set.seed(123)
  b_compas <-
    train_test_balance(
      compas,
      y = 'Two_yr_Recidivism',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(sum(as.integer(b_compas$train$Two_yr_Recidivism == 1)) == 1650)
  expect_true(sum(as.integer(b_compas$train$Two_yr_Recidivism == 0))  == 2052)
  expect_true(sum(as.integer(b_compas$test$Two_yr_Recidivism == 1)) == 563)
  expect_true(sum(as.integer(b_compas$test$Two_yr_Recidivism == 0)) == 672)
  expect_true(sum(as.integer(b_compas$valid$Two_yr_Recidivism == 1)) == 596)
  expect_true(sum(as.integer(b_compas$valid$Two_yr_Recidivism == 0)) == 639)

  set.seed(123)
  b_lymph <-
    train_test_balance(
      lymph,
      y = 'class',
      balance = TRUE,
      fractions = c(0.6, 0.2, 0.2)
    )
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 1)) == 0)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 2)) == 47)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 3)) == 36)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 4)) == 2)

  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 1)) == 1)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 2)) == 17)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 3)) == 12)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 4)) == 1)

  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 1)) == 1)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 2)) == 17)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 3)) == 13)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 4)) == 1)


  set.seed(123)
  b_lymph <-
    train_test_balance(
      lymph,
      y = 'class',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 1)) == 2)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 2)) == 46)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 3)) == 40)
  expect_true(sum(as.integer(as.integer(b_lymph$train$class) == 4)) == 0)

  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 1)) == 0)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 2)) == 16)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 3)) == 12)
  expect_true(sum(as.integer(as.integer(b_lymph$test$class) == 4)) == 2)

  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 1)) == 0)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 2)) == 19)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 3)) == 9)
  expect_true(sum(as.integer(as.integer(b_lymph$valid$class) == 4)) == 2)

  set.seed(123)
  b_test <-
    train_test_balance(
      testing_data,
      y = 'y',
      balance = TRUE,
      fractions = c(0.6, 0.2, 0.2)
    )
  expect_true(round(mean(b_test$train$y), 1) == 14.5)
  expect_true(round(mean(b_test$test$y), 1)  == 14.6)
  expect_true(round(mean(b_test$valid$y), 1) == 14.6)


  set.seed(123)
  b_test <-
    train_test_balance(
      testing_data,
      y = 'y',
      balance = FALSE,
      fractions = c(0.6, 0.2, 0.2)
    )

  expect_true(round(mean(b_test$train$y), 1) == 14.6)
  expect_true(round(mean(b_test$test$y), 1)  == 14.1)
  expect_true(round(mean(b_test$valid$y), 1) == 14.5)
})
