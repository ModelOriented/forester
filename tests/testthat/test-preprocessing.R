test_that("test-preprocessing", {

  prep_compas <- preprocessing(compas, 'Two_yr_Recidivism')
  suppressWarnings(prep_lisbon <- preprocessing(lisbon, 'Price'))
  prep_iris   <- preprocessing(iris, 'Species')
  prep_adult  <- preprocessing(adult[1:1000,], 'salary')
  prep_lymph  <- preprocessing(lymph, 'class')
  prep_test   <- preprocessing(testing_data, 'y')

  rm_stat_compas <- pre_rm_static_cols(compas, 'Two_yr_Recidivism')
  rm_stat_lisbon <- pre_rm_static_cols(lisbon, 'Price')
  rm_stat_iris   <- pre_rm_static_cols(iris, 'Species')
  rm_stat_adult  <- pre_rm_static_cols(adult[1:1000,], 'salary')
  rm_stat_lymph  <- pre_rm_static_cols(lymph, 'class')
  rm_stat_test   <- pre_rm_static_cols(testing_data, 'y')

  expect_true(ncol(rm_stat_compas) == 7)
  expect_true(ncol(compas) == 7)

  expect_true(ncol(rm_stat_lisbon) == 14)
  expect_true(ncol(lisbon) == 17)

  expect_true(ncol(rm_stat_iris) == 5)
  expect_true(ncol(iris) == 5)

  expect_true(ncol(rm_stat_adult) == 15)
  expect_true(ncol(adult[1:1000,]) == 15)

  expect_true(ncol(rm_stat_lymph) == 19)
  expect_true(ncol(lymph) == 19)

  expect_true(ncol(rm_stat_test) == 12)
  expect_true(ncol(testing_data) == 12)

  bin_compas <- binarize_target(rm_stat_compas, 'Two_yr_Recidivism')
  bin_lisbon <- binarize_target(rm_stat_lisbon, 'Price')
  bin_iris   <- binarize_target(rm_stat_iris, 'Species')
  bin_adult  <- binarize_target(rm_stat_adult, 'salary')
  bin_lymph  <- binarize_target(rm_stat_lymph, 'class')
  bin_test   <- binarize_target(rm_stat_test, 'y')

  expect_true(length(bin_compas$Two_yr_Recidivism) == sum(as.integer(bin_compas$Two_yr_Recidivism %in% c(1, 2))))
  expect_true(is.null(levels(bin_lisbon$Price)))
  expect_true(length(levels(bin_iris$Species)) == 3)
  expect_true(length(bin_adult$salary) == sum(as.integer(bin_adult$salary %in% c(1, 2))))
  expect_true(length(levels(bin_lymph$class)) == 4)
  expect_true(is.null(levels(bin_test$y)))

  miss_compas <- manage_missing(bin_compas, 'Two_yr_Recidivism')
  suppressWarnings(miss_lisbon <- manage_missing(bin_lisbon, 'Price'))
  miss_iris   <- manage_missing(bin_iris, 'Species')
  miss_adult  <- manage_missing(bin_adult, 'salary')
  miss_lymph  <- manage_missing(bin_lymph, 'class')
  miss_test   <- manage_missing(bin_test, 'y')

  expect_true(ncol(miss_compas) == 7)
  expect_true(ncol(miss_lisbon) == 14)
  expect_true(ncol(miss_iris) == 5)
  expect_true(ncol(miss_adult) == 15)
  expect_true(ncol(miss_lymph) == 19)
  expect_true(ncol(miss_test) == 11)

  no_missing <- 'No target values are missing. \nNo predictor values are missing. '

  expect_output(check_missing(miss_compas, 'Two_yr_Recidivism'), no_missing)
  expect_output(check_missing(miss_lisbon, 'Price'), no_missing)
  expect_output(check_missing(miss_iris, 'Species'), no_missing)
  expect_output(check_missing(miss_adult, 'salary'), no_missing)
  expect_output(check_missing(miss_lymph, 'class'), no_missing)
  expect_output(check_missing(miss_test, 'y'), no_missing)

  expect_true(is.null(save_deleted_columns(compas, miss_compas)))
  expect_equal(save_deleted_columns(lisbon, miss_lisbon), c("Country", "District", "Municipality"))
  expect_true(is.null(save_deleted_columns(iris, miss_iris)))
  expect_true(is.null(save_deleted_columns(adult[1:1000,], miss_adult)))
  expect_true(is.null(save_deleted_columns(lymph, miss_lymph)))
  expect_true(save_deleted_columns(testing_data, miss_test) == 'X3')

})
