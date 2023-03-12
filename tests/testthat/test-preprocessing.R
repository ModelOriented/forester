test_that('test-preprocessing', {
  prep_compas    <- preprocessing(compas, 'Two_yr_Recidivism', type = 'binary_clf')
  suppressWarnings(
    prep_lisbon  <- preprocessing(lisbon, 'Price', type = 'regression')
    )
  prep_iris      <- preprocessing(iris[1:100, ], 'Species')
  prep_adult     <- preprocessing(adult[1:1000, ], 'salary', type = 'binary_clf')
  prep_lymph     <- preprocessing(lymph, 'class', type = 'binary_clf')
  prep_test      <- preprocessing(testing_data, 'y', type = 'regression')

  rm_stat_compas <- pre_rm_static_cols(compas, 'Two_yr_Recidivism')
  rm_stat_lisbon <- pre_rm_static_cols(lisbon, 'Price')
  rm_stat_iris   <- pre_rm_static_cols(iris, 'Species')
  rm_stat_adult  <- pre_rm_static_cols(adult[1:1000, ], 'salary')
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

  bin_compas <- binarize_target(rm_stat_compas, 'Two_yr_Recidivism')$bin_data
  bin_lisbon <- binarize_target(rm_stat_lisbon, 'Price')$bin_data
  bin_iris   <- binarize_target(rm_stat_iris, 'Species')$bin_data
  bin_adult  <- binarize_target(rm_stat_adult, 'salary')$bin_data
  bin_lymph  <- binarize_target(rm_stat_lymph, 'class')$bin_data
  bin_test   <- binarize_target(rm_stat_test, 'y')$bin_data

  expect_true(length(bin_compas$Two_yr_Recidivism) == sum(as.integer(bin_compas$Two_yr_Recidivism %in% c(1, 2))))
  expect_true(is.null(levels(bin_lisbon$Price)))
  expect_true(length(levels(bin_iris$Species)) == 3)
  expect_true(length(bin_adult$salary) == sum(as.integer(bin_adult$salary %in% c(1, 2))))
  expect_true(length(levels(bin_lymph$class)) == 4)
  expect_true(is.null(levels(bin_test$y)))

  labels_compas <- binarize_target(rm_stat_compas, 'Two_yr_Recidivism')$labels
  labels_lisbon <- binarize_target(rm_stat_lisbon, 'Price')$labels
  labels_iris   <- binarize_target(rm_stat_iris, 'Species')$labels
  labels_adult  <- binarize_target(rm_stat_adult, 'salary')$labels
  labels_lymph  <- binarize_target(rm_stat_lymph, 'class')$labels
  labels_test   <- binarize_target(rm_stat_test, 'y')$labels

  expect_equal(labels_compas, c('0', '1'))
  expect_true(is.null(labels_lisbon))
  expect_true(is.null(labels_iris))
  expect_equal(labels_adult, c('<=50K', '>50K'))
  expect_true(is.null(labels_lymph))
  expect_true(is.null(labels_test))

  miss_compas   <- manage_missing(bin_compas, 'Two_yr_Recidivism')
  suppressWarnings(
    miss_lisbon <- manage_missing(bin_lisbon, 'Price')
  )
  miss_iris     <- manage_missing(bin_iris, 'Species')
  miss_adult    <- manage_missing(bin_adult, 'salary')
  miss_lymph    <- manage_missing(bin_lymph, 'class')
  miss_test     <- manage_missing(bin_test, 'y')

  expect_true(ncol(miss_compas) == 7)
  expect_true(ncol(miss_lisbon) == 14)
  expect_true(ncol(miss_iris) == 5)
  expect_true(ncol(miss_adult) == 15)
  expect_true(ncol(miss_lymph) == 19)
  expect_true(ncol(miss_test) == 11)

  del_cor_compas <- delete_correlated_values(miss_compas, 'Two_yr_Recidivism', verbose = FALSE)$data
  del_cor_lisbon <- delete_correlated_values(miss_lisbon, 'Price', verbose = FALSE)$data
  del_cor_iris   <- delete_correlated_values(miss_iris, 'Species', verbose = FALSE)$data
  del_cor_adult  <- delete_correlated_values(miss_adult, 'salary', verbose = FALSE)$data
  del_cor_lymph  <- delete_correlated_values(miss_lymph, 'class', verbose = FALSE)$data
  del_cor_test   <- delete_correlated_values(miss_test, 'y', verbose = FALSE)$data

  expect_true(ncol(del_cor_compas) == 7)
  expect_true(ncol(del_cor_lisbon) == 11)
  expect_true(ncol(del_cor_iris) == 3)
  expect_true(ncol(del_cor_adult) == 15)
  expect_true(ncol(del_cor_lymph) == 19)
  expect_true(ncol(del_cor_test) == 11)

  del_id_compas <- delete_id_columns(del_cor_compas)
  del_id_lisbon <- delete_id_columns(del_cor_lisbon)
  del_id_iris   <- delete_id_columns(del_cor_iris)
  del_id_adult  <- delete_id_columns(del_cor_adult)
  del_id_lymph  <- delete_id_columns(del_cor_lymph)
  del_id_test   <- delete_id_columns(del_cor_test)

  expect_true(ncol(del_id_compas) == 7)
  expect_true(ncol(del_id_lisbon) == 10)
  expect_true(ncol(del_id_iris) == 3)
  expect_true(ncol(del_id_adult) == 15)
  expect_true(ncol(del_id_lymph) == 19)
  expect_true(ncol(del_id_test) == 11)

  set.seed(123)
  boruta_compas <- boruta_selection(del_id_compas, 'Two_yr_Recidivism')
  boruta_lisbon <- boruta_selection(del_id_lisbon, 'Price')
  boruta_iris   <- boruta_selection(del_id_iris, 'Species')
  boruta_adult  <- boruta_selection(del_id_adult, 'salary')
  boruta_lymph  <- boruta_selection(del_id_lymph, 'class')
  boruta_test   <- boruta_selection(del_id_test, 'y')

  expect_true(ncol(boruta_compas) == 7)
  expect_true(ncol(boruta_lisbon) == 10)
  expect_true(ncol(boruta_iris) == 3)
  expect_true(ncol(boruta_adult) == 13)
  expect_true(ncol(boruta_lymph) == 15)
  expect_true(ncol(boruta_test) == 5)

  expect_true(is.null(save_deleted_columns(compas, del_id_compas)))
  expect_equal(save_deleted_columns(lisbon, del_id_lisbon), c('Id', 'PropertyType', 'AreaNet', 'AreaGross', 'Country', 'District', 'Municipality'))
  expect_equal(save_deleted_columns(iris, del_id_iris), c('Petal.Length', 'Petal.Width'))
  expect_true(is.null(save_deleted_columns(adult[1:1000,], del_id_adult)))
  expect_true(is.null(save_deleted_columns(lymph, del_id_lymph)))
  expect_true(save_deleted_columns(testing_data, del_id_test) == 'X3')

  prep_compas   <- preprocessing(compas, 'Two_yr_Recidivism', TRUE, FALSE)
  prep_iris     <- preprocessing(iris, 'Species', TRUE, FALSE)
  prep_adult    <- preprocessing(adult[1:1000,], 'salary', TRUE, FALSE)
  prep_lymph    <- preprocessing(lymph, 'class', TRUE, FALSE)
  prep_test     <- preprocessing(testing_data, 'y', TRUE, FALSE)
  suppressWarnings(
    prep_lisbon <- preprocessing(lisbon, 'Price', TRUE, FALSE)
  )
})
