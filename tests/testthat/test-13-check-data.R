test_that('test-check-data', {
  datasets <- list(lisbon, testing_data, iris, compas)
  targets  <- c('Price', 'y', 'Species', 'Two_yr_Recidivism')
  types    <- c('regression', 'regression', 'multiclass', 'binary_clf')
  verbose  <- TRUE
  time     <- NULL
  status   <- NULL
  for (i in 1:length(datasets)) {
    df   <- datasets[[i]]
    y    <- targets[i]
    type <- types[i]

    expect_output(basic_info(df, y, time, status, verbose))
    expect_output(check_static(df, verbose))
    expect_output(check_duplicate_col(df, verbose))
    expect_output(check_missing(df, y, time, status, verbose))
    expect_no_error(df  <- manage_missing(df, y))
    expect_output(check_dim(df, verbose))
    expect_output(check_cor(df, y, time, status, verbose))
    expect_output(check_outliers(df, verbose))
    expect_output(check_y_balance(df, y, time, status, type, verbose))
    expect_output(detect_id_columns(df, verbose))
  }
})
