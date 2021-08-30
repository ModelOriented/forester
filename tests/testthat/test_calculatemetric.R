context("Check calculate_metric() function")

x <- sample(10,10)
y <- sample(10,10)

  # Metrics in regression problem:
  test_that("Numeric return value for mse", {
    expect_length(calculate_metric("mse",x,y), 1)
  })
  
  test_that("Numeric return value for rmse", {
    expect_length(calculate_metric("rmse",x,y), 1)
  })
  
  test_that("Numeric return value for r2", {
    expect_length(calculate_metric("r2",x,y), 1)
  })
  
  test_that("Numeric return value for mad", {
    expect_length(calculate_metric("mad",x,y), 1)
  })
  
  
  
  # Metrics in classification problem:
  test_that("Numeric return value for recall", {
    expect_length(calculate_metric("recall",x,y), 1)
  })
  
  
  test_that("Numeric return value for precision", {
    expect_length(calculate_metric("precision",x,y), 1)
  })
  
  
  test_that("Numeric return value for f1", {
    expect_length(calculate_metric("f1",x,y), 1)
  })
  
  test_that("Numeric return value for accuracy", {
    expect_length(calculate_metric("accuracy",x,y), 1)
  })
  
  test_that("Numeric return value for auc", {
    expect_length(calculate_metric("auc",x,y), 1)
  })
  
  