context("Check make functions")

source("objects_for_tests.R")

test_make_functions <- function(make_function) {
  
  test_that(paste(substitute(make_function), ": An invalid data"), {
    expect_error(
      make_function(agaricus.train, "label", "classification"),
      "Object is not one of the types: 'data.frame', 'dgCMatrix', 'matrix', 'data.table"
    )
  })
  
  
  test_that(paste(substitute(make_function), ": An invalid data"), {
    expect_error(
      make_function(label_agaricus, "agaricus.train$label", "classification"),
      "The data frame is empty or has too little columns."
    )
  })
  
  test_that(paste(substitute(make_function), ": A matrix as data"), {
    m <- as.matrix(titanic_imputed[c("age", "fare", "sibsp", "parch", "survived")])
    model <- make_function(m, "survived", "classification")
    expect_s3_class(model, "forester_model")
  })
  
  test_that(paste(substitute(make_function), ": A data.table as data"), {
    dt <- data.table::as.data.table(apartments)
    model <- make_function(dt, "m2.price", "regression")
    expect_s3_class(model, "forester_model")
  })
  
  test_that(paste(substitute(make_function), ": A dgCMatrix as data"), {
    model <- make_function(agaricus_comb, "label", "classification")
    expect_s3_class(model, "forester_model")
  })
  
  # Test XGBoost 1 regression:
  test_that(paste(substitute(make_function), ": An invalid target column"), {
    expect_error(
      make_function(apartments, "car_garage", "classification"),
      "Either 'target' input is not a character or data does not have column named similar to 'target'"
    )
  })
  
  
  test_that(paste(substitute(make_function), ": An invalid target column"), {
    expect_error(
      make_function(apartments, "m2.price", "clustering"),
      "Type of problem is invalid."
    )
  })
  
  
  test_that(paste(substitute(make_function), ": Out of scope of binary classification"), {
    expect_error(
      make_function(apartments, "district", "classification"),
      "Too many classes for binary classification"
    )
  })
  
  test_that(paste(substitute(make_function), ": Out of scope of binary classification"), {
    iris_one <- iris[iris$Species %in% c("setosa"), ]
    iris_one$Species <- as.factor(iris_one$Species)
    expect_error(make_function(iris_one, "Species", "classification"),
      "Too few classes for binary classification"
    )
  })
  
  test_that(paste(substitute(make_function), ": Testing model"), {
    model <- make_function(apartments, "m2.price", "regression")
    expect_s3_class(model, "forester_model")
  })
  
  
  test_that(paste(substitute(make_function), ": Testing predict function"), {
    model <- make_function(apartments, "m2.price", "regression")
    expect_vector(predict(model, apartments_test))
  })
  
  
  # Test XGBoost 2 classification:
  test_that(paste(
    substitute(make_function),
    ": Coercing value to factor column and return with message:"
  ),
  {
    expect_message(model <- make_function(iris_bin, "Species", "classification"))
    expect_s3_class(model, "forester_model")
  })
  
  
  test_that(paste(substitute(make_function), ": Testing predict function"), {
    model <- make_function(iris_bin, "Species", "classification")
    expect_vector(predict(model, iris_bin_test))
  })
  
  
  # Test XGBoost 3 classification:
  test_that(paste(substitute(make_function), ": Testing model"), {
    expect_message(model <- make_function(titanic, "survived", "classification"))
    expect_s3_class(model, "forester_model")
  })
  
  
  test_that(paste(substitute(make_function), ": Testing model"), {
    expect_error(
      make_function(titanic, "survived", "regression"),
      "Program is stopped. The class of target column is factor, not appropriate for regression problem"
    )
  })
  
  test_that(paste(substitute(make_function), ": With and without filling NA"), {
    model <- make_function(titanic, "survived", "classification", fill_na = TRUE)
    expect_s3_class(model, "forester_model")
    model <- make_function(titanic, "survived", "classification", fill_na = FALSE)
    expect_s3_class(model, "forester_model")
  })
  
  test_that(paste(substitute(make_function), ": With feature selection"), {
    model <- make_function(titanic, "survived", "classification", fill_na = TRUE, num_features = 3)
    expect_s3_class(model, "forester_model")
    model <- make_function(titanic, "survived", "classification", num_features = 3)
    expect_s3_class(model, "forester_model")
  })
  
  test_that(paste(substitute(make_function), ": With tuning"), {
    model <- make_function(titanic, "survived", "classification", tune = TRUE, iter = 1)
    expect_s3_class(model, "forester_model")
    model <- make_function(apartments, "m2.price", "regression", tune = TRUE, iter = 1, metric = "mse")
    expect_s3_class(model, "forester_model")
  })
}

### Testing all make_*() functions
test_make_functions(make_lightgbm)
test_make_functions(make_xgboost)
test_make_functions(make_catboost)
test_make_functions(make_ranger)

test_that("Column names with special characters ", {
  apartments_wrong_colnames <- apartments
  colnames(apartments_wrong_colnames)[2] <- "construction-year"
  expect_error(make_ranger(apartments_wrong_colnames, "m2.price", "regression"))
  expect_error(make_xgboost(apartments_wrong_colnames, "m2.price", "regression"))
})


### Additional test for prepare data function
test_that("Testing imputation", {
  f <- file()
  lines <- c("3", "0", "1", "2")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  options("mypkg.connection" = f)
  
  expect_message(model <- make_ranger(titanic_imbalanced, "survived","classification"))
  expect_message(model <- make_ranger(titanic_imbalanced, "survived", "classification"))
  expect_message(model <- make_ranger(titanic_imbalanced, "survived", "classification"))
  options("mypkg.connection" = stdin())
  close(f)
})

test_that("Wrong starting arguments for prepare function", {
  expect_error(make_ranger(apartments, "m2.price", "regression", fill_na = "a"),
               "Argument fill_na should be a logical variable: TRUE or FALSE")
  expect_error(make_ranger(apartments, "m2.price", "regression", num_features = "a"),
               "Argument num_features should be a numerical.")
})







