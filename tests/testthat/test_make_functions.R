context("Check make functions")
library(data.table)

source("objects_for_tests.R")

test_make_functions <- function(make_function) {
  
  test_that(paste(substitute(make_function), ": An invalid data"), {
    expect_error(
      make_function(agaricus.train, "label", "classification"),
      "Object is not one of the types: 'data.frame','dgCMatrix','matrix','data.table"
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
    expect_s3_class(model, "explainer")
  })
  
  test_that(paste(substitute(make_function), ": A data.table as data"), {
    dt <- as.data.table(apartments)
    model <- make_function(dt, "m2.price", "regression")
    expect_s3_class(model, "explainer")
  })
  
  test_that(paste(substitute(make_function), ": A data.table as data"), {
    model <- make_function(agaricus_comb, "label", "classification")
    expect_s3_class(model, "explainer")
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
    expect_s3_class(model, "explainer")
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
    expect_s3_class(model, "explainer")
  })
  
  
  test_that(paste(substitute(make_function), ": Testing predict function"), {
    model <- make_function(iris_bin, "Species", "classification")
    expect_vector(predict(model, iris_bin_test))
  })
  
  
  # Test XGBoost 3 classification:
  test_that(paste(substitute(make_function), ": Testing model"), {
    expect_message(model <- make_function(titanic, "survived", "classification"))
    expect_s3_class(model, "explainer")
  })
  
  
  test_that(paste(substitute(make_function), ": Testing model"), {
    expect_error(
      make_function(titanic, "survived", "regression"),
      "Program is stopped. The class of target column is factor, not appropriate for regression problem"
    )
  })
  
}

### Testing all make_*() functions
test_make_functions(make_lightgbm)
test_make_functions(make_xgboost)
test_make_functions(make_catboost)
test_make_functions(make_ranger)

### Additional test for ranger function
test_that("make_ranger : Creating formula with dpecial characters", {
  expect_message(model <- make_ranger(agaricus.train$data, "cap-shape=bell", "classification"))
})

### Additional test for catboost function
test_that("make_ranger : Creating formula with dpecial characters", {
  titanic_strange_numbers <- titanic_imputed
  titanic_strange_numbers$survived <- ifelse(titanic_imputed$survived == 1, 100, 0)
  expect_message(model <- make_catboost(titanic_strange_numbers, "survived", "classification"))
})
