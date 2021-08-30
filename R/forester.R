#' Automated Machine Learning Model Solver
#'
#' Different tree-based models such as: XGBoost, ranger, CatBoost, LightGBM, etc. require different syntaxes and
#' different specific data objects. This function provides a simple and unified formula to create those models
#' with options to automatically cover the whole process of creating Machine Learning Model: Preprocessing Data,
#' Feature Engineering, Creating Models, Optimizing Hyperparameters, Model Explanation and Evaluating Models.
#'
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - training data set to create model, if data_test = NULL, then data will be
#' automatically divided into training and testing dataset. NOTE: data has to contain the target column.
#' @param target character: name of the target column, should be character and has to be column name in data.
#' @param type character: defining the task. Two options are: "regression" and "classification", particularly, binary classification.
#' @param metric character, name of metric used for evaluating best model. For regression, options are: "mse", "rmse", "mad" and "r2".
#' For classification, options are: "auc", "recall", "precision", "f1" and "accuracy".
#' @param data_test optional argument, class of data.frame, matrix, data.table or dgCMatrix - test data set used for evaluating model performance.
#' @param remove_outliers logical, default is FALSE. If TRUE, all rows containing outliers in numeric columns will be removed, except for target column.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param scaling character, default is NULL. Parameter is used for scaling features. Options are "standardize", "minmax" and NULL.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#' @param tune logical. If TRUE, function will perform the hyperparameter tuning steps for each model inside.
#' @param iter number (default: 20) - total number of times the optimization step is to repeated. This argument is used when tune = TRUE.
#' @param verbose (default: 1) - verbosity of priting messages. Options are 0 (silent), 1 (warning), 2 (info), 3 (debug).
#'
#'
#' @return An object of the class \code{forester_model} which is the best model with respect to the
#' chosen matrix. It's also an object of the class \code{explainer} from DALEX family inherited the
#' explanation for the best chosen model.


forester <- function(data, target, type, metric = NULL, data_test = NULL, remove_outliers = FALSE, fill_na = TRUE, scaling = NULL, num_features = NULL, tune = FALSE, iter = 20, verbose = 1){

<<<<<<< Updated upstream
forester <- function(data, target, type, metric = NULL, data_test = NULL)
{
  
  ### If data_test is blank, it is needed to split data into data_train and data_test
  if (is.null(data_test))
  {
    if (type == "regression")
    {
      # Split data in ratio 4:1
      sample_size <- floor(0.8 * nrow(data))
      set.seed(123)
      train_index <- sample(seq_len(nrow(data)), size = sample_size)
      
      data_train <- data[train_index,]
      data_test  <- data[-train_index,]
    }
    
    if (type=="classification")
    {
      # Split data by ratio 4:1, while stratification is needed.
      uniq <- unique(data[[target]])
      data_negative <- data[data[[target]] == uniq[1],]
      data_positive <- data[data[[target]] == uniq[2],]
      
      set.seed(123)
      sample_size_pos <- floor(0.8 * nrow(data_positive))
      sample_size_neg <- floor(0.8 * nrow(data_negative))
      
      train_index_pos <- sample(seq_len(nrow(data_positive)),size = sample_size_pos)
      train_index_neg <- sample(seq_len(nrow(data_negative)),size = sample_size_neg)
      
      data_train <- rbind(data_positive[train_index_pos,], data_negative[train_index_neg,])
      data_test  <- rbind(data_positive[-train_index_pos,], data_negative[-train_index_neg,])
    }
  }
  
  ### Shuffering rows in data_train and data_test:
  rows_train <- sample(nrow(data_train))
  rows_test  <- sample(nrow(data_test))
  
  data_train <- data_train[rows_train,]
  data_test  <- na.omit(data_test[rows_test,])
  
  ### Creating models:
  ranger_exp   <- make_ranger(data_train, target, type)
  suppressMessages(catboost_exp <- make_catboost(data_train, target, type))
  suppressMessages(xgboost_exp  <- make_xgboost(data_train, target, type))
  suppressMessages(lightgbm_exp <- make_lightgbm(data_train, target, type))
  
  models <- list(catboost_exp, xgboost_exp, ranger_exp, lightgbm_exp)
  
  ### Processing target column in data_test
  if (type == "classification"){
      # Unquote quotation marks
      processed_label <- catboost_exp$y
      uniq_label <- unique(processed_label)
      data_test[[target]] <- ifelse(data_test[[target]] == data_train[[target]][1],
                                 uniq_label[1],
                                 uniq_label[2])
  }
=======
  message("__________________________")
  message("FORESTER")
  data <- check_conditions(data, target, type)
  
  ### If data_test is blank, it is needed to split data into data_train and data_test
  if (is.null(data_test)){
    splited_data <- split_data(data, target, type)
    data_train <- splited_data[[1]]
    data_test <- splited_data[[2]]
    
  } else {
    data_test <- check_conditions(data_test, target, type)
    data_train <- data
    
    # Check structure of data_test:
    if (!(setequal(colnames(data_train),colnames(data_test)))){
      stop("Column names in train data set and test data set are not identical.")
    }
  }
  
  ### Creating models:
  ranger_exp <- make_ranger(data = data_train, target = target, type = type,
                              tune = tune, metric = metric,
                              iter = iter, fill_na = fill_na)
  message("--- Ranger model has been created ---")
  suppressMessages(catboost_exp <- make_catboost(data = data_train, target = target, type = type,
                                                 tune = tune, metric = metric,
                                                 iter = iter, fill_na = fill_na))
  message("--- Catboost model has been created ---")
  suppressMessages(xgboost_exp  <- make_xgboost(data = data_train, target = target, type = type,
                                                tune = tune, metric = metric,
                                                iter = iter, fill_na = fill_na))
  message("--- Xgboost model has been created ---")
  suppressMessages(lightgbm_exp <- make_lightgbm(data = data_train, target = target, type = type,
                                                 tune = tune, metric = metric,
                                                 iter = iter, fill_na = fill_na))
  message("--- LightGBM model has been created ---")
  
  message("__________________________")
  message("COMPARISON")
>>>>>>> Stashed changes
  
  best_model <- evaluate(catboost_exp, xgboost_exp, ranger_exp, lightgbm_exp,
                               data_test = data_test, 
                               target = target,
                               metric = metric)
  
  return(best_model$best_model)
  
  return(best_model)
}


