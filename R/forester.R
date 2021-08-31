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
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#' @param tune logical. If TRUE, function will perform the hyperparameter tuning steps for each model inside.
#' @param tune_iter number (default: 20) - total number of times the optimization step is to repeated. This argument is used when tune = TRUE.
#'
#'
#' @return An object of the class \code{forester_model} which is the best model with respect to the
#' chosen metric. It's also an object of the class \code{explainer} from DALEX family inherited the
#' explanation for the best chosen model.
#' 
#' @export
#' @importFrom stats predict
#' @examples
#' \donttest{
#' # regression
#' library(DALEX)
#' data(apartments, package="DALEX")
#'
#' exp <- forester(apartments, "m2.price", "regression")
#' # plot(model_parts(exp))
#'
#' # binary classification
#' library(DALEX)
#' data(titanic_imputed, package="DALEX")
#' 
#' exp <- forester(titanic_imputed, "survived", "classification")
#' # plot(model_parts(exp))
#'}
##


forester <- function(data, target, type, metric = NULL, data_test = NULL, fill_na = TRUE, num_features = NULL, tune = FALSE, tune_iter = 20){

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
                              tune = tune, tune_metric = metric,
                            tune_iter = tune_iter, fill_na = fill_na)
  message("--- Ranger model has been created ---")
  suppressMessages(catboost_exp <- make_catboost(data = data_train, target = target, type = type,
                                                 tune = tune, tune_metric = metric,
                                                 tune_iter = tune_iter, fill_na = fill_na))
  message("--- Catboost model has been created ---")
  suppressMessages(xgboost_exp  <- make_xgboost(data = data_train, target = target, type = type,
                                                tune = tune, tune_metric = metric,
                                                tune_iter = tune_iter, fill_na = fill_na))
  message("--- Xgboost model has been created ---")
  suppressMessages(lightgbm_exp <- make_lightgbm(data = data_train, target = target, type = type,
                                                 tune = tune, tune_metric = metric,
                                                 tune_iter = tune_iter, fill_na = fill_na))
  message("--- LightGBM model has been created ---")
  
  message("__________________________")
  message("COMPARISON")
  
  best_model <- evaluate(catboost_exp, xgboost_exp, ranger_exp, lightgbm_exp,
                               data_test = data_test, 
                               target = target,
                               metric = metric)
  
  return(best_model$best_model)
  
  return(best_model)
}


