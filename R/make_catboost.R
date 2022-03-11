#' Automated function for explaining catboost models
#' 
#' Function \code{make_catboost} creates catboost model with default parameters using ranger library and
#' wraps it with the explainer.
#' 
#' @param data data.frame, matrix, data.table or dgCMatrix - data will be used to run the catboost model. NOTE: data has to contain the target column.
#' @param target character: name of the target column, should be character and has to be a column name in data.
#' @param type character: defining the task. Two options are: "regression" and "classification", particularly, binary classification.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#' @param tune logical. If TRUE, function will perform the hyperparameter tuning steps for each model inside.
#' @param tune_metric character, name of metric used for evaluating best model. For regression, options are: "mse", "rmse", "mad" and "r2".
#' For classification, options are: "auc", "recall", "precision", "f1" and "accuracy".
#' @param tune_iter number (default: 20) - total number of times the optimization step is to repeated. This argument is used when tune = TRUE.
#' @param label string indicating the name of the model. Might be usefull while comparing different models of the same type.
#'
#' @return An object of the class \code{explainer}.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @importFrom stats median weighted.mean
#' @export
#' @examples
#' \donttest{
#' # regression
#' library(DALEX)
#' data(apartments, package="DALEX")
#'
#' exp <- make_catboost(apartments, "m2.price", "regression")
#' # plot(model_parts(exp))
#'
#' # binary classification
#' library(DALEX)
#' data(titanic_imputed, package="DALEX")
#' exp <- make_catboost(titanic_imputed, "survived", "classification")
#' # plot(model_parts(exp))
#'}
##

make_catboost <-function(data, target, type, fill_na = TRUE, num_features = NULL, tune = FALSE, tune_metric = NULL, tune_iter = 20, label = "Catboost") {
  
  message("--- Creating Catboost model ---")
  # Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications

  ### Tuning step 
  if (!tune){
    # Creating model 
    categorical <- which(sapply(data, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target),drop = FALSE],
                                             data[, target], cat_features = categorical)
    cat_model <- catboost::catboost.train(cat_data, params = list(verbose = 0))
    
  } else {
    # Checking the metric 
    tune_metric <- check_metric(tune_metric, type)
    
    # Argument for metrics which should be minimized 
    desc <- ifelse(tune_metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    # Creating validation set in ratio 4:1
    splited_data <- split_data(data, target, type)
    data <- splited_data[[1]]
    data_val <- splited_data[[2]]
    
    # Creating pool objects for catboost 
    categorical <- which(sapply(data, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target), drop = FALSE],
                                             data[, target], cat_features = categorical)
    
    cat_data_val <- catboost::catboost.load_pool(data_val[, -which(names(data_val) == target), drop = FALSE],
                                                 data_val[, target], cat_features = categorical)
    
    ### Preparing tuning function 
    catboost_tune_fun <- function(iterations, depth, learning_rate, random_strength, bagging_temperature, border_count, l2_leaf_reg){
      # Model for evaluating hyperparameters
      catboost_tune <- catboost::catboost.train(cat_data,
                                                params = list(verbose = 0,
                                                              iterations = iterations,
                                                              depth = depth,
                                                              learning_rate = learning_rate,
                                                              random_strength = random_strength,
                                                              bagging_temperature = bagging_temperature,
                                                              border_count = border_count,
                                                              l2_leaf_reg = l2_leaf_reg))
      
      # Evaluating model
      predicted <- catboost::catboost.predict(catboost_tune, cat_data_val)
      if (type == "classification"){
        predicted <- ifelse(predicted >= 0.5, 1, 0)
      }
      score <- desc * calculate_metric(tune_metric, predicted, data_val[[target]])
      
      list(Score = score, Pred = predicted)
    }
    
    ### Tuning process
    message("--- Starting tuning process")
    tuned_catboost <- rBayesianOptimization::BayesianOptimization(catboost_tune_fun,
                                           bounds = list(iterations = c(10L, 1000L),
                                                         depth = c(1L, 8L),
                                                         learning_rate = c(0.01, 1.0),
                                                         random_strength = c(1e-9, 10),
                                                         bagging_temperature = c(0.0, 1.0),
                                                         border_count = c(1L, 255L),
                                                         l2_leaf_reg = c(2L, 30L)),
                                           init_grid_dt = NULL,
                                           init_points = 10,
                                           n_iter = tune_iter,
                                           acq = "ucb",
                                           kappa = 2.576,
                                           eps = 0.0,
                                           verbose = TRUE)
    
    # Best hyperparameters
    catboost_params <- append(tuned_catboost$Best_Par, list(verbose = 0))
    
    # Creating final model 
    cat_model <- catboost::catboost.train(cat_data, params = catboost_params)
    
  }
  
  
  # Custom predict function for classification
  cat_model$modifications <- modifications
  catboost_predict <- function(object, newdata) {
    # Changing data type to data frame 
    newdata <- check_conditions(newdata)
    
    # Filling NA in test set
    rec <- object$modifications

    newdata <- as.data.frame(recipes::bake(rec, newdata))
    
    newdata_pool <- catboost::catboost.load_pool(newdata)
    pred <- catboost::catboost.predict(object, newdata_pool)
    if (type == "classification"){
      return( ifelse(pred >= 0.5, 1, 0))
    } else {
        return(pred)
    }
  }
    
  
  # Creating an eplainer
  catboost_explained <- DALEX::explain(cat_model,
                                       data = data[, -which(names(data) == target),drop=FALSE],
                                       y = data[, target], 
                                       label = label,
                                       predict_function = catboost_predict,
                                       type = type,
                                       verbose = 0)
  
  ### S3 objects
  catboost_explained$modifications <- modifications
  class(catboost_explained) <- c("forester_model", "explainer")
  return(catboost_explained)
}
