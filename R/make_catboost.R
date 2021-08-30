#' Automated function for explaining catboost models
#' 
#' Function \code{make_catboost} creates catboost model with default parameters using ranger library and
#' wraps it with the explainer.
#' 
#' @param data training set for ranger model. It can be data frame, matrix, data.table or dgCMatrix. Note: data should contain target column
#' @param target name of a target value. Should be character. Has to be in data frame
#' @param type specify weather it is classification task or regression. Should be one of these characters: "classif", "regr"
#'
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

make_catboost <-function(data, target, type, fill_na = TRUE, num_features = NULL, tune = FALSE, metric = NULL, iter = 20) {

  # Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications

  ### Tuning step 
  if (!tune){
    # Creating model 
    categorical <- which(sapply(apartments, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target),drop = FALSE],
                                             data[, target], cat_features = categorical)
    cat_model <- catboost::catboost.train(cat_data, params = list(verbose = 0))
    
  } else {
    # Checking the metric 
    metric <- check_metric(metric, type)
    
    # Argument for metrics which should be minimized 
    desc <- ifelse(metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    # Creating validation set in ratio 4:1
    splited_data <- split_data(data, target, type)
    data <- splited_data[[1]]
    data_val <- splited_data[[2]]
    
    # Creating pool objects for catboost 
    categorical <- which(sapply(apartments, is.factor))
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
      score <- desc * calculate_metric(metric, predicted, data_val[[target]])
      
      list(Score = score, Pred = predicted)
    }
    
    ### Tuning process
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
                                           n_iter = iter,
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
                                       label = "CatBoost",
                                       predict_function = catboost_predict,
                                       type = type,
                                       verbose = 0)
  
  ### S3 objects
  catboost_explained$modifications <- modifications
  class(catboost_explained) <- c("forester_model", "explainer")
  return(catboost_explained)
}
