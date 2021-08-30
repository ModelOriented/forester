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

<<<<<<< Updated upstream
make_catboost <-function(data, target, type) {
  ### Conditions
  # Checking data class
  if (!any(class(data) %in% c("data.frame", "dgCMatrix", "matrix", "data.table"))){
    stop("Object is not one of the types: 'data.frame', 'dgCMatrix', 'matrix', 'data.table")
  }
  
  # Changing data to normal data frame
  if (any(class(data) != "data.frame")){
    if (any(class(data) == 'dgCMatrix')){
        data <- as.data.frame(as.matrix(data))
    } else if (any(class(data) == 'matrix')){
        data <- as.data.frame(data)
    } else if (any(class(data) == 'data.table')){
        data <- as.data.frame(data)
    }
  }
  
  
  ### Data processing level 1/2 (remove NAs, split label and training frame,...)
  # Remove rows with NA values (I will write in the documentation of function):
  data <- na.omit(data)
  
  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }
  
  # Changing characters columns to factors 
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],
                                             as.factor)
  
  # Checking if target column is in data frame
  if (typeof(target) != 'character' | !(target %in% colnames(data))) {
    stop("Either 'target' input is not a character or data does not have column named similar to 'target'")
  }
  
  # Checking if type is correct
  if (typeof(type) != 'character' | (type != "classification" & type != "regression")){
    stop("Type of problem is invalid.")
  }
  
  
  ### Model
  # First binary classification
=======
make_catboost <-function(data, target, type, fill_na = TRUE, num_features = NULL, tune = FALSE, metric = NULL, iter = 20) {

  # Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications
>>>>>>> Stashed changes
  
  ### Tuning step 
  if (!tune){
    # Creating model 
    categorical <- which(sapply(apartments, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target),drop = FALSE],
                                             data[, target], cat_features = categorical)
    cat_model <- catboost::catboost.train(cat_data, params = list(verbose = 0))
    
<<<<<<< Updated upstream
    # Custom predict function for classification
    catboost_predict <- function(object, newdata) {
      newdata_pool <- catboost::catboost.load_pool(newdata)
      return( catboost::catboost.predict(object, newdata_pool, prediction_type = "Probability"))
    }
    
    # Checking if theres right number of classes in target column
    if (length(unique(data[, target])) < 2) {
      stop("Too few classes for binary classification")
    } else if (length(unique(data[, target])) > 2){
      stop("Too many classes for binary classification")
    }
    
    if (class(data[[target]]) != "numeric") {
      # Converting target column to numeric for Dalex
      uniq <- unique(data[, target])
      data[, target] <- ifelse(data[, target] == uniq[1], 0, 1)
      message(paste("Wrong type of target column. Changed to numeric: ",
                    uniq[1], " -> 1 and ", uniq[2], " -> 0 ", sep = ""))
    }
    
    if (!(any(unique(data[, target]) == 1) & any(unique(data[, target]) == 0))){
      uniq <- unique(data[, target])
      data[, target] <- ifelse(data[, target] == uniq[1], 0, 1)
      message(paste("Changing classe' numbers to 0 and 1: ",
                    uniq[1], " -> 1 and ", uniq[2], " -> 0 ", sep = ""))
    }
=======
  } else {
    # Checking the metric 
    metric <- check_metric(metric, type)
    
    # Argument for metrics which should be minimized 
    desc <- ifelse(metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    # Creating validation set in ratio 4:1
    splited_data <- split_data(data, target, type)
    data <- splited_data[[1]]
    data_val <- splited_data[[2]]
>>>>>>> Stashed changes
    
    # Creating pool objects for catboost 
    categorical <- which(sapply(apartments, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target), drop = FALSE],
                                             data[, target], cat_features = categorical)
    
    cat_data_val <- catboost::catboost.load_pool(data_val[, -which(names(data_val) == target), drop = FALSE],
                                                 data_val[, target], cat_features = categorical)
    
<<<<<<< Updated upstream
    ### Explainer
    catboost_explained <- DALEX::explain(cat_model,
                                       data = data[, -which(names(data) == target),drop=FALSE],
                                       y = data[, target], 
                                       label = "CatBoost",
                                       predict_function = catboost_predict,
                                       type = "classification",
                                       verbose = 0)
  } else {
    # Custom predict function for regression
    catboost_predict <- function(object, newdata) {
      newdata_pool <- catboost::catboost.load_pool(newdata)
      return( catboost::catboost.predict(object, newdata_pool))
    }
    # Checking if target column is numeric
    if (class(data[[target]]) != 'numeric' & class(data[[target]]) != 'integer') {
      stop("Program is stopped. The class of target column is factor, not appropriate for regression problem")
=======
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
>>>>>>> Stashed changes
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
