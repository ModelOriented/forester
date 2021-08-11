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
  
  if (type == "classification") {
    
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
    
    # Creating model
    categorical <- which(sapply(apartments, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target),drop=FALSE],
                                   data[, target], cat_features = categorical)
    
    cat_model <- catboost::catboost.train(cat_data, params = list(verbose = 0))
    
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
    }
    
    #Creating a model
    categorical <- which(sapply(apartments, is.factor))
    cat_data <- catboost::catboost.load_pool(data[, -which(names(data) == target),drop = FALSE],
                                   data[, target], cat_features = categorical)
    
    cat_model <- catboost::catboost.train(cat_data, params = list(verbose = 0))
    
    ### Explainer
    catboost_explained <- DALEX::explain(cat_model,
                                       data = data[, -which(names(data) == target), drop = FALSE],
                                       y = data[, target], 
                                       label = "CatBoost",
                                       predict_function = catboost_predict,
                                       type = "regression")
  }
  
  ### S3 objects 
  class(catboost_explained) <- c("forester_model", "explainer")
  return(catboost_explained)
}
