#' Automated function for explaining ranger model
#'
#' Function \code{make_ranger} creates ranger model with default parameters using ranger library and
#' wraps it with the explainer.
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - data will be used to run the XGBoost model. NOTE: data has to contain the target column.
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
#' exp <- make_ranger(apartments, "m2.price", "regression")
#' # plot(model_parts(exp))
#'
#' # binary classification
#' library(DALEX)
#' data(titanic_imputed, package="DALEX")
#' exp <- make_ranger(titanic_imputed, "survived", "classification")
#' # plot(model_parts(exp))
#'}
##

make_ranger <- function(data, target, type, fill_na = FALSE, num_features = NULL, tune = FALSE, tune_metric = NULL, tune_iter = 20, label = "Ranger") {
  
  message("--- Creating Ranger model ---")
  # Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications
  
  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }
  
  # Checking if target column is in data frame
  if (typeof(target) != 'character' | !(target %in% colnames(data))) {
    stop("Either 'target' input is not a character or data does not have column named similar to 'target'")
  }
  
  # Checking if type is correct
  if (typeof(type) != 'character' | (type != "classification" & type != "regression")) {
    stop("Type of problem is invalid.")
  }
  
  # Creating formula
  # try to convert column names to names without znaki
  test_colnames <- lapply(colnames(data), function(x) gsub("_", "", x))
  test_colnames <- lapply(test_colnames, function(x) gsub("[.]", "", x))
  if (any(grepl('[[:punct:]]', test_colnames))) {
    stop("Column names are wrong for creating a formula. Column names can not contain special characters as '+', '-', '=', '*'. Try substitute special characters with '_' sign.")
  }
  form <- stats::as.formula(paste(target , "~."))
  
  is_classif <- type == "classification"
  
  if (!tune){
    rg <- ranger::ranger(form, data = data, classification = is_classif)
  } else {
    message('--- Starting tuning process')
    # Checking the metric 
    tune_metric <- check_metric(tune_metric, type)

    # Argument for metrics which should be minimized 
    desc <- ifelse(tune_metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    # Creating validation set in ratio 4:1
    splited_data <- split_data(data, target, type)
    data <- splited_data[[1]]
    data_val <- splited_data[[2]]
    
    ### Preparing tuning function 
    ranger_tune_fun <- function(num.trees, sample.fraction, mtry, min.node.size, replace, respect.unordered.factors){
      # Creating model for evaluating hyperparameters
      ranger_tune <- ranger::ranger(form, data,
                                    num.trees = num.trees,
                                    mtry = ceiling(mtry * (ncol(data) - 1)),
                                    min.node.size = ceiling(nrow(data) ^ min.node.size),
                                    sample.fraction = sample.fraction,
                                    replace = as.logical(replace),
                                    respect.unordered.factors = as.logical(respect.unordered.factors),
                                    classification = is_classif)
      
      # Evaluating hyperparameters
      predicted <- predict(ranger_tune, data_val[, -which(names(data_val) == target)])$predictions
      score <- desc * calculate_metric(tune_metric, predicted, data_val[[target]])
      
      list(Score = score, Pred = predicted)
    }
    
    ### Tuning process
    tuned_ranger <- rBayesianOptimization::BayesianOptimization(ranger_tune_fun,
                                         bounds = list(num.trees = c(1L, 2000L),
                                                       mtry = c(0, 1),
                                                       min.node.size = c(0, 1),
                                                       sample.fraction = c(0.1, 1),
                                                       respect.unordered.factors = c(0L, 1L),
                                                       replace = c(0L, 1L)),
                                         init_grid_dt = NULL,
                                         init_points = 10,
                                         n_iter = tune_iter,
                                         acq = "ucb",
                                         kappa = 2.576,
                                         eps = 0.0,
                                         verbose = TRUE)
    
    best_params <- tuned_ranger$Best_Par
    
    # After choosing best parameters from BayesianOptimization method, data loss prevention by using the original data_train
    # to train the model, which is in this case, data + data_val, since on the line 89, data was modified.
    rg <- ranger::ranger(form, data = rbind(data,data_val),
                         num.trees = best_params["num.trees"],
                         mtry = ceiling(best_params["mtry"] * (ncol(data) - 1)),
                         min.node.size = ceiling(nrow(data) ^ best_params["min.node.size"]),
                         sample.fraction = best_params["sample.fraction"],
                         respect.unordered.factors = as.logical(best_params["respect.unordered.factors"]),
                         replace = as.logical(best_params["replace"]),
                         classification = is_classif)
  }
  
  ### Explainer
  # Deleting target column from data frame
  df_exp <- data[,-which(names(data) == target), drop = FALSE]
  
  ### Creating an explainer
  # Custom predict function for regression
  ranger_predict <- function(object, newdata) {
    # Changing data type to data frame 
    newdata <- check_conditions(newdata)
    
    # Applying modifications
    rec <- object$modifications
    newdata <- as.data.frame(recipes::bake(rec, newdata))
    
    return( predict(object, newdata)$predictions )
  }
  
  rg$modifications <- modifications
  ranger_explained <- DALEX::explain(rg,
                                     data = df_exp,
                                     y = data[, target],
                                     label = label,
                                     predict_function = ranger_predict,
                                     verbose = 0)
  ranger_explained$modifications <- modifications
  class(ranger_explained) <- c("forester_model", "explainer")
  return(ranger_explained)
}
