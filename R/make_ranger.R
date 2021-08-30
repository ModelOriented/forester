#' Automated function for explaining ranger model
#'
#' Function \code{make_ranger} creates ranger model with default parameters using ranger library and
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

make_ranger <- function(data, target, type, fill_na = FALSE, num_features = NULL, tune = FALSE, metric = NULL, iter = 20) {
  
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
  
  message("__________________________")
  message("CREATING MODEL")
  if (!tune){
    rg <- ranger::ranger(form, data = data, classification = is_classif)
  } else {
    # Checking the metric 
    metric <- check_metric(metric, type)

    # Argument for metrics which should be minimized 
    desc <- ifelse(metric %in% c("mse", "rmse", "mad"), -1, 1)
    
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
      score <- desc * calculate_metric(metric, predicted, data_val[[target]])
      
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
                                         n_iter = iter,
                                         acq = "ucb",
                                         kappa = 2.576,
                                         eps = 0.0,
                                         verbose = TRUE)
    
    best_params <- tuned_ranger$Best_Par
    
    rg <- ranger::ranger(form, data = data,
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
                                     label = "Ranger",
                                     predict_function = ranger_predict,
                                     verbose = 0)
  ranger_explained$modifications <- modifications
  class(ranger_explained) <- c("forester_model", "explainer")
  return(ranger_explained)
}
