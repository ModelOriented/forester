#' Function for comparing models 
#' 
#' Function \code{compare_models} enables user to compare various model with
#' the same type of task. Function compares them based on metric which can be chosen by the user 
#' and returns the best model 
#' 
#' @param models list of models to compare. Models shoud be either \code{forester_model} or \code{explainer} object.
#' @param data_test data frame for evaluation models
#' @param target character indicating the target column in data test table
#' @param metric string containing name of mertic based on which the model will be selected. 
#' Metric shoud be written in small letters. Can be omited.
#'
#'
#' @return An object of the class \code{forester_model}.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @importFrom stats median weighted.mean
#' @export
#' @examples
#' \donttest{
#' library(DALEX)
#' data(apartments, package="DALEX")
#' 
#' lightgbm_explained <- make_lightgbm(apartments, "m2.price", "regression")
#' ranger_explained <- make_ranger(apartments, "m2.price", "regression")
#' 
#' models <- list(lightgbm_explained, ranger_explained)
#' 
#' best_model <- compare_models(models, apartments, "m2.price", "rmse")
#'}
##

compare_models <- function(models, data_test, target, metric = NULL){
  
  ### Starting conditions 
  if (class(models) != "list"){
    stop("Models should be passed as a list")
  }
  
  if (length(models) == 0){
    stop("List of models is empty.")
  }
  
  # Variable for checking models type 
  if (!any(class(models[[1]]) %in% c("forester_model", "explainer"))){
    stop("Wrong class of model. Models should have forester_model or explainer class.")
  }
  models_type <- models[[1]]$model_info$type
  
  # Data frame for final results 
  results <- data.frame()
  
  ### Creating data frame with all models and metrics 
  for (m in models){
    if (!any(class(m) %in% c("forester_model", "explainer"))){
      stop("Wrong class of model. Models should have forester_model or explainer class.")
    }
    
    if (m$model_info$type != models_type){
      stop("All model should have the same type: classification or regression.
         Can not compare models with different types")
    }
    
    ### Transforming target column 
    
    # uploading data test
    update_data(m, data_test[, -which(names(data_test) == target)],
                             data_test[[target]], verbose = FALSE)
    
    # calculating metrics 
    mp <- model_performance(m)
    row <- data.frame(m$label, mp$measures)
    results <- rbind(results, row)
  }
  colnames(results)[1] <- "model"
  
  if (is.null(metric)){
    metric <- ifelse(models_type == "classification", "auc", "rmse")
  } else {
    if (models_type == "classification"){
      if (! metric %in% c("auc", "recall", "precision", "f1", "accuracy")){
        stop("Wrong metric selected. For classification consider one of those metrics: 
           auc, recall, precision, f1, accuracy")
      }
    } else {
      if (! metric %in% c("mse", "rmse", "mad", "r2")){
        stop("Wrong metric selected. For classification consider one of those metrics: 
           rmse, mse, mad, r2")
      }
    }
  }

  # Additional variable for metrics which are supposed to be minimalized 
  ifelse(metric %in% c("mse", "rmse", "mad"), negative <- -1, negative  <- 1)
  
  ### Choosing the best model 
  chosen_metric <- negative * results[[metric]]
  best_model <- models[[which.max(chosen_metric)]]

  #### display table function 
  # Changing order of columns so that chosen metric is first
  if (which(names(results) == metric) != 2){
    non_imprtant_metrics <- names(mp$measures)[!names(mp$measures) %in% metric]
    col_names <- c("model", metric, non_imprtant_metrics)
    results <- results[col_names]
  }
  
  # Sorting results based on chosen metric
  final <- results[order(results[[metric]],
                         decreasing = as.logical(TRUE + negative)),]
  
  # Printing table
  print(knitr::kable(final, row.names = F))
  
  return(best_model)
}


