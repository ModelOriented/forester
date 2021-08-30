#' Function for evaluating and comparing models 
#' 
#' Function \code{evaluate} enables user to compare various model with
#' the same type of task. Function compares them based on metric which can be chosen by the user 
#' and returns the best model 
#' 
#' @param ... models to compare. Models shoud be either \code{forester_model} or \code{explainer} object.
#' @param data_test data frame for evaluation models
#' @param target character indicating the target column in data test table
#' @param metric string containing name of mertic based on which the model will be selected. 
#' Metric shoud be written in small letters. Can be omited.
#'
#'
#' @return A list containing an object of the class \code{forester_model} and data frame with results.
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
#' best_model <- evaluate(lightgbm_explained, ranger_explained, 
#'                              data_test = apartments, 
#'                              target = "m2.price",
#'                              metric = "rmse")
#'}
##

evaluate <- function(..., data_test, target, metric = NULL){

  models <- list(...)
  
  ### Starting conditions 
  if (length(models) == 0){
    stop("List of models is empty.")
  }
  
  # Variable for checking models type 
  if (!any(class(models[[1]]) %in% c("forester_model", "explainer"))){
    stop("Wrong class of model. Models should have forester_model or explainer class.")
  }
  models_type <- models[[1]]$model_info$type
  
  # Checking test set
  data_test <- check_conditions(data = data_test,
                                target = target,
                                type = models_type)
  
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
    steps <- m$modifications$steps[[1]]

    if (any(class(steps) == "step_naomit")){
      data_test <- na.omit(data_test)
    }
    
    # uploading data test
    update_data(m, data_test[, -which(names(data_test) == target)],
                             data_test[[target]], verbose = FALSE)
    
    # calculating metrics 
    mp <- model_performance(m)
    row <- data.frame(m$label, mp$measures)
    results <- rbind(results, row)
  }
  colnames(results)[1] <- "model"
  
  metric <- check_metric(metric, models_type)

  class(results) <- c("forester_results", "data.frame")
  
  #### Printing part 
  
  # Additional variable for metrics which are supposed to be minimalized 
  ifelse(metric %in% c("mse", "rmse", "mad"), negative <- -1, negative  <- 1)
  
  ### Choosing the best model 
  chosen_metric <- negative * results[[metric]]
  best_model <- models[[which.max(chosen_metric)]]
  

  print(results, metric)

  message("The best model based on ", metric, " metric is ", best_model$label, ".")
  
  return(list(best_model = best_model, results = results))
}


print.forester_results <- function(results, metric = NULL){
  
  if (is.null(metric)){
    message("Results of compared models:")
    print(knitr::kable(results, row.names = FALSE, "simple"))
  } else {
    # Additional variable for metrics which are supposed to be minimalized 
    ifelse(metric %in% c("mse", "rmse", "mad"), negative <- -1, negative  <- 1)

    #### display table function 
    # Changing order of columns so that chosen metric is first
    # Checking if column with chosen metric is on second place
    if (which(names(results) == metric) != 2){
      non_imprtant_metrics <- colnames(results)[!names(results) %in% c(metric, "model")]
      col_names <- c("model", metric, non_imprtant_metrics)
      results <- results[col_names]
    }
    
    # Sorting results based on chosen metric
    final <- results[order(results[[metric]],
                           decreasing = as.logical(TRUE + negative)),]
    
    # Printing table
    message("Results of compared models:")
    print(knitr::kable(final, row.names = FALSE, "simple"))
  }
}

