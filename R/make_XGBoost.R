#' Automated Function for Explaining XGBoost model
#'
#' Function \code{make_xgboost} automates the process of applying XGBoost model
#' for a data set and simultaneously creates an explainer from the use of DALEX package.
#' The created explainer can be further processed by functions for explanations.
#'
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
#' 
#' @return An object of the class \code{explainer} for XGBoost model from given data, target and defined type of problem.
#'
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @export
#' @importFrom stats predict
#' @examples
#' # simple explainer for regression problem
#'
#' library(datasets)
#' library(DALEX)
#'
#' data("iris")
#' EXPLAINER <- make_xgboost(iris,"Petal.Length",type="regression")
#'
#' ## Variable importance
#' variable_importance_xgb <- model_parts(EXPLAINER,type="raw")
#' plot(variable_importance_xgb)
#'
#' ## Single variable:
#' sv_xgb_petal_width <- model_profile(EXPLAINER, variable="Petal.Width",type="partial")
#' plot(sv_xgb_petal_width)
#'
#' # simple explainer for classification problem
#'
#' library(DALEX)
#' ## We will predict the survived state: 0 and 1
#' titanic_explainer <- make_xgboost(titanic_imputed, "survived", "classification")
#'
#' ## Assessment for model performance:
#' model_performance(titanic_explainer)
#'
#' ## Finding important features:
#' plot(model_parts(titanic_explainer))
#'
#' ## Plot reverse cumulative distribution of module of residual
#' plot(model_performance(titanic_explainer))
##

make_xgboost <- function(data, target, type, fill_na = FALSE, num_features = NULL, tune = FALSE, tune_metric = NULL, tune_iter = 20){
  
  ### Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications
  
  # Change character columns to factors
  data[sapply(data, is.character)] <-
    lapply(data[sapply(data, is.character)], as.factor)
  
  ### Encoding
  if (any(sapply(data, is.factor))){
    # Performing encoding
    # Creating formula
    # try to convert column names to names without znaki
    test_colnames <- lapply(colnames(data), function(x) gsub("_", "", x))
    test_colnames <- lapply(test_colnames , function(x) gsub("[.]", "", x))
    if (any(grepl('[[:punct:]]', test_colnames))) {
      stop("Column names are wrong for creating a formula. Column names can not contain special characters as '+', '-', '=', '*'. Try substitute special characters with '_' sign.")
    }
    form <- stats::as.formula(paste(target , "~."))
    
    rec_encoding <- recipes::recipe(form, data)
    encode_step <- recipes::step_dummy(rec_encoding, recipes::all_predictors())
    suppressWarnings(encode_step_trained <- recipes::prep(encode_step, training = data))
    
    data_encoded <- as.data.frame(recipes::bake(encode_step_trained, data))
    
    encoding_recipe <- encode_step_trained
  } else {
    data_encoded <- data
    encoding_recipe <- NULL
  }

  
  # Creating trial models and tuning part:
  
  if (!tune){

    # Extract label_column from new data_info (data_info is splited)
    label_column <- data_encoded[, target]
    
    # Transform from data frame type to matrix to prepare for XGBoost model:
    data_encoded_matrix <- data.matrix(data_encoded[,!(colnames(data_encoded) %in% target), drop = FALSE])

    # Convert to object used in XGBoost model:
    dtrain <-
      xgboost::xgb.DMatrix(data = data_encoded_matrix, label = label_column)
    
    
    model <- xgboost::xgb.train(data = dtrain,
                                verbose = 0,
                                nrounds = 20,
                                objective = ifelse(type == "regression","reg:squarederror","binary:logistic"),
                                eval_metric = ifelse(type == "regression","rmse","logloss")
    )
  } else {
    # Checking the metric 
    tune_metric <- check_metric(tune_metric, type)
    
    # Spliting data_train and data_validation:
    data_splitted <- split_data(data_encoded, target, type)
    data_train <- data_splitted[[1]]
    data_val <- data_splitted[[2]]
    
    # Extract y_val and data_val without target column:
    y_val    <- data_val[[target]]
    data_val <- data_val[ ,!(colnames(data_val) %in% target), drop = FALSE]
    data_val <- data.matrix(data_val)
    
    # Extract label_column from new data_info (data_info is splited)
    label_column <- data_train[[target]]
    data_train <- data_train[,!(colnames(data_train) %in% target), drop = FALSE]
    
    # Transform from data frame type to matrix to prepare for XGBoost model:
    data_encoded_matrix <- data.matrix(data_train)
    
    # Convert to object used in XGBoost model:
    dtrain <-
      xgboost::xgb.DMatrix(data = data_encoded_matrix, label = label_column)
    
    
    
    # Argument for metrics which should be minimized
    desc <- ifelse(tune_metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    xgboost_tune_fun <- function(nrounds, eta, subsample, max_depth, min_child_weight, colsample_bytree,
                                 colsample_bylevel, lambda, alpha){
      xgboost_tune <- xgboost::xgb.train(data = dtrain,
                                         nrounds = nrounds,
                                         eta = 2^eta,
                                         subsample = subsample,
                                         max_depth = max_depth,
                                         min_child_weight = 2^min_child_weight,
                                         colsample_bytree = colsample_bytree,
                                         colsample_bylevel = colsample_bylevel,
                                         lambda = 2^lambda,
                                         alpha = 2^alpha,
                                         objective = ifelse(type == "regression","reg:squarederror","binary:logistic"),
                                         eval_metric = ifelse(type == "regression","rmse","logloss")
      )
      if (type == "regression"){
        predicted <- predict(xgboost_tune, data_val)
      }
      # If user wants to return probability of classes: remove ifelse statement.
      if (type == "classification"){
        predicted <- ifelse(predict(xgboost_tune, data_val)>=0.5,1,0)
      }
      score <- desc * calculate_metric(tune_metric, predicted, y_val)
      
      list(Score = score, Pred = predicted)
    }
    
    
    # Tuning process:
    tuned_xgboost <- rBayesianOptimization::BayesianOptimization(xgboost_tune_fun,
                                                                 bounds = list(nrounds = c(1L, 5000L),
                                                                               eta = c(-10, 0),
                                                                               subsample = c(0.1, 1),
                                                                               max_depth = c(1L, 15L),
                                                                               min_child_weight = c(0,7),
                                                                               colsample_bytree = c(0,1),
                                                                               colsample_bylevel = c(0,1),
                                                                               lambda = c(-10,10),
                                                                               alpha = c(-10,10)),
                                                                 init_grid_dt = NULL,
                                                                 init_points = 5,
                                                                 n_iter = tune_iter,
                                                                 acq = "ucb",
                                                                 kappa = 2.576,
                                                                 eps = 0.0,
                                                                 verbose = TRUE)
    
    best_params <- tuned_xgboost$Best_Par
    
    model <- xgboost::xgb.train(data = dtrain,
                                nrounds = best_params["nrounds"],
                                eta = 2^best_params["eta"],
                                subsample = best_params["subsample"],
                                max_depth = best_params["max_depth"],
                                min_child_weight = 2^best_params["min_child_weight"],
                                colsample_bytree = best_params["colsample_bytree"],
                                colsample_bylevel = best_params["colsample_bylevel"],
                                lambda = 2^best_params["lambda"],
                                alpha = 2^best_params["alpha"],
                                objective = ifelse(type == "regression","reg:squarederror","binary:logistic"),
                                eval_metric = ifelse(type == "regression","rmse","logloss"))
  }
  
  
  ### Creating predict function:
  model$modifications <- modifications
  model$encoding <- encoding_recipe
  
  xgboost_predict <- function(object, newdata) {
    # Changing data type to data frame 
    newdata <- check_conditions(newdata)
    
    # Applying modifications
    rec <- object$modifications
    rec_enc <- object$encoding
    
    newdata <- as.data.frame(recipes::bake(rec, newdata))
    if (!is.null(rec_enc)){
      newdata <- as.data.frame(recipes::bake(rec_enc, newdata))
    }
  
    data_encoded_matrix_newdata <- data.matrix(newdata)
    
    if (type == "regression"){
      return(predict(object, data_encoded_matrix_newdata))
    }
    # If user wants to return probability of classes: remove ifelse statement.
    if (type == "classification"){
      return(ifelse(predict(object, data_encoded_matrix_newdata) >= 0.5,1,0))
    }
  }
  
  ### Explainer from DALEX
  # For simplicity, take processed matrix from original data frame for explanation purpose:
  explainer_automate_xgb <- DALEX::explain(
    model,
    data = data[, !(colnames(data) %in% target), drop = FALSE],
    y = data[, target],
    predict_function = xgboost_predict,
    label = "XGBoost",
    type = type,
    verbose = 0
  )
  
  ### S3 objects 
  explainer_automate_xgb$modifications <- modifications
  class(explainer_automate_xgb) <- c("forester_model", "explainer")
  return(explainer_automate_xgb)
}


