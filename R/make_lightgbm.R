#' Automated Function for Explaining LightGBM model
#'
#' Function \code{make_lightgbm} automates the process of applying LightGBM model
#' for a dataset and simutaneously creates explainer from the use of DALEX package.
#' The created explainer can be further processed by functions for explanations.
#'
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - data will be used to run the LightGBM model. NOTE: data has to contain the target column.
#' @param target character: name of the target column, placed in quotation marks. The input target name is compulsory to be one of column names of input data.
#' @param type character: defining the task, placed in quotation marks. Two options for type: "regression" and "classification", particularly, binary classification.
#'
#' @return An object of the class \code{explainer} for LightGBM model with given data, target and defined type of problem.
#'
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @export
#' @importFrom stats predict
#' @importFrom utils head tail installed.packages methods
#'
#' @examples
#' # simple explainer for regression problem
#'
#' library(datasets)
#' library(DALEX)
#' data("iris")
#' EXPLAINER <- make_lightgbm(iris,"Petal.Length",type="regression")
#'
#' ## Variable importance
#' variable_importance_gbm <- model_parts(EXPLAINER,type="raw")
#' plot(variable_importance_gbm)
#'
#' ## Single variable:
#' sv_gbm_petal_width <- model_profile(EXPLAINER, variable="Petal.Width",type="partial")
#' plot(sv_gbm_petal_width)
#'
#'
#' # simple explainer for classification problem
#'
#' library(DALEX)
#' ## We will predict the survived state: 0 and 1
#' titanic_explainer <- make_lightgbm(titanic_imputed, "survived", "classification")
#'
#' ## Assessment for model performance:
#' model_performance(titanic_explainer)
#'
#' ## Finding important features:
#' plot(model_parts(titanic_explainer))
#'
#' ## Plot reverse cumulative distribution of module of residual
#' plot(model_performance(titanic_explainer))
#'
##

<<<<<<< Updated upstream
make_lightgbm <- function(data, target, type = "regression")
{
  ### Conditions:
  # Check data class
  if (!any(class(data) %in% c("data.frame", "dgCMatrix", "matrix", "data.table")))
    stop("Object is not one of the types: 'data.frame', 'dgCMatrix', 'matrix', 'data.table")

  # Unify data class to data frame
  if (any(class(data) == "matrix"))
  {
    data <- as.data.frame(data)
  }
  if (any(class(data) == "data.table"))
  {
    data <- as.data.frame(data)
  }
  if (any(class(data) == "dgCMatrix"))
  {
    data <- as.data.frame(as.matrix(data))
  }
  
  ### Data processing level 1/2 (remove NAs, split label and training frame,...)
  # Remove rows with NA values (I will write in the documentation of function):
  data <- na.omit(data)

  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }

  # Check if target is one of colnames of data frame
  if (!is.character(target) | !(target %in% colnames(data)))
    stop(
      "Either 'target' input is not a character or data does not have column named similar to 'target'"
    )

  # Change character columns to factors
  data[sapply(data, is.character)] <-
    lapply(data[sapply(data, is.character)], as.factor)

  # Extract the target vector from our dataframe:
  label_column <- data[[target]]

  # Check condition for type between "classification" and "regression":
  ## Classification type
  if ((type != "classification") & (type != "regression"))
    stop("Type of problem is invalid.")



  ### Conditions and processing on target column:
  # Binary Classification
  if (type == "classification")
  {
    if (length(unique(label_column)) < 2)
    {
      stop("Too few classes for binary classification")
    }
    if (length(unique(label_column)) > 2)
    {
      stop("Too many classes for binary classification")
    }

    # Coercing the target from factor to numeric values:
    if (class(label_column) == "factor")
    {
      label_column <- as.numeric(data[[target]] == data[1, target])
      message("Program coerces factors into numeric values:")
      var_true <- as.character(data[1, target])
      var_all  <- as.character(unique(data[, target]))
      message(paste("MECHANISM: ", var_true, " -> 1 and ", var_all[!var_all  %in% var_true], " -> 0."))
    }

    # Rescaling target columns to be in [0,1]
    uniq_vals <- unique(label_column)
    if ((min(uniq_vals) != 0) | (max(uniq_vals) != 1))
    {
      # Change max -> 1; min -> 0:
      max <- max(uniq_vals)
      min <- min(uniq_vals)
      message("Two values in target column are not in [0,1]")
      message(paste("MECHANISM: ", min , " -> 0 and", max, "-> 1."))
      label_column[label_column == max] <- 1
      label_column[label_column == min] <- 0
    }
  }


  # Regression:
  if (type == "regression")
  {
    if (class(label_column) == "factor")
    {
      stop(
        "Program is stopped. The class of target column is factor, not appropriate for regression problem"
      )
    }
    # Double-check for numeric type of label column:
    label_column <- as.numeric(label_column)
  }


=======
make_lightgbm <- function(data, target, type, num_features = NULL, fill_na = TRUE, tune = FALSE, metric = NULL, iter=20){
>>>>>>> Stashed changes

  # Preparing data 
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)
  
  data <- prepared_data$data
  modifications <- prepared_data$modifications
  
  ### Data processing level 2/2
  # Data frame with transformed label_column:
  data_info <- data[, !(colnames(data) %in% target), drop = FALSE]
  
  # Using lgb.convert_with_rules. The function transforms the data into a fittable data
  data_info_rules <- lightgbm::lgb.convert_with_rules(data = data_info)
  data_encoded <- data_info_rules$data
  
  # Transform from dataframe to matrix:
  data_encoded_matrix <- as.matrix(data_encoded)
<<<<<<< Updated upstream

  # Convert to object used in LightGBM model:
  names_cat_vars <- names(data_info_rules$rules)
  dtrain <-
    lightgbm::lgb.Dataset(
      data = data_encoded_matrix,
      label = label_column,
      categorical_feature = as.vector(which(
        colnames(data_encoded) %in% names_cat_vars
      ))
    )



  ### Creating predict function:
  lightgbm_predict <- function(object, newdata) {
    newdata_encoded <-
      lightgbm::lgb.convert_with_rules(data = newdata, rules = data_info_rules$rules)$data
    data_encoded_matrix_newdata <- as.matrix(newdata_encoded)
    return (predict(object, data_encoded_matrix_newdata))
  }


  ### XGBoost Model
  # Regression
  if (type == "regression") {
    model <- lightgbm::lightgbm(
      data = dtrain,
      verbose = -1,
      learning_rate = 0.7,
      nrounds = 10,
      objective = "regression"
    )
  }

  # Binary classification
  if (type == "classification") {
=======
  
  if (!tune){
    # Convert to object used in LightGBM model:
    names_cat_vars <- names(data_info_rules$rules)
    dtrain <-
      lightgbm::lgb.Dataset(
        data  = data_encoded_matrix,
        label = data[, target],
        categorical_feature = as.vector(which(colnames(data_encoded) %in% names_cat_vars))
      )
    
  ### Creating Model:
>>>>>>> Stashed changes
    model <- lightgbm::lightgbm(
      data = dtrain,
      verbose = -1,
      learning_rate = 0.4,
      nrounds = 10,
      objective = ifelse(type == "regression", "regression", "binary"))
  } else {
    # Checking the metric 
    metric <- check_metric(metric, type)
    
    data_tune <- data
    
  # Spliting data_train and data_validation:
    df_splitted <- split_data(data_tune, target, type)
    data_train <- df_splitted[[1]]
    data_val <- df_splitted[[2]]
    
  # Saving data for explainer
    data <- data_train
    
  # Extract y_val. Data_val without target column:
    y_val    <- data_val[[target]]
    data_val <- data_val[ ,!(colnames(data_val) %in% target), drop = FALSE]
    
  # Update label_column from data_train. Data_train without target column:
    label_column <- data_train[[target]]
    data_train <- data_train[, !(colnames(data_train) %in% target), drop = FALSE]
    
  # Using lgb.convert_with_rules. The function transforms the data into a fittable data
    data_train_tuned_rules <- lightgbm::lgb.convert_with_rules(data = data_train)
    data_encoded_tuned <- data_train_tuned_rules$data
    
  # Transform from dataframe to matrix:
    data_encoded_tuned_matrix <- as.matrix(data_encoded_tuned)
    
  # Convert to data_train object used in LightGBM:   
    names_cat_tuned_vars <- names(data_train_tuned_rules$rules)
    
  # Encode data_val:
    data_val <- lightgbm::lgb.convert_with_rules(data = data_val, rules = data_train_tuned_rules$rules)$data
    data_val <- as.matrix(data_val)
    
  ### Tuning part:
    # Argument for metrics which should be minimized
    desc <- ifelse(metric %in% c("mse", "rmse", "mad"), -1, 1)
    
    lightgbm_tune_fun <- function(learning_rate, num_leaves, bagging_fraction,
                                  max_depth, max_bin, min_data_in_leaf,
                                  min_sum_hessian_in_leaf,
                                  subsample,nrounds){
      # Creating LightGBM object 
      dtrain <- lightgbm::lgb.Dataset(
          data = data_encoded_tuned_matrix,
          label = label_column,
          categorical_feature = as.vector(which(
            colnames(data_encoded_tuned) %in% names_cat_tuned_vars))
        )
      
     lightgbm_tune <- lightgbm::lightgbm(data = dtrain,
                                         learning_rate = learning_rate,
                                         num_leaves = num_leaves,
                                         bagging_fraction = bagging_fraction,
                                         max_depth = max_depth,
                                         max_bin = max_bin,
                                         min_data_in_leaf = min_data_in_leaf,
                                         min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                                         subsample = subsample,
                                         nrounds = nrounds,
                                         verbose = -1,
                                         objective = ifelse(type == "regression", "regression", "binary")
      )
      if (type == "regression"){
        predicted <- predict(lightgbm_tune, data_val)
      }
      # If user wants to return probability of classes: remove ifelse statement.
      if (type == "classification"){
        predicted <- ifelse(predict(lightgbm_tune, data_val) >= 0.5, 1, 0)
      }
     
      score <- desc * calculate_metric(metric, predicted, y_val)
      list(Score = score, Pred = predicted)
    }
    
    
    # Tuning process:
    tuned_lightgbm <- rBayesianOptimization::BayesianOptimization(lightgbm_tune_fun,
                                          bounds = list(nrounds = c(1L, 5000L),
                                                        learning_rate = c(0.01, 1.0),
                                                        num_leaves = c(24L,80L),
                                                        bagging_fraction = c(0.8, 1),
                                                        max_depth = c(1L,15L),
                                                        max_bin = c(20L,90L),
                                                        min_data_in_leaf = c(7L,63L),
                                                        min_sum_hessian_in_leaf = c(0,100),
                                                        subsample = c(0.1,1.0)),
                                          init_grid_dt = NULL,
                                          init_points = 10,
                                          n_iter = iter,
                                          acq = "ucb",
                                          kappa = 2.576,
                                          eps = 0.0,
                                          verbose = TRUE)
    
    best_params <- tuned_lightgbm$Best_Par
    
    # Creating last LihgtGBM dataset
    dtrain <- lightgbm::lgb.Dataset(
      data = data_encoded_tuned_matrix,
      label = label_column,
      categorical_feature = as.vector(which(
        colnames(data_encoded_tuned) %in% names_cat_tuned_vars))
    )
    
    model <- lightgbm::lightgbm(data = dtrain,
                                nrounds = best_params["nrounds"],
                                learning_rate = best_params["learning_rate"],
                                num_leaves = best_params["num_leaves"],
                                bagging_fraction = best_params["bagging_fraction"],
                                max_depth = best_params["max_depth"],
                                max_bin = best_params["max_bin"],
                                min_data_in_leaf = best_params["min_data_in_leaf"],
                                min_sum_hessian_in_leaf = best_params["min_sum_hessian_in_leaf"],
                                subsample = best_params["subsample"],
                                objective = ifelse(type == "regression", "regression", "binary"),
                                verbose = -1
                                )
  }
  
  ### Creating predict function:
  lightgbm_predict <- function(object, newdata, rec = modifications) {
    # Changing data type to data frame 
    newdata <- check_conditions(newdata)
    
    newdata <- as.data.frame(recipes::bake(rec, newdata))
    
    newdata_encoded <- lightgbm::lgb.convert_with_rules(data = newdata,
                                                        rules = data_info_rules$rules)$data
    data_encoded_matrix_newdata <- as.matrix(newdata_encoded)
    
    if (type == "regression"){
      return(predict(object, data_encoded_matrix_newdata))
    } else {
      return(ifelse(predict(object, data_encoded_matrix_newdata) >= 0.5, 1, 0))
    }
  }
  
  ### Explainer from DALEX
  # For simplicity, take processed matrix from original data frame for explanation purpose:
  explainer_automate_lightgbm <- DALEX::explain(
    model,
    data = data[, which(colnames(data) != target), drop = FALSE],
    y = data[, target],
    predict_function = lightgbm_predict,
    label = "LightGBM",
    type = type,
    verbose = 0
  )
  
  ### S3 objects 
  explainer_automate_lightgbm$modifications <- modifications
  class(explainer_automate_lightgbm) <- c("forester_model", "explainer")
  return(explainer_automate_lightgbm)
}


