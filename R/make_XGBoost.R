#' Automated Function for Explaining XGBoost model
#'
#' Function \code{make_xgboost} automates the process of applying XGBoost model
#' for a dataset and simutaneously creates explainer from the use of DALEX package.
#' The created explainer can be further processed by functions for explanations.
#'
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - data will be used to run the XGBoost model. NOTE: data has to contain the target column.
#' @param target character: name of the target column, placed in quotation marks. The input target name is compulsory to be one of column names of input data.
#' @param type character: defining the task, placed in quotation marks. Two options for type: "regression" and "classification", particularly, binary classification.
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

make_xgboost <- function(data, target, type = "regression")
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
  if ((type != "classification") & (type != "regression"))
    stop("Type of problem is invalid.")
  
  
  
  ### Conditions and processing on target column:
  # Binary Classification
  if (type == "classification")
  {
    # Checking number of classes in target column:
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
  
  
  
  ### Data processing level 2/2
  # Data frame without target
  data_info <- data[,!(colnames(data) %in% target), drop = FALSE]
  
  # One-hot encoding for categorical features in Data frame without target
  names_factor <-
    colnames(data_info)[sapply(data_info, is.factor)] # finding names of categorical attributes
  vector_index <-
    which(colnames(data_info) %in% names_factor)     # positions of those categorical attributes
  # in colnames of Data frame without target
  data_encoded <-
    model.matrix(~ . - 1, data = data_info,
      contrasts.arg = lapply(data_info[, vector_index, drop = FALSE],
                             contrasts, contrasts = FALSE)
    )
  # using model.matrix to create one-hot encoded matrix
  
  # Transform from data frame type to matrix to prepare for XGBboost model:
  data_encoded_matrix <- data.matrix(data_encoded)
  
  # Convert to object used in XGBoost model:
  dtrain <-
    xgboost::xgb.DMatrix(data = data_encoded_matrix, label = label_column)
  
  ### Creating predict function:
  xgboost_predict <- function(object, newdata) {
    names_factor_newdata <-
      colnames(newdata)[sapply(newdata, is.factor)]
    vector_index_newdata <-
      which(colnames(newdata) %in% names_factor_newdata)
    data_encoded_newdata <-
      model.matrix(~ . - 1, data = newdata,
        contrasts.arg = lapply(newdata[, vector_index_newdata, drop = FALSE],
                               contrasts, contrasts = FALSE)
      )
    data_encoded_matrix_newdata <- data.matrix(data_encoded_newdata)
    return (predict(object, data_encoded_matrix_newdata))
  }
  
  
  
  ### XGBoost Model
  # Regression
  if (type == "regression") {
    model <- xgboost::xgb.train(
      data = dtrain,
      verbose = 0,
      max.depth = 8,
      eta = 0.3,
      nrounds = 4,
      nthread = 2,
      objective = "reg:linear"
    )
  }
  
  # Binary classification
  if (type == "classification") {
    model <- xgboost::xgb.train(
      data = dtrain,
      verbose = 0,
      max.depth = 8,
      eta = 0.3,
      nrounds = 4,
      nthread = 2,
      eval_metric = "logloss",
      objective = "binary:logistic"
    )
  }
  
  
  
  ### Explainer from DALEX
  # For simplicity, take processed matrix from original data frame for explanation purpose:
  explainer_automate_xgb <- DALEX::explain(
    model,
    data = data[, -which(names(data) == target), drop = FALSE],
    y = label_column,
    predict_function = xgboost_predict,
    label = "XGBoost",
    type = type,
    verbose = 0
  )
  
  ### S3 objects 
  class(explainer_automate_xgb) <- c("forester_model", "explainer")
  return(explainer_automate_xgb)
}
