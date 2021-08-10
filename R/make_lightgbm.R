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
      label_column[label_column == min]  <- 0
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
  data_info <- data[, !(colnames(data) %in% target), drop = FALSE]

  # Using lgb.convert_with_rules. The function transforms the data into a fittable data
  data_info_rules <- lightgbm::lgb.convert_with_rules(data = data_info)
  data_encoded <- data_info_rules$data

  # Transform from dataframe to matrix:
  data_encoded_matrix <- as.matrix(data_encoded)

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
      learning_rate=0.7,
      nrounds = 10,
      objective = "regression"
    )
  }

  # Binary classification
  if (type == "classification") {
    model <- lightgbm::lightgbm(
      data = dtrain,
      verbose = -1,
      learning_rate = 0.4,
      nrounds = 10,
      objective = "binary"
    )
  }



  ### Explainer from DALEX
  # For simplicity, take processed matrix from original data frame for explanation purpose:
  explainer_automate_lightgbm <- DALEX::explain(
    model,
    data = data[,-which(names(data) == target),
                drop = FALSE],
    y = label_column,
    predict_function = lightgbm_predict,
    label = "LightGBM"
  )
  return(explainer_automate_lightgbm)
}
