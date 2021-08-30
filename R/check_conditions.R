#' Checking conditions for dataset
#'
#' \code{check_conditions} verifies whether the input data with the defined target and defined type of problem
#' is ready for further steps in creating Machine Learning models.
#'
#' Function transforms different data structures into dataframe, checks the presence of defined target column in data and the validity
#' of type.
#'
#'
#' @param data dataset, used for training models. Class of data_train is one of those classes: data.frame, matrix, data.table or dgCMatrix. NOTE: data_train includes target column.
#' @param target character, indicating name of the target column in data_train.
#' @param type character, defining the task. Option is "regression" or "classification", namely binary classification.
#'
#' @return A verified dataframe, which is ready for creating ML models.
#'
#'
#' @references forester library \url{https://modeloriented.github.io/forester/}

check_conditions <- function(data, target = NULL, type = NULL){
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
  
  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }
  
  # Check if target is one of colnames of data frame
  if (!is.null(target)){
    if (!is.character(target) | !(target %in% colnames(data)))
      stop(
        "Either 'target' input is not a character or data does not have column named similar to 'target'"
      )
  }
  
  # Check condition for type between "classification" and "regression"
  if (!is.null(type)){
    if ((type != "classification") & (type != "regression"))
      stop("Type of problem is invalid.")
  }
  
  if (!is.null(target) & !is.null(type)){
    ### Conditions and processing on target column:
    # Binary Classification
    if (type == "classification")
    {
      # Checking number of classes in target column:
      if (length(unique(data[, target])) < 2)
      {
        stop("Too few classes for binary classification")
      }
      if (length(unique(data[, target])) > 2)
      {
        stop("Too many classes for binary classification")
      }
      
      # Coercing the target from factor to numeric values:
      if (class(data[, target]) == "factor")
      {
        labels_sorted <- sort(unique(data[, target]))
        var_true <- labels_sorted[1]
        var_false  <- labels_sorted[2]
        data[, target] <- as.numeric(data[, target] == var_true)
        message("Program coerces factors into numeric values:")
        message(paste("MECHANISM: ", var_true, " -> 1 and ", var_false, " -> 0."))
      }
      
      # Rescaling target columns to be in [0,1]
      uniq_vals <- unique(data[, target])
      if ((min(uniq_vals) != 0) | (max(uniq_vals) != 1))
      {
        # Change max -> 1; min -> 0:
        max <- max(uniq_vals)
        min <- min(uniq_vals)
        message("Two values in target column are not in [0,1]")
        message(paste("MECHANISM: ", min , " -> 0 and", max, "-> 1."))
        data[, target] <- ifelse(data[, target] == max, 1, 0)
      }
    }
    
    # Regression:
    if (type == "regression")
    {
      if (class(data[, target]) == "factor")
      {
        stop(
          "Program is stopped. The class of target column is factor, not appropriate for regression problem"
        )
      }
      # Double-check for numeric type of label column:
      data[, target] <- as.numeric(data[, target])
    }
  }
  
  

  return(data)
}
