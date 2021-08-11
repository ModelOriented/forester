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

make_ranger <- function(data, target, type) {
  ### Conditions
  # Checking data class
  if (!any(class(data) %in% c("data.frame", "dgCMatrix", "matrix", "data.table"))) {
    stop("Object is not one of the types: 'data.frame', 'dgCMatrix', 'matrix', 'data.table")
  }

  # Changing data to normal data frame
  if (any(class(data) != "data.frame")) {
    if (any(class(data) == 'dgCMatrix')) {
        data <- as.data.frame(as.matrix(data))
    } else if (any(class(data) == 'matrix')) {
        data <- as.data.frame(data)
    } else if (any(class(data) == 'data.table')) {
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
  test_colnames <- lapply(colnames(data), function(x) gsub("[.]", "", x))
  if (any(grepl('[[:punct:]]', test_colnames))) {
    colnames(data) <- lapply(colnames(data), function(x) gsub("[[:punct:]]", "_", x))
    target <- gsub("[[:punct:]]", "_", target)
    message("Column names are wrong for creating a formula. Replacing special signs with '_'")
  }
  form <- stats::as.formula(paste(target , "~."))
  
  
  ### Model
  # First binary classification
  if (type == "classification") {
    # Checking if theres right number of classes in target column
    if (length(unique(data[, target])) < 2) {
      stop("Too few classes for binary classification")
    } else if (length(unique(data[, target])) > 2) {
      stop("Too many classes for binary classification")
    }
    
    if (class(data[[target]]) != "numeric") {
      # Converting target column to numeric for Dalex
      uniq <- unique(data[, target])
      data[, target] <- ifelse(data[, target] == uniq[1], 0, 1)
      message(paste("Wrong type of target column. Changed to numeric: ",
          uniq[1], " -> 1 and ", uniq[2], " -> 0 ", sep = "")
      )
    }
    
    # Creating model
    rg <- ranger::ranger(form, data = data, classification = TRUE)
  } else {
    # Checking if target column is numeric
    if (class(data[[target]]) != 'numeric' &
        class(data[[target]]) != 'integer') {
      stop("Program is stopped. The class of target column is factor, not appropriate for regression problem")
    }
    
    #Creating a model
    rg <- ranger::ranger(form, data = data)
  }
  
  ### Explainer
  # Deleting target column from data frame
  df_exp <- data[,-which(names(data) == target)]
  
  # Creating an explainer
  ranger_explained <- DALEX::explain(rg,
                                     data = df_exp,
                                     y = data[, target],
                                     label = "Ranger",
                                     verbose = 0)
  
  class(ranger_explained) <- c("forester_model", "explainer")
  return(ranger_explained)
}
