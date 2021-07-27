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
#' exp <- make_ranger(apartments, "m2.price", "regr")
#' # plot(model_parts(exp))
#'
#' # binary classification
#' library(DALEX)
#' data(titanic_imputed, package="DALEX")
#' exp <- make_ranger(titanic_imputed, "survived", "classif")
#' # plot(model_parts(exp))
#'}
##

make_ranger <- function(data, target, type) {
  ### Conditions
  # Checking data class
  if (!(any(class(data) != 'data.frame') | any(class(data) != 'matrix')
        | any(class(data) != 'dgCMatrix') | any(class(data) != 'data.table'))){
      stop("Wrong data type")
  }

  # Changing data to normal data frame
  if (any(class(data) != "data.frame")){
    if (any(class(data) == 'dgCMatrix')){data <- as.data.frame(as.matrix(data))}
    else if (any(class(data) == 'matrix')){data <- as.data.frame(data)}
    else if (any(class(data) == 'data.table')){data <- as.data.frame(data)}
  }

  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }

  # Checking if target column is in data frame
  if (typeof(target) != 'character' | !(target %in% colnames(data))) {
    stop("Incorrect target value")
  }

  # Checking if type is correct
  if (typeof(type) != 'character' | (type != "classif" & type != "regr")){
    stop("Incorrect type name. Choose beetwen 'classif' and 'regr'. ")
  }

  # Creating formula
  # Checking if target value has nans
  if (any(is.na(data[, target]))){
    stop("There can't be NaN values in target column!")
  }
  # try to convert column names to names without znaki
  test_colnames<- lapply(colnames(data),function(x) gsub("_", "", x))
  test_colnames<- lapply(colnames(data),function(x) gsub("[.]", "", x))
  if(any(grepl('[[:punct:]]', test_colnames))) {
    colnames(data) <- lapply(colnames(data),function(x) gsub("[[:punct:]]", "_", x))
    target <- gsub("[[:punct:]]", "_", target)
    message("Column names are wrong for creating a formula. Replacing special signs with '_'")
  }
  form <- stats::as.formula(paste(target , "~."))


  ### Model
  # First binary classification
  if (type == "classif") {
    # Checking if theres right number of classes in target column
    if (length(unique(data[, target])) < 2) {
      stop("To little classes for binary classification")
    } else if (length(unique(data[, target])) > 2){
      stop("To many classes for binary classification")
    }

    if (class(data[[target]]) != "numeric") {
      # Converting target column to numeric for Dalex
      uniq <- unique(data[, target])
      data[, target] <- ifelse(data[, target] == uniq[1], 0, 1)
      message(paste("Wrong type of target column. Changed to numeric: ",
                    uniq[1], " -> 1 and ", uniq[2], " -> 0 ", sep=""))
    }

    # Creating model
    rg <- ranger::ranger(form, data = data, classification = TRUE)
  } else {
    # Checking if target column is numeric
    if (class(data[[target]]) != 'numeric' & class(data[[target]]) != 'integer') {
      stop("Target value should be numeric for regression task")
    }

    #Creating a model
    rg <- ranger::ranger(form, data = data)
  }

  ### Explainer
  # Deleting target column from data frame
  df_exp <- data[, -which(names(data) == target)]

  # Creating an explainer
  ranger_explained <- DALEX::explain(rg,
                              data = df_exp,
                              y = data[, target],
                              label = "Ranger")

  return(ranger_explained)
}










