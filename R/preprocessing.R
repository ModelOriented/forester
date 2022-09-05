#' Conducts preprocessing processes
#'
#' @param data A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A preprocessed dataset.
#' @export
#'
#' @examples
#' data(compas)
#' prep_data <- preprocessing(compas,'Two_yr_Recidivism')
preprocessing <- function(data, y) {
  # sorry for commenting out the code, but i want to discuss the need of writting
  # a verbose_cat function
  # cat(' -------------------- PREPROCESSING REPORT --------------------- \n \n')
  pre_data <- pre_rm_static_cols(data, y)
  pre_data <- binarize_target(pre_data, y)
  pre_data <- manage_missing(pre_data, y)
  del_cols <- save_deleted_columns(data, pre_data)

  # cat(' ------------------ PREPROCESSING REPORT END ------------------- \n \n')
  return(
    list(
      data     = pre_data,
      colnames = del_cols
    )
  )
}

#' Removes columns with one value for all rows
#'
#' @param data A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A dataset with deleted unwanted columns.
#' @export
#'
#' @examples
#' prep_data <- pre_rm_static_cols(compas,'Two_yr_Recidivism')
pre_rm_static_cols <- function(data, y) {
  del <- c()
  for (i in 1:ncol(data)) {
    if (length(unique(data[, i])) == 1) {
      del <- c(del, i)
    }
  }
  if (!is.null(del)) {
    #cat('Removed static columns: ', paste0(colnames(data[del]), sep='; '), '.', '\n \n', sep = '')
    data <- data[, -del]
  } else{
    data <- data
    #cat('No static columns were removed. \n \n')
  }
  return(data)
}

#' Binarizes the target column
#'
#' @param data A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A dataset with binarized target column.
#' @export
#'
#' @examples
#' binarize_target(iris[1:100,],'Species')
binarize_target <- function(data, y){
  if(guess_type(data,y) == 'binary_clf'){
    #cat('Binarizing the target column. \n \n')
    data[[y]] <- as.integer(data[[y]])
  }else{
    #cat('No need for target binarization. \n \n')
  }

  return(data)
}

#' Manages missing values
#'
#' @param df A data source, that is one of major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string which indicates a target column name.
#'
#' @return A dataframe with removed and imputed missing values.
#' @export
manage_missing <- function(df, y) {
  # remove mostly missing columns
  col_to_rm <- c()
  for (i in 1:ncol(df)) {
    if (length(df[, i][df[, i] == '']) >= length(df[, i]) / 2) {
      col_to_rm <- c(col_to_rm, i)
    }

    if (is.numeric(df[, i]) == FALSE) {
      df[, i] <- as.factor(df[, i])
    }
  }
  if (length(col_to_rm) > 0) {
    df <- df[, -col_to_rm]
  }
  # input missing values via mice
  df <- mice::mice(df, seed = 123, print = FALSE)
  df <- mice::complete(df)

  return(df)
}

#' Saves column names deleted during preprocessing process
#'
#' @param df A data source before preprocessing, that is one of major R formats:
#' data.table, data.frame, matrix and so on.
#' @param pre_df A data source after preprocessing, that is one of major R formats:
#' data.table, data.frame, matrix and so on.
#'
#' @return A  vector of strings with column names.
#' @export
save_deleted_columns <- function(df, pre_df) {

  names_df        <- colnames(df)
  names_pre_df    <- colnames(pre_df)
  deleted_columns <- c()

  for (i in 1:length(names_df)) {
    if (!(names_df[i] %in% names_pre_df)) {
      deleted_columns <- c(deleted_columns, names_df[i])
    }
  }

  return(deleted_columns)
}
