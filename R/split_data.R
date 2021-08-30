#' Splitting Data Into Train And Test Set
#'
#' \code{split_data} divide the data frame with given target and type into 
#' data train and data test set with adjustable ratio.
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - training data set to create model, if data_test = NULL, then data will be
#' automatically divided into training and testing dataset. NOTE: data has to contain the target column.
#' @param target character: name of the target column, should be character and has to be column name in data.
#' @param type character: defining the task. Two options are: "regression" and "classification", particularly, binary classification.
#' @param ratio numeric, ranged from between 0 and 1, indicating the proportion of data train over original data set.
#'
#' @return A list of data train and data test.
#'



split_data <- function(data, target, type, ratio = 0.8){
  if (type == "regression"){
    # Split data in ratio 4:1
    sample_size <- floor(ratio * nrow(data))
    train_index <- sample(seq_len(nrow(data)), size = sample_size)
    
    data_train <- data[train_index,]
    data_test  <- data[-train_index,]
  }
  
  if (type=="classification"){
    # Split data by ratio 4:1, while stratification is needed.
    uniq <- unique(data[[target]])
    data_negative <- data[data[[target]] == uniq[1],]
    data_positive <- data[data[[target]] == uniq[2],]
    
    sample_size_pos <- floor(ratio * nrow(data_positive))
    sample_size_neg <- floor(ratio * nrow(data_negative))
    
    train_index_pos <- sample(seq_len(nrow(data_positive)),size = sample_size_pos)
    train_index_neg <- sample(seq_len(nrow(data_negative)),size = sample_size_neg)
    
    data_train <- rbind(data_positive[train_index_pos,], data_negative[train_index_neg,])
    data_test  <- rbind(data_positive[-train_index_pos,], data_negative[-train_index_neg,])
  }
  
  ### Shuffering rows in data_train and data_test:
  rows_train <- sample(nrow(data_train))
  rows_test  <- sample(nrow(data_test))
  
  data_train <- data_train[rows_train,]
  data_test  <- data_test[rows_test,]
  
  return(list(data_train, data_test))
}
