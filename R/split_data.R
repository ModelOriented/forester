

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
