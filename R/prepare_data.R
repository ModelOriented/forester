#' Automatic Function For Preparing Data
#'
#' Function \code{prepare_data} covers fundamental techniques in ML Feature Engineering process.
#'
#' With adjustable arguments, users can independently decide how function deals with NAs value
#' and important features selection. Furthermore, options for sampling methods will pop up in
#' case of classification problem.
#'
#' @param data_train dataset, used for training models. Class of data_train is one of those classes: data.frame, matrix, data.table or dgCMatrix. NOTE: data_train includes target column.
#' @param target character, indicating name of the target column in data_train.
#' @param type character, defining the task. Option is "regression" or "classification", namely binary classification.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#'
#' @return processed data as data frame 
#'
#'
#' @references forester library \url{https://modeloriented.github.io/forester/}


prepare_data  <- function(data_train, target, type, fill_na = TRUE, num_features = NULL){
  
  ### Conditions 
  data_train <- check_conditions(data_train, target, type)
  
  if (fill_na != TRUE & fill_na != FALSE){
    stop("Argument fill_na should be a logical variable: TRUE or FALSE")
  }
  
  if (!is.null(num_features) && !is.numeric(num_features)){
    stop("Argument num_features should be a numerical.")
  }
  
  num_rows <- nrow(data_train)
  num_cols <- ncol(data_train)
  message("Original shape of train data frame: ", num_rows, " rows, ", num_cols, " columns")

  message("_____________")
  message("NA values")
  ### Imputation:
  # Messages informing percentage of NAs in data_train and data_test:
  if (any(is.na(data_train))){
    message("percentage of NAs in data_train: ",sum(is.na(data_train))/prod(dim(data_train)) * 100, "%")
  } else {
    message("There is no NA values in your data.")
  }

  #### Filling NAs:
  # Creating formula
  # try to convert column names to names without znaki
  test_colnames <- lapply(colnames(data_train), function(x) gsub("_", "", x))
  test_colnames <- lapply(test_colnames, function(x) gsub("[.]", "", x))
  if (any(grepl('[[:punct:]]', test_colnames))) {
    stop("Column names are wrong for creating a formula. Column names can not contain special characters as '+', '-', '=', '*'. Try substitute special characters with '_' sign.")
  }
  form <- stats::as.formula(paste(target , "~."))

  if (fill_na){
    
    # Message for the user about imputation
    if (any(is.na(data_train))){
      message("NA values has been filled.")
    }
    
    #### Imputation with tidyvers 
    # deleting na values from target column 
    data_train <- data_train[!is.na(data_train[[target]]),]
    
    rec <- recipes::recipe(form, data = data_train)
    impute_step <- recipes::step_impute_knn(rec, recipes::all_predictors())
    impute_step_trained <- recipes::prep(impute_step, training = data_train)
    
    data_train <- as.data.frame(recipes::bake(impute_step_trained, data_train))

    model_recipe <- impute_step_trained
    
  } else {
    if (any(is.na(data_train))){
      message("Deleting rows with NA values in train set. If you want to fill NA values use fill_na = TRUE")
      data_train <- na.omit(data_train)
      message("Shape of data train frame after deleting NA values: ",
              nrow(data_train), " rows, ", ncol(data_train), " columns")
    }

    rec <- recipes::recipe(form, data = data_train)
    naomit_step <- recipes::step_naomit(rec, recipes::all_predictors())
    naomit_step_trained <- recipes::prep(naomit_step, training = data_train)
    
    data_train <- as.data.frame(recipes::bake(naomit_step_trained, data_train))

    model_recipe <- naomit_step_trained
  }
  
  
  
  ### Over or under sampling 
  if (type == "classification"){
    uniq <- unique(data_train[[target]])
    nrow_class_1 <- nrow(data_train[data_train[[target]] == uniq[1] , , drop = FALSE])
    nrow_class_2 <- nrow(data_train) - nrow_class_1
    percent_class_1 <- round(nrow_class_1/nrow(data_train) * 100, 4) 
    percent_class_2 <- round((100 - percent_class_1), 4)
    
    if (percent_class_1 < 10 | percent_class_1 > 90){
      message("_____________")
      message("Imbalanced data")
      message("Your training set has: ",nrow(data_train),"rows in total.")
      message("Class ", uniq[1], " accounts for ", percent_class_1, "%")
      message("Class ", uniq[2], " accounts for ", percent_class_2, "%")
      message("Your data might be umbalanced. Do you want to use oversampling or undersampling method?. Press 0, 1 or 2 to decide.")
      message("0 - nothing")
      message("1 - undersampling")
      message("2 - oversampling")
      message("What is your choice?")
      
      con <- getOption("mypkg.connection")
      choice <- readLines(con = con, n = 1)
      #choice <- readline()
      
      while (choice != 0 & choice != 1 & choice != 2){
        message("Wrong option. Choose option: ")
        choice <- readLines(con = con, n = 1)
        #choice <- readline()
      }
      
      # Starting sampling method:
      class_1_ind <- which(data_train[[target]] == uniq[1])
      class_2_ind <- which(data_train[[target]] == uniq[2])
      
      # Undersampling:
      if (choice == 1){
        n_samp <- min(length(class_1_ind), length(class_2_ind))
        ind1   <- sample(class_1_ind, n_samp)
        ind2   <- sample(class_2_ind, n_samp)
        data_train <- data_train[c(ind1,ind2),]
        message("Performing undersampling")
        message("Shape of data train frame after undersampling: ",
                nrow(data_train), " rows, ", ncol(data_train), " columns")
        
      } else if (choice == 2){
        # Oversampling
        n_samp <- max(length(class_1_ind), length(class_2_ind))
        ind1 <- sample(class_1_ind, n_samp, replace = !(length(class_1_ind) == n_samp))
        ind2 <- sample(class_2_ind, n_samp, replace = !(length(class_2_ind) == n_samp))
        data_train <- data_train[c(ind1,ind2), ]
        message("Performing oversampling")
        message("Shape of data train frame after oversampling: ",
                nrow(data_train), " rows, ", ncol(data_train), " columns")
      }
    }
  }

  ### Feature selection 
  if (!is.null(num_features)){
    message("_____________")
    message("Feature selection")
    tryCatch(
      {
        feat_imp <- Boruta::Boruta(data_train[,-which(names(data_train) == target)], data_train[[target]])
        imps <- Boruta::attStats(feat_imp)
        imps2 = imps[, c('meanImp'), drop = FALSE]
        top_features <- row.names(head(imps2, num_features))
        
        select_step <- recipes::step_select(model_recipe, all_of(c(top_features)))
        select_step_trained <- recipes::prep(select_step, training = data_train)
        model_recipe <- select_step_trained
        
        data_train <- data_train[, which(colnames(data_train) %in% c(top_features, target))]

      },
      warning = function(cond) {
        if(cond$message == "getImp result contains NA(s) or NaN(s); replacing with 0(s), yet this is suspicious."){
          message("Data set is probably to small for feature selection. If you're using undersampling try oversampling instead.")
        } else {
          message(cond$message)
        }
      }
    )
  }
  
  prepared_data <- list(data = data_train, modifications = model_recipe)
  class(prepared_data) <- "forester_prepared_data"
  
  return(prepared_data)
}

