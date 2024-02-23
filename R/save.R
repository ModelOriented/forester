#' Save elements from forester
#'
#' @param train The return from `train` function.
#' @param file A string describing the name and path of the file. By default set to
#' NULL which creates a `forester_{timestamp}` in current working directory.
#' @param verbose  A logical value, if set to TRUE, provides all information about
#' training process, if FALSE gives none.
#' @param return_name A logical value, if set to TRUE, function returns \
#' full path and name of the saved file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Training the model.
#' train <- train(data        = iris,
#'                y           = 'Species',
#'                bayes_iter  = 0,
#'                random_eval = 3)
#'
#' # Saving the outcomes.
#' save_forest(train, 'saved_train')
#'
#' # Reading saved file.
#' train2 <- readRDS('saved_train.RData')
#' }
save_forest <- function(train,
                        file        = NULL,
                        verbose     = TRUE,
                        return_name = FALSE) {
  if (is.null(file)) {
    time <- unclass(as.POSIXlt(Sys.time()))
    name <- paste('forester',
                  time$mday,
                  time$mon + 1,
                  substr(time$year, 2, 3),
                  time$hour,
                  time$min,
                  round(time$sec),
                  '.RData',
                  sep = '_')
  } else {
    name <- paste(file, '.RData', sep = '')
  }
  saveRDS(train[names(train)], file = name)
  verbose_cat('File: "', name, '" saved successfully.', sep = '', verbose = verbose)
  if (return_name) {
    return(name)
  }
}
