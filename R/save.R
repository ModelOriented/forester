#' Save elements from forester
#'
#' @param train The return from `train` function.
#' @param list The list of names of elements from train. By default `all` save every element.
#' @param name A name of the file. By default `forester_{timestamp}`.
#' @param path A path to save the file. By default current working directory.
#' @param verbose  A logical value, if set to TRUE, provides all information about
#' training process, if FALSE gives none.
#' @param return_name A logical value, if set to TRUE, function returns \
#' full path and name of the saved file.
#'
#' @export
#'
#' @examples
#' train <- train(iris, 'Sepal.Width')
#' save(train)
save <- function(train,
                 list = 'all',
                 name = NULL,
                 path = NULL,
                 verbose = TRUE,
                 return_name = FALSE) {
  if (list == 'all') {
    list <- names(train)
  } else if (list == 'models') {
    list <- 'models_list'
  }

  if (is.null(name)) {
    time <- unclass(as.POSIXlt(Sys.time()))
    name <- paste(path,
                  'forester',
                  time$mday,
                  time$mon + 1,
                  substr(time$year, 2, 3),
                  time$hour,
                  time$min,
                  round(time$sec),
                  '.RData',
                  sep = '_')
  } else {
    name <- paste(path, name, '.RData', sep = '')
  }

  saveRDS(train[list], file = name)
  verbose_cat('File: "', name, '" saved successfully.', sep = '', verbose = verbose)
  if (return_name) {
    return(name)
  }
}
