#' Print the provided cat-like input if verbose is TRUE
#'
#' @param ... R objects - strings, (see `cat` documentation).
#' @param sep A character vector of strings to append after each element.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#'
#' @export
verbose_cat <- function(..., sep = ' ', verbose = TRUE) {
  if (verbose) {
    cat(..., sep = sep)
  }
}
