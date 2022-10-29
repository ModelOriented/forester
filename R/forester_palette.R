#' Return colors from palette
#'
#' @param ... List of names of colors.
#'
#' @return A list of colors' codes.
#' @export
#'
#' @examples
#' forester_palette()
#' forester_palette('brown')
#' forester_palette('green', 'dark green')
forester_palette <- function(...) {
  forester_colors <- c(
    `dark green`  = '#7C843C',
    `green`       = '#D6E29C',
    `light green` = '#AFC968',
    `brown`       = '#B1805B',
    `dark brown`  = '#74533D')

  cols <- c(...)

  if (is.null(cols))
    return (forester_colors)

  forester_colors[cols]
}
