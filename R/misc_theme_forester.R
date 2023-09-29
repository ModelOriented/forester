#' Forester Theme for ggplot2 objects
#'
#' @return theme for ggplot2 objects
#' @export
#' @rdname theme_forester

theme_forester <- function() {
  theme_bw(base_line_size = 0) %+replace%
    theme(axis.ticks        = element_blank(),
          legend.background = element_blank(),
          legend.key        = element_blank(),
          panel.background  = element_blank(),
          panel.border      = element_blank(),
          strip.background  = element_blank(),
          plot.background   = element_blank(),
          complete          = TRUE,
          legend.direction  = 'horizontal',
          legend.position   = 'top',
          axis.line.y   = element_line(color = 'white'),
          axis.ticks.y  = element_line(color = 'white'),
          axis.title    = element_text(color = 'black'),
          plot.title    = element_text(color = 'black', size = 16, hjust = 0),
          plot.subtitle = element_text(color = 'black', hjust = 0),
          axis.text     = element_text(color = 'black', size = 10),
          strip.text    = element_text(color = 'black', size = 12, hjust = 0),
          axis.text.x   = element_text(color = 'black'),
          axis.text.y   = element_text(color = 'black'),
          panel.grid.major.y = element_line(color = 'grey90', size = 0.5, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey90', size = 0.5,  linetype = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank())

}

#' Forester color palettes for ggplot2 objects
#'
#' @param n number of colors for color palette
#'
#' @return color palette as vector of charactes
#' @export
#' @rdname colors_forester
colors_discrete_forester <- function(n = 2) {
  if (n == 1) return('#AFC968')
  if (n == 2) return(c( '#AFC968', '#B1805B'))
  if (n == 3) return(c( '#AFC968', '#B1805B', '#7C843C'))
  if (n == 4) return(c( '#AFC968', '#B1805B', '#7C843C', '#74533D'))
  if (n == 5) return(c( '#AFC968', '#B1805B', '#7C843C', '#74533D', '#D6E29C'))
  c( '#AFC968', '#B1805B', '#7C843C', '#74533D', '#D6E29C')[((0:(n-1)) %% 5) + 1]
}


#' @export
#' @rdname colors_forester
colors_diverging_forester <- function() {
  c('#7C843C', '#74533D')
}


