#' Colour text for use in HTML output (eg help docs)
#'
#' @description
#'
#' @details From https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html
#'
#' @param text Text to display
#' @param colour Valid css colour
#'
#' @returns Character string
#' @export
#'

html_colourise <- function(text, colour) {
  sprintf("<span style='color: %s;'>%s</span>",
          colour,
          text)
}
