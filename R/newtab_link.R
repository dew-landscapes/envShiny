#' Create a 'new tab' hyperlink
#'
#' Takes a text string, appends an icon, and handles the html to open in a new tab.
#'
#' @param text Clickable text to appear on the page. Is followed by a new tab icon.
#' @param url Link to open.
#' @param name Text to show when hovering over link text.
#'
#' @return An HTML string that is displayed as a hyperlink
#' @export
#'

newtab_link <- function(text,
                        url,
                        name = NULL) {

  htmltools::a(
    htmltools::span(text, shiny::icon("arrow-up-right-from-square")),
    title = name,
    href = url,
    target = "_blank")
}
