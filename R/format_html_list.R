#' Format a character vector to a bulleted/numbered list
#'
#' @param char Character vector of items to be listed
#' @param ordered Use numbers (TRUE) or bullets (FALSE; default)
#'
#' @returns HTML character string.
#'
#' @export
#'
format_html_list <- function(char, ordered = FALSE){

  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")

  bullets <- paste0(seps[1], char, seps[2], collapse = "")

  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])

  htmltools::HTML(html_list)
}
