#' Standard title panel used in various envProject shiny apps
#'
#' Displays page title as "title | description" on a green banner with the DEW logo.
#'
#' @param title Main name of the app/project
#' @param description Short descriptor of the app/project
#' @param window_title Text that appears in the browser header
#'
#' @returns Shiny taglist in a bslib::layout_columns wrapper.
#' @export
#'

title_panel <- function(title,
                        description,
                        window_title = title) {

  bslib::layout_columns(
    col_widths = c(10, -1, 1),

    shiny::titlePanel(
      title = htmltools::span(
        htmltools::span(title, style = "font-size:80%;"),
        "|",
        htmltools::strong(description)
      ),
      windowTitle = window_title),

    htmltools::img(src = "envShiny/DEW_horizontal_REV.png",
                   width = 250),

    max_height = "50px",
    class = "env-title-pane"
  )

}


.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "envShiny",
    directoryPath = system.file("www", package = pkgname)
  )
}
