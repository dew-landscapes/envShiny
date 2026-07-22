#' Set a shiny app to use envShiny styles
#'
#' Also calls `shinyjs::useShinyjs()`. Needed for eg `map_card`'s raster opacity slider
#'
#' @returns
#' @export
#'
use_styles <- function() {

  css_files <- fs::dir_ls(system.file("styles", package = "envShiny"),
                          regexp = "css$")

  htmltools::tagList(
    purrr::map(css_files, htmltools::includeCSS),
    shinyjs::useShinyjs()
  )
}
