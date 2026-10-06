#' Standardised card for holding leaflet maps, with a Shiny module
#'
#' Use `map_cardUI` in app UI, using a unique id value. Use `map_cardServer` in the shiny server
#' function, with a matching id value. Create the map to be displayed as a reactive outside of this
#' function.
#'
#' Card body is filled with a map with no padding. Designed mainly for 'side-by-side' maps. Card
#' footer contains controls for basemap (as side-by-side maps can't handle it with native leaflet
#' basemap controls) and, optionally, raster opacity.
#'
#' `map_cardServer` creates `output$map`, namespaced by the id. To update the map via
#' `leafletProxy`, use the `session` argument, i.e. `leafletProxy("map", session = ..)`.
#'
#' @param id Module namespace ID. Must match id used in `map_cardServer`.
#' @param header Optional header content. Use `bslib::card_header(..)` to add.
#' @param use_opacity Logical - add a raster opacity slider in the card footer?
#' @param leaflet_height Height of the leaflet UI. Sent to the `height` argument of the
#'   `leafletOutput`.
#' @param full_screen Logical, sent to the `full_screen` argument of the `card`.
#'
#' @returns For `map_cardUI`: A `bslib::card` UI object
#'
#' @rdname map_card_module
#'
#' @export

map_cardUI <- function(id,
                       header = NULL,
                       use_opacity = TRUE,
                       leaflet_height = 400,
                       full_screen = FALSE) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = full_screen,
    header,

    bslib::card_body(
      class = "p-0",
      leaflet::leafletOutput(ns("map"),
                             height = leaflet_height)
    ),

    bslib::card_footer(
      bslib::toolbar(
        align = "left",
        bslib::layout_columns(
          col_widths = c(4, -2, if(use_opacity) 6 else -8),
          style = "width:100%;",
          bslib::toolbar_input_select(ns("basemap"),
                                      "Basemap", show_label = TRUE,
                                      choices = c("OpenStreetMap", "ImageMapSA", "Esri imagery"),
                                      width = "50%"),
          if(use_opacity) {
            htmltools::div(class = "inline-slider",
                           shiny::sliderInput(ns("opacity"),
                                              "Raster opacity",
                                              value = 0.8, min = 0, max = 1, step = 0.1, ticks = FALSE)

            )
          } else NULL
        )
      )
    )
  )
}

#' @param id Module namespace ID. Must match id used in `map_cardUI`.
#' @param map Reactive leaflet map. Construct the base map layers to be included in the card as a reactive outside of this module.
#'
#' @returns For `map_cardServer`: List with this module's `session` object and map `opacity` and `basemap` controls.
#'
#' @rdname map_card_module
#'
#' @export

map_cardServer <- function(id, map) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$map <- leaflet::renderLeaflet(map())

      return(
        list(
          session = session,
          opacity = reactive(input$opacity),
          basemap = reactive(input$basemap)
        )
      )

    }
  )
}
