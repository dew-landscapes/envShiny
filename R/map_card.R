#' Standardised card for holding leaflet maps
#'
#' Card body is filled with a map with no padding. Designed mainly for 'side-by-side' maps.
#' Card footer contains controls for basemap (as side-by-side maps can't handle it with native leaflet basemap controls)
#' and, optionally, raster opacity.
#'
#' @details Requires [`envShiny::use_styles`] to be called in ui to style raster opacity slider.
#'
#'
#' @param map_id Output ID of the leaflet map.
#' @param basemap_id,opacity_id Input ID of basemap controls and raster opacity. Raster opacity is optional - set to NULL to remove.
#' @param header Optional header content. Use `bslib::card_header(..)` to add.
#'
#' @returns
#' @export
#'
#' @examples
map_card <- function(map_id,
                     basemap_id,
                     opacity_id,
                     header = NULL,
                     leaflet_height = 400,
                     full_screen = FALSE) {

    bslib::card(
      full_screen = full_screen,
      header,

      bslib::card_body(
        class = "p-0",
        leaflet::leafletOutput(map_id,
                               height = leaflet_height)
      ),

      bslib::card_footer(
        bslib::toolbar(
          align = "left",
          bslib::layout_columns(col_widths = c(4,-2,6),
                                style = "width:100%;",
                                bslib::toolbar_input_select(basemap_id,
                                                            "Basemap", show_label = TRUE,
                                                            choices = c("OpenStreetMap", "ImageMapSA", "Esri imagery"),
                                                            width = "50%"),
                                if(!is.null(opacity_id)) {
                                  htmltools::div(class = "inline-slider",
                                                 shiny::sliderInput(opacity_id,
                                                                    "Raster opacity",
                                                                    value = 0.8, min = 0, max = 1, step = 0.1, ticks = FALSE)

                                  )
                                } else NULL
          )
        )
      )
    )
}
