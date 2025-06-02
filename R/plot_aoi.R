#' Plot area of interest
#'
#' @description
#' Create a simple ggplot of an AOI and, optionally, the 'base' vector used to create the aoi without buffers/filtering, and a region_taxa polygon.
#' Map extent is set to the wider of SA or the AOI, or can be set manually to just the AOI with `zoom`.
#'
#' @param extent_sf An `sf` object of the actual extent i.e. the AOI, e.g. from `tar_read(extent_sf)`.
#' @param extent_poly An `sf` object of the base vector used to build `extent_sf`, e.g. from H:/data/vector.
#' @param region_taxa_sf An `sf` object to add to the map, e.g. from tar_read(region_taxa_sf)
#' @param zoom Logical. Set the map extent to `extent_sf`? Works well with a `bslib::input_switch` input.
#'
#' @details
#' Requires `sa` and `aus` sf objects to build the map - usually as found in data/vector. They are NOT included in this package. Map crs is set to `crs(aus)`.
#'
#' @returns ggplot2 object with `extent_sf` overlayed on an SA/Aus map, with optional `extent_poly` and `region_taxa`.
#'
#' @export
#'

plot_aoi <- function(extent_sf,
                     extent_poly = NULL,
                     region_taxa_sf = NULL, #eg from tar_read(region_taxa_sf)
                     zoom = FALSE #zoom to extent_sf?
){

  extent_sf <- extent_sf %>%
    sf::st_transform(crs = sf::st_crs(aus))

  ## map limits
  if(zoom){
    #extent
    xlim <- sf::st_bbox(extent_sf)[c("xmin", "xmax")]
    ylim <- sf::st_bbox(extent_sf)[c("ymin", "ymax")]
  } else {
    #wider of extent/SA
    xmin <- min(sf::st_bbox(sa)["xmin"],
                sf::st_bbox(extent_sf)["xmin"])
    xmax <- max(sf::st_bbox(sa)["xmax"],
                sf::st_bbox(extent_sf)["xmax"])
    ymin <- min(sf::st_bbox(sa)["ymin"],
                sf::st_bbox(extent_sf)["ymin"])
    ymax <- max(sf::st_bbox(sa)["ymax"],
                sf::st_bbox(extent_sf)["ymax"])

    xlim  <- c(xmin, xmax)
    ylim  <- c(ymin, ymax)
  }

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = aus,
                     fill = 'white')

  if(!is.null(region_taxa_sf)) {
    map <- map +
      ggplot2::geom_sf(data = region_taxa_sf,
                       ggplot2::aes(colour = "region_taxa"),
                       lwd = 1.5, fill = NA)
  }
  if(!is.null(extent_poly)) {
    map <- map +
      ggplot2::geom_sf(data = extent_poly,
                       ggplot2::aes(colour = "extent_poly"),
                       fill = NA, lwd = 1.2)
  }

  map +
    ggplot2::geom_sf(data = extent_sf,
                     ggplot2::aes(colour = "extent"),
                     lwd = 1.2, fill = NA) +
    ggplot2::coord_sf(xlim, ylim) +
    ggplot2::scale_colour_manual(values = c("extent" = "coral",
                                            "region_taxa" = "blue",
                                            "extent_poly" = "grey20"),
                                 name = "") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey70", fill = NA))

}
