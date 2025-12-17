#' Update the base layer used in a side-by-side leaflet map
#'
#' Used after creating a map using [build_basemap_sidebyside]. Needed because [leafem::addsidebyside] doesn't support the use of in-map base layers controls.
#'
#' @param map An existing leaflet map object, usually piped from `leafletProxy()`
#' @param basemap The basemap layer which the map should be updated to use
#'
#' @details If another layer (e.g. a tif) is being drawn on top of the basemaps, and is 'swipeable', use
#' `map |> hideGroup("tif") |> showGroup("tif")` to ensure it stays on top of the basemaps.
#'
#'
#' @return A leaflet map, as an HTML widget object.
#' @export
#'
#'
update_sidebyside_basemap <- function(map,
                                      basemap = c("OpenStreetMap", "ImageMapSA", "Esri imagery")) {


  map <- if(basemap == "Esri imagery"){
    map |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                layerId = "base_left",
                                options = leaflet::pathOptions(pane = "left")) |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                layerId = "base_right",
                                options = leaflet::pathOptions(pane = "right"))

  } else if(basemap == "ImageMapSA"){
    map |>
      leaflet::addWMSTiles("https://imagemap.geohub.sa.gov.au/mapproxy/wms",
                           layers = "SAGovMosaic",
                           group = "base",
                           layerId = "base_left",
                           options = leaflet::pathOptions(pane = "left")) |>
      leaflet::addWMSTiles("https://imagemap.geohub.sa.gov.au/mapproxy/wms",
                           layers = "SAGovMosaic",
                           group = "base",
                           layerId = "base_right",
                           options = leaflet::pathOptions(pane = "right"))
  } else if(basemap == "OpenStreetMap"){
    map |>
      addTiles(group = "base",
               layerId = "base_left",
               options = leaflet::pathOptions(pane = "left")) |>
      addTiles(group = "base",
               layerId = "base_right",
               options = leaflet::pathOptions(pane = "right"))

  } else stop("No supported basemap provided; use 'OpenStreetMap', 'ImageMapSA', or 'Esri imagery'")

  map |>
    leaflet.extras2::removeSidebyside("sidebyside") |>
    leaflet.extras2::addSidebyside(layerId = "sidebyside",
                                   rightId = "base_right", leftId = "base_left")
}
