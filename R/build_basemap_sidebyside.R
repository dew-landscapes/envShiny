#' Create a standardised leaflet base map for two layers side-by-side
#'
#' Create a leaflet map upon which to add layers through `leaflet::add` functions or `leafletProxy`.
#' Uses leaflet.extras2::sidebyside to add a vertical slider to view two layers side-by-side.
#' Add additional layers to a single side by specifying `options = pathOptions(pane = 'left/right')`, or add layers above the slider by using the panes parameters with zIndex>1.
#'
#' Includes fewer options than `envShiny::build_basemap`, which is better for adding multiple layers.
#'
#'
#' @param panes Character vector of names to add as additional height-ordered layers. Call in added layers with options=pathOptions(pane = 'blah')
#' @param panes_zIndex Numeric vector of panes levels. Must be equal length to `panes`.
#' @param basemap Name of basemap layer to use. Note that `sidebyside` doesn't support layerControl for basemaps, so only one value is used. Supports default OpenStreetMap & ESRI World Imagery, and ImageMapSA via its public URL (which reverts to ESRI imagery outside its extent).
#' @param ... Passed to initial leaflet(), eg. for setting height.
#'
#' @returns A leaflet map, as an HTML widget object.
#'
#' @export

build_basemap_sidebyside <- function(panes = NULL,
                                     panes_zIndex = NULL,
                                     basemap = c("OpenStreetMap", "ImageMapSA", "Esri imagery"),
                                     ...
){

  if(length(panes) != length(panes_zIndex)){
    stop("Number of panes != number of panes_zIndex.")
  }
  if(length(basemap) > 1) {
    basemap <- basemap[1]
    warning(paste0("More than one basemap layer provided but only one is supported. Using '", basemap, "'."))
  }

  map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 4,
                                                            maxZoom = 18,
                                                            zoomControl = FALSE),
                          ...) |>
    htmlwidgets::onRender( #zoom controls
      "function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") |>
    leaflet::addScaleBar(position = "bottomright",
                         options = leaflet::scaleBarOptions(
                           imperial = FALSE)) |>
    leaflet::addMapPane("left", zIndex = 1) |>
    leaflet::addMapPane("right", zIndex = 1)

  if(basemap == "Esri imagery"){
    map <- map |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                group = "base",
                                layerId = "base_left",
                                options = leaflet::pathOptions(pane = "left")) |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                group = "base",
                                layerId = "base_right",
                                options = leaflet::pathOptions(pane = "right"))

  } else if(basemap == "ImageMapSA"){

    map <- map |>
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
  } else {

    map <- map |>
      leaflet::addTiles(layerId = "base_left",
                        group = "base",
                        options = leaflet::pathOptions(pane = "left")) |>
      leaflet::addTiles(layerId = "base_right",
                        group = "base",
                        options = leaflet::pathOptions(pane = "right"))

  }


  if(!is.null(panes)){
    for(i in 1:length(panes)){
      map <- map |>
        leaflet::addMapPane(name = panes[i],
                            zIndex = panes_zIndex[i])
    }
  }

  map <- map |>
    leaflet.extras2::addSidebyside(layerId = "sidebyside",
                                   rightId = "base_right", leftId = "base_left")

  return(map)
}

