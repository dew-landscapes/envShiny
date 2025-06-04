#' Create a standard leaflet base map
#'
#' Create a leaflet map upon which to add layers through `leaflet::add` functions or `leafletProxy`.
#' Includes controls for panes zIndex (i.e. layer heights), basemaps, and sets controls positions.
#'
#' @param panes Character vector of names to add as additional height-ordered layers. Call in added layers with options=pathOptions(pane = 'blah'). Basemaps are automatically included at zIndex=1.
#' @param panes_zIndex Numeric vector of panes levels. Must be equal length to `panes`.
#' @param use_basemaps Names of leaflet basemaps. Defaults are OpenStreetMap & ESRI World Imagery (built in to leaflet), and ImageMapSA via its public URL (which reverts to ESRI imagery outside its extent).
#' @param ... Passed to initial leaflet(), eg. for setting height.
#'
#' @returns A leaflet map, as an HTML widget object.
#'
#' @export

build_basemap <- function(panes = NULL,
                          panes_zIndex = NULL,
                          use_basemaps = c("OpenStreetMap", "ImageMapSA", "Esri imagery"),
                          ...
){

  if(length(panes) != length(panes_zIndex)){
    stop("Number of panes != number of panes_zIndex.")
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
    leaflet::addMapPane("basemaps", zIndex = 1) |>
    leaflet::addTiles(options = leaflet::pathOptions(pane = "basemaps")) |> #OSM
    leaflet::addLayersControl(baseGroups = unique(c("OpenStreetMap", use_basemaps)),
                              position = "topleft",
                              options = list(collapsed = FALSE))

  if("Esri imagery" %in% use_basemaps){
    map <- map |>
      leaflet::addProviderTiles("Esri.WorldImagery",
                                group = "Esri imagery",
                                options = leaflet::pathOptions(pane = "basemaps"))
  }

  if("ImageMapSA" %in% use_basemaps){
    map <- map |>
      leaflet::addWMSTiles("https://imagemap.geohub.sa.gov.au/mapproxy/wms",
                           layers = "SAGovMosaic",
                           group = "ImageMapSA",
                           options = leaflet::pathOptions(pane = "basemaps"))
  }


  if(!is.null(panes)){
    for(i in 1:length(panes)){
      map <- map |>
        leaflet::addMapPane(name = panes[i],
                            zIndex = panes_zIndex[i])
    }
  }

  return(map)
}

