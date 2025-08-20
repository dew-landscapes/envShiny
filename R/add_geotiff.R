

#' Add a tif to a leaflet function (tweaked leafem::addGeotiff)
#'
#' This version uses file.copy instead of sf::gdal_utils to create the temp .tif that is drawn, to avoid huge file sizes (see \href{envSDMs #7}{https://github.com/dew-landscapes/envSDMs/issues/7})
#' Designed to work with rasters in 4326 and therefore this version uses project = FALSE as default; leafem uses TRUE and reprojects on the fly.
#'
#'  @param map the map to add the raster data to.
#' @param file path to the GeoTIFF file to render.
#' @param url url to the GeoTIFF file to render. Ignored if \code{file} is provided.
#' @param group he name of the group this raster image should belong to.
#' @param layerId the layerId.
#' @param resolution the target resolution for the simple nearest neighbor interpolation.
#'   Larger values will result in more detailed rendering, but may impact performance.
#'   Default is 96 (pixels).
#' @param bands which bands to use in case of multi-band Geotiff.
#' @param arith an optional function to be applied to a multi-layer object.
#'   Will be computed on-the-fly in the browser.
#' @param project **Change from leafem version:** default is FALSE and expects epsg:4326 rasters.
#' @param method character defining the resampling method to be used when
#' \code{project} is \code{TRUE}.
#' See \url{https://gdal.org/en/latest/programs/gdalwarp.html#cmdoption-gdalwarp-r} for
#' possible values.
#' @param opacity opacity of the rendered layer.
#' @param options options to be passed to the layer.
#'   See \code{\link[leaflet]{tileOptions}} for details.
#' @param colorOptions list defining the palette, breaks and na.color to be used.
#' @param rgb logical, whether to render Geotiff as RGB.
#' @param pixelValuesToColorFn optional JS function to be passed to the browser.
#'   Can be used to fine tune and manipulate the color mapping.
#'   See examples & \url{https://github.com/r-spatial/leafem/issues/25} for
#'   some examples.
#' @param autozoom whether to automatically zoom to the full extent of the layer.
#'   Default is \code{TRUE}
#' @param imagequery If \code{TRUE} a leaflet control with the hovered/clicked
#'   value will appear on the map.
#' @param imagequeryOptions additional options for the control panel.
#' @param ... currently not used.
#'
#' @return
#' A leaflet map object.
#' @export
#'
add_geotiff = function(map,
                       file = NULL,
                       url = NULL,
                       group = NULL,
                       layerId = NULL,
                       resolution = 96,
                       bands = NULL,
                       arith = NULL,
                       project = TRUE,
                       method = NULL,
                       opacity = 0.8,
                       options = leaflet::tileOptions(),
                       colorOptions = NULL,
                       rgb = FALSE,
                       pixelValuesToColorFn = NULL,
                       autozoom = TRUE,
                       imagequery = TRUE,
                       imagequeryOptions = NULL,
                       ...) {

  if (inherits(map, "mapview")) map = leafem:::mapview2leaflet(map)

  if (is.null(file) & is.null(url))
    stop("need either file or url!\n", call. = FALSE)

  if (is.null(group))
    group = basename(tools::file_path_sans_ext(file))

  if (is.null(layerId)) layerId = group
  layerId = gsub("\\.", "_", layerId)

  if (grepl("\\s", layerId)) {
    warning("The layerId is invalid. Maybe it contains spaces?")
  }

  if (missing(imagequeryOptions)) imagequeryOptions <- leafem::imagequeryOptions()
  imagequeryOptions[["imagequery"]] <- imagequery

  if (is.null(colorOptions)) {
    colorOptions = leafem::colorOptions()
  }

  if (is.null(arith)) {
    if (is.null(bands)) {
      bands = 1
    } else {
      bands = bands
    }
  }
  if (!is.null(arith)) {
    bands = leafem:::extractBands(arith)
    # bands = sort(bands) - min(bands)
  }

  # bands = sort(bands)
  # min_band = min(bands)

  if (!is.null(file)) {
    path_layer = tempfile()
    dir.create(path_layer)
    path_layer = paste0(path_layer, "/", layerId, "_layer.tif")

    file.copy(file, path_layer, overwrite = TRUE)
    # sf::gdal_utils(
    #   util = "translate"
    #   , source = file
    #   , destination = path_layer
    #   , options = c(
    #     "-b", bands,
    #     "-scale", 0, 1
    #   )
    # )

    # if (project) {
    #   path_layer_tmp = tempfile(fileext = ".tif")
    #   file.copy(path_layer, path_layer_tmp, overwrite = TRUE)
    #   # for some reason we need to delete the destination file for gdalwarp to work
    #   unlink(path_layer)
    #   method = ifelse(is.null(method), "near", method)
    #   sf::gdal_utils(
    #     util = "warp"
    #     , source = path_layer_tmp
    #     , destination = path_layer
    #     , options = c(
    #       "-t_srs", "EPSG:4326"
    #       , "-r", method
    #       , "-overwrite"
    #     )
    #   )
    # }

    bands = seq_along(bands)

    map$dependencies <- c(
      map$dependencies
      , leafem:::fileAttachment(path_layer, layerId)
      , leafem:::leafletGeoRasterDependencies()
      , leafem:::chromaJsDependencies()
    )

    leaflet::invokeMethod(
      map
      , data = leaflet::getMapData(map)
      , method = "addGeotiff"
      , url
      , group
      , layerId
      , resolution
      , bands - 1
      , leafem:::bandCalc(arith)
      , opacity
      , options
      , colorOptions
      , rgb
      , pixelValuesToColorFn
      , autozoom
      , imagequeryOptions
    )
  } else {
    map$dependencies <- c(
      map$dependencies
      , leafem:::leafletGeoRasterDependencies()
      , leafem:::chromaJsDependencies()
    )

    leaflet::invokeMethod(
      map
      , data = leaflet::getMapData(map)
      , method = "addGeotiff"
      , url
      , group
      , layerId
      , resolution
      , bands - 1
      , arith
      , opacity
      , options
      , colorOptions
      , rgb
      , pixelValuesToColorFn
      , autozoom
      , imagequeryOptions
    )
  }

}

