#' Download several shapefiles from a shiny server
#'
#' Writes individual shapefiles (with all necessary components, i.e. .shp, .dbf etc), and zips them into a single download.
#'
#'
#'
#' @param data A list object of datasets to download. Must be named unless layer_names is specified.
#' @param layer_names Names to save individual layers as. Length must match length of data - usually should use names(data) or some derivitive, e.g `paste0("data_", names(data))``
#' @param zip_name Name of the .zip folder downloaded.
#'
#' @return A .zip download from the shiny server containing all supplied data.
#'
#' @seealso [download_shp()]
#' @export
#'
#'
#'
download_shp_multiple <- function(data,
                                  layer_names = names(data),
                                  zip_name) {

  downloadHandler(
    filename = function() {
      paste0(zip_name, ".zip")
    },
    content = function(file) {

      tmp.path <- fs::path(tempdir(), as.integer(Sys.time()))

      purrr::walk2(data, layer_names,
                   \(x, y)
                   sf::st_write(x,
                                dsn = tmp.path,
                                layer = y,
                                driver = "ESRI Shapefile",
                                append = FALSE,
                                quiet = TRUE)
                  )

      zip_file <- fs::path(tmp.path, paste0(zip_name, ".zip"))

      shp_files <- fs::dir_ls(tmp.path,
                              regexp = paste0(layer_names, collapse = "|"))

      zip::zipr(zipfile = zip_name,
                files = shp_files)

      file.copy(zip_name, file)

      file.remove(c(shp_files, zip_name))
    }
  )
}
