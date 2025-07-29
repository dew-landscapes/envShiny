#' Download a shapefile from a shiny app
#'
#' Create and zip the components of an ESRI shapefile (i.e. .shp, .dbf, etc) into a single download. Use instead of shiny::downloadHandler

#' From https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
#'
#' @param data Data to be downloaded, usually a reactive
#' @param layer_name Name of the shapefile, ie will output layer_name.shp, layer_name.dbf, etc.
#' @param zip_name Name of the .zip folder downloaded.
#'
#' @return A file download from the shiny server.
#'
#' @seealso [download_shp_multiple()]
#'
#' @export
#'

download_shp <- function(data,
                         layer_name,
                         zip_name = layer_name) {

  downloadHandler(
    filename = function() {
      paste0(zip_name, ".zip")
    },
    content = function(file) {

      tmp.path <- tempdir()

      sf::st_write(data,
                   dsn = tmp.path,
                   layer = layer_name,
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet = TRUE)

      zip_file <- fs::path(tmp.path, paste0(zip_name, ".zip"))

      shp_files <- fs::dir_ls(tmp.path,
                              regexp = layer_name)

      zip::zipr(zipfile = zip_name,
                files = shp_files)

      file.copy(zip_name, file)

      file.remove(c(shp_files, zip_name))
    }
  )
}
