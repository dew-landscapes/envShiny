#' Download a shapefile from a shiny app
#'
#' Create and zip the components of an ESRI shapefile (i.e. .shp, .dbf, etc) into a single download. Use instead of shiny::downloadHandler

#' From https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
#'
#' @param data Data to be downloaded, usually a reactive. If multiple = TRUE, must be a list.
#' @param layer_names Name of the shapefile/s, ie will output layer_name.shp, layer_name.dbf, etc. If multiple = TRUE, should be same length as data list.
#' @param zip_name Name of the .zip folder downloaded.
#' @param multiple Download contains multiple shapefiles? Check requirements for data and layer_names if TRUE.
#'
#' @return A .zip file download from the shiny server, containing one or more shapefiles.
#'
#' @export
#'

download_shp <- function(data,
                         layer_names,
                         zip_name = if(!multiple) layer_names,
                         multiple = FALSE) {

  downloadHandler(
    filename = function() {
      paste0(zip_name, ".zip")
    },
    content = function(file) {

      tmp.path <- fs::path(tempdir(), as.integer(Sys.time()))

      if(multiple == TRUE){
        if(!is.list(data)) stop("data must be a list.")
        purrr::walk2(data, layer_names,
                     \(x, y)
                     sf::st_write(x,
                                  dsn = tmp.path,
                                  layer = y,
                                  driver = "ESRI Shapefile",
                                  append = FALSE,
                                  quiet = TRUE)
        )

      } else {

        sf::st_write(data,
                     dsn = tmp.path,
                     layer = layer_names,
                     driver = "ESRI Shapefile",
                     append = FALSE,
                     quiet = TRUE)
      }

      zip_file <- fs::path(tmp.path, paste0(zip_name, ".zip"))

      shp_files <- fs::dir_ls(tmp.path,
                              regexp = paste0(layer_names, collapse = "|"),
                              recurse = TRUE)

      zip::zipr(zipfile = zip_file,
                files = shp_files)

      file.copy(zip_file, file)

    }
  )
}
