#' Make a zipfile of shapefile parts to be downloaded.
#'
#' Create the components of one or more shapefiles, and zip them to a single file that is ready for
#' \code{shiny::downloadHandler}. Assign to an object (e.g. `zipfile`) within the function in the \code{content}
#' argument of downloadHandler, then use \code{file.copy(zipfile, file)} (see example).
#'
#' @details Replaces \code{envShiny::download_shp} (see [envShiny #2](https://github.com/dew-landscapes/envShiny/issues/2)).
#'
#' @param data Data to be downloaded, usually a reactive. If multiple = TRUE, must be a list.
#' @param layer_names Name of the shapefile/s, ie will output layer_name.shp, layer_name.dbf, etc. If multiple = TRUE,
#'   should be same length as data list - one name for each shapefile.
#' @param zip_name Name of the .zip folder downloaded. MUST match the name provided to the filename argument of
#'   shiny::download_handler.
#' @param multiple Download contains multiple shapefiles? Check requirements for data and layer_names if TRUE.
#' @param metadata Optionally, an object of class `WbWorkbook` from \code{openxlsx2} to include as metadata (.xlsx file) in the downloaded zip.
#'
#' @return A temp path to the .zip file ready to be downloaded.
#' @export
#'
#' @examples
#' \dontrun{
#' output$download <- downloadHandler(
#' filename = function() {
#'   paste0("download_", input$taxa, ".zip")
#' },
#' content = function(file) {
#'   zipfile <- make_shp_download(data = data(),
#'                                layer_names = paste0("data_", input$taxa),
#'                                zip_name = paste0("download_", input$taxa),
#'                                multiple = FALSE)
#'   file.copy(zipfile, file)
#' }
#' )
#' }
make_shp_download <- function(data,
                              layer_names,
                              zip_name = if(!multiple) layer_names,
                              multiple = FALSE,
                              metadata = NULL) {

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

  if(!is.null(metadata)) {
    openxlsx2::wb_save(metadata,
                       file = fs::path(tmp.path, "metadata.xlsx"))

    metadata <- fs::path(tmp.path, "metadata.xlsx")
  }

  zip_file <- fs::path(tmp.path, paste0(zip_name, ".zip"))

  shp_files <- fs::dir_ls(tmp.path,
                          regexp = paste0(layer_names, collapse = "|"),
                          recurse = TRUE)

  zip::zipr(zipfile = zip_file,
            files = c(shp_files, metadata))

  return(zip_file)

}
