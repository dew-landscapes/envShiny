#' Methods for adding a geotiff to a leaflet map
#'
#' Calls either `addCOG` or `addGeotiff` (from leafem) to add a tif file to a leaflet map, based on
#' some test (`use_reproject`) of if a reprojected COG exists.
#'
#' @param map Leaflet map, usually via `leafletProxy() |> `
#' @param use_reproject Logical: one or more tests for if a COG should be used
#' @param reproj_file File path to the COG
#' @param original_file File path to the original tif, for when use_reproject == FALSE
#' @param prefix Prefix used in `shiny::addResourcePath`; forms part of the url in addCOG
#' @param ... Passed to the leafem function, e.g. visual options. Note that some arguments (e.g. `colorOptions`) aren't currently supported by addCOG.
#'
#' @returns
#' @export
#'
#' @examples
#'\dontrun{
#'  add_tif(use_reproject = loaded$predmaps == "fine" && input$select_tif_type == "reproject" && sel_taxa_files()$predict_reproj_exists,
#'          reproj_file = sel_taxa_files()$predict_reproj_file,
#'          original_file = sel_taxa_files()$predict_fine_file,
#'          group = "Prediction")
#'}
#'
#'
#'
add_tif <- function(map,
                    use_reproject,  #logical: input$select_tif_type == "reproject" && sel_taxa_files()$thresh_reproj_exists
                    reproj_file,    #file path: sel_taxa_files()$thresh_reproj_file
                    original_file,  #file path: sel_taxa_files()$thresh_file
                    prefix,         #paste0(input$select_grain, loaded_taxa$selection),
                    ...) {
  if(use_reproject) {
    url <- paste0(prefix, "/",
                  basename(reproj_file))
    leafem::addCOG(map,
                   url = url,
                   ...)
  } else {
    leafem::addGeotiff(map,
                       file = original_file,
                       project = TRUE, ...)
  }
}
