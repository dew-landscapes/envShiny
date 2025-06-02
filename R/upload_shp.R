#' Handle shapefiles in `shiny::fileInput`
#'
#' Shapefiles require at least .shp, .shx, and .dbf (plus ideally .prj) components, but `shiny::fileInput` doesn't handle this well for `sf` reading functions.
#' This function prepares a set of uploaded shapefile components for reading by renaming them in the shiny temp directory before using sf::st_read on that directory.
#'
#' From https://stackoverflow.com/questions/67309399/allow-user-to-upload-a-shapefile-in-shiny
#'
#' @param shp_path Result of `input${id}` from `fileInput('id')` - df with name, path etc
#'
#' @returns An sf object, read from the shiny temp directory created by `fileInput`
#'
#' @export
#'

upload_shp <- function(shp_path) {

  infiles <- shp_path$datapath # get the location of files

  dir <- unique(dirname(infiles)) # get the directory

  outfiles <- file.path(dir, shp_path$name) # create new path name

  name <- gsub(paste(".shp",".dbf",".sbn",".sbx",".shx",".prj", sep = "|"), "", shp_path$name[1]) # strip file extension

  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files

  x <- sf::read_sf(file.path(dir, paste0(name, ".shp"))) # read in shapefile

  return(x)
}
