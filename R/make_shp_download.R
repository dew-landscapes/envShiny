make_shp_download <- function(data,
                              layer_names,
                              zip_name = if(!multiple) layer_names,
                              multiple = FALSE) {

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

  return(zip_file)

}
