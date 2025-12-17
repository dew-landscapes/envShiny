#' Add custom legend to leaflet map
#'
#' Default leaflet legends only exist as a colour-filled square, this function allows customisation of size, shape, fill, and outline.
#' If style parameters are of unequal lengths they get recycled.
#' From [this stackoverflow question](https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends)
#'
#' @param map Leaflet map object
#' @param colors Vector of colour names or hex codes. Fill colour(s)
#' @param labels Character vector. Legend label(s)
#' @param sizes Numeric vector of legend shape size. Default 14 most closely matches default 'unstyled' leaflet legend.
#' @param shapes Vector of shapes. Only supports "square" or "circle", as shape comes only from the border-radius style of squares.
#' @param borders Vector of colour names or hex codes. Shape outline colour(s)
#' @param opacity Numeric between 0-1, length 1. Opacity of legend shapes
#' @param ... Other arguments to `leaflet::addLegend` (e.g. title)
#'
#' @return Leaflet legend added to the `map` provided
#' @export
#'
addLegendCustom <- function(map,
                            colors,
                            labels,
                            sizes = 14,
                            shapes = "square",
                            borders = colors,
                            opacity = 1,
                            ...){

  if(!any(shapes %in% c("square", "circle"))) {
    shapes = "square"
    warning("In envShiny::addLegendCustom: Unsupported shape, reverting to default 'square'.", call.=FALSE)
  }

  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ",
           sizes, "px;margin-top: 4px;line-height: ",
           sizes, "px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)


  return(
    map |>
      leaflet::addLegend(colors = legend_colors, labels = legend_labels, opacity = opacity, ...)
      )
}

