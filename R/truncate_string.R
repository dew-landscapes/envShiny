#' Truncate long text strings in a DT::datatable output
#'
#' @param cols Character vector of columns to apply truncation to
#' @param length Number of characters to truncate to
#'
#' @returns A list, to be used within `columnDefs` list in datatable `options` (see example)
#' @export
#'
#' @examples
#' \dontrun{
#' DT::datatable(
#' data.frame(text = "A very very very very very long string"),
#' options = list(
#'   columnDefs = list(
#'     truncate_string("text", 20)
#'   )
#' )
#' )
#' }
#'
truncate_string <- function(cols = "_all",
                            length = 60) {

  list(
    targets = cols,
    render = DT::JS(paste0("function(data, type, row, meta) {",
                    "return type === 'display' && data != null && data.length >", length, "?",
                    "'<span title=\"' + data + '\">' + data.substr(0, ", length, ") + '...</span>' : data;",
                    "}"))
  )

}

