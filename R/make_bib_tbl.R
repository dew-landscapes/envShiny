#' Prepare a bio_all.bib file for inclusion in a metadata bibliography
#'
#' Uses a single .bib file to gather data names and their respective bibtex and reference-list formats.
#'
#' @param bibfile Path to .bib file
#'
#' @return Dataframe with cols 'data_name', 'ref' (plain text) and 'bibtex'
#' @export
#'
#' @examples
#' \dontrun{
#' make_bib_tbl("H:/data/envOcc/aus_imcra_prov_dissolve______0__P50Y/bio_all.bib")
#' }
#'
make_bib_tbl <- function(bibfile){

  ## bibtex (.bib format)
  bibtex_df <- readr::read_lines(bibfile) |>
    tibble::as_tibble() |>
    dplyr::mutate(entry_flag = dplyr::case_when(grepl("@Misc", value, ignore.case = TRUE) ~TRUE,
                                                .default = FALSE),
                  entry = cumsum(entry_flag)) |>
    dplyr::summarise(bibtex = paste(value, collapse = "\n"), .by = entry) |>
    dplyr::mutate(data_name = stringr::str_extract(bibtex, "(?<=\\{)[^,]+")) |>
    dplyr::select(data_name, bibtex) |>
    dplyr::distinct()

  ## plain text (bibliography format)
  bib <- bibtex::read.bib(bibfile)
  bib_df <- purrr::map(names(bib),
                       \(x)
                       tibble::tibble(data_name = x,
                                      ref = format(bib[x], "text"))
  ) |>
    dplyr::bind_rows()

  bib_tbl <- dplyr::left_join(bib_df, bibtex_df)

  return(bib_tbl)
}
