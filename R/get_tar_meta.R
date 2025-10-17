#' Build a table of targets metadata and progress
#'
#' @param store Path to the store to check (usually tars$blah$store, or the reactive equivalent)
#'
#' @returns A data frame with names of targets. Excludes targets with `_branches_` in the name.
#'
#' @seealso [`envShiny::tar_meta_opts`] in the options argument of `DT::datatable()` is useful for easy consistent output formatting.
#'
#'
#' @export

get_tar_meta <- function(store){

  prog <- targets::tar_progress(names = !contains("_branches_"),
                       store = store)
  meta <- targets::tar_meta(names = !contains("_branches_"),
                        store = store,
                        targets_only = TRUE) |>
    dplyr::arrange(time)

  res <- meta |>
    dplyr::left_join(prog) |>
    dplyr::mutate(date = format(time, "%d-%b-%Y"), time = format(time, "%H:%M"),
                  seconds = round(seconds, 2),
                  size = paste0(fs::fs_bytes(bytes), "B")) |>
    dplyr::mutate(size = gsub("(?<=\\d)(?=[A-Za-z])", " ", size, perl = TRUE)) |>
    dplyr::select(name, progress, date, time, seconds, size, format, warnings)

  res
}
