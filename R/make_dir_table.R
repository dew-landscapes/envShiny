
#' Make table from output directory structure
#'
#' Collect file paths and turn them into a table, with a column per level. Needed for projects that use multiple extents/grains.
#'
#' @param out_dir The *absolute* file path to an output directory.
#' @param dir_levels Character vector (treated as an orderd factor) describing the directory levels returned.
#' Default is usally fine, but some projects have an additional level below grain, e.g. 'aoi'
#' @param return_level Lowest directory level name to return. E.g. default 'grain' will return a df with names 'root' to 'grain'.
#'
#' @details
#' Additional filtering (e.g. to filter folders within 'grain' that don't follow the latest targets setup)
#' is best done by setting a higher `return_level` then performing the final `filter(return_level != "") |> distinct()` on the result.
#'
#' @return A data frame with one column per directory level, named from 'root' (e.g. H:) to `return_level`. Distinct rows per file path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' range_dir <- fs::path_abs(here::here("..", "..", "out", "envRange"))
#' regcont_dir <- fs::path_abs(here::here("..", "..", "out", "envRegCont"))
#'
#'
#' make_dir_table(range_dir)
#' make_dir_table(range_dir, return_level = "stores")
#'
#' make_dir_table(regcont_dir,
#'                dir_levels = c("root", "branch", "out", "proj", "extent", "grain",
#'                               "aoi"),
#'                return_level = "aoi"
#' )
#'
#' ## For an out-directory with non-targets folders within 'grain':
#' make_dir_table(cleaned_dir, return_level = "stores") |>
#'   dplyr::group_by(extent, grain) |>
#'   dplyr::filter("setup" %in% stores) |>   ## filter to newer folders that use targets
#'   dplyr::select(-stores) |>
#'   dplyr::distinct()
#' }
#'
make_dir_table <- function(out_dir,
                           dir_levels = c("root", "branch", "out", "project", "extent", "grain", "stores"),
                           return_level = "grain"){

  if(grepl("\\.\\.\\/", out_dir)) stop("Relative out_dir path detected; use absolute file paths with eg fs::path_abs(out_dir)")

  dir_levels <- factor(dir_levels, levels = dir_levels, ordered = TRUE)

  use_levels <- dir_levels[dir_levels <= return_level]

  recurse_levels <- length(dir_levels) - length(stringr::str_split(out_dir, pattern = "\\/")[[1]])

  folders <- fs::dir_ls(out_dir, recurse = recurse_levels, type = "directory") |>
    gsub(pattern = "/app/", replacement = "app/") #to handle sol server 'drive' naming

  res <- folders |>
    stringr::str_split_fixed(pattern = "/", n = max(stringr::str_count(folders, "/")) + 1) |>
    tibble::as_tibble(.name_repair = 'minimal') |>
    setNames(as.character(use_levels))

  res <- res[!is.na(names(res))]

  res |>
    dplyr::filter(get(return_level) != "") |>
    dplyr::distinct()

}
