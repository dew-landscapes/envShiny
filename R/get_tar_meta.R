#' Depreciated to `envTargets`: Build a table of targets metadata and progress
#'
#' @param store Path to the store to check (usually tars$blah$store, or the reactive equivalent)
#'
#' @returns A data frame with names of targets. Excludes targets with `_branches_` in the name.
#'
#' @seealso [`envShiny::tar_meta_opts`] in the options argument of `DT::datatable()` is useful for easy consistent output formatting.
#'
#'
#' @export

get_tar_meta <- function(store) {

  lifecycle::deprecate_warn(
    "29 June 2026",
    what = "envShiny::get_tar_meta()",
    with = "envTargets::get_tar_meta()"
  )

  envTargets::get_tar_meta(store)

}
