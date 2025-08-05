#' Knit help files
#'
#' Knit .md files from .rmd files for display in shiny via includeMarkdown().
#'
#' @param help_dir Folder containing .rmd files.
#'
#' @return .md files, saved into help_dir
#' @export
#'

knit_help <- function(help_dir = here::here("shiny", "help")) {

  help_files <- gsub("*.Rmd$", "", list.files(help_dir, pattern = "*.Rmd$", recursive = T))

  purrr::walk(help_files,
              \(x) {
                infile <- fs::path(help_dir, paste0(x, ".Rmd"))
                outfile <- fs::path(help_dir, paste0(x, ".md"))
                ##knit only when changes made (though anything with evaluated R code will also update)
                if(!file.exists(outfile) || !all(readr::read_lines(infile) == readr::read_lines(outfile))) {
                  knitr::knit(infile,
                              output = outfile
                  )
                }
              }
  )

}
