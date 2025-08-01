% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tar_meta.R
\name{get_tar_meta}
\alias{get_tar_meta}
\title{Build a table of targets metadata and progress}
\usage{
get_tar_meta(store)
}
\arguments{
\item{store}{Path to the store to check (usually tars$blah$store, or the reactive equivalent)}
}
\value{
A data frame with names of targets. Excludes targets with `_branches_` in the name.
}
\description{
Build a table of targets metadata and progress
}
\seealso{
[`envShiny::tar_meta_opts`] in the options argument of `DT::datatable()` is useful for easy consistent output formatting.
}
