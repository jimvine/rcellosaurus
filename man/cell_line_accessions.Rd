% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell_line-details.R
\name{cell_line_accessions}
\alias{cell_line_accessions}
\title{Find accessions from cell-lines}
\usage{
cell_line_accessions(cell_line, type = NULL)
}
\arguments{
\item{cell_line}{A node or nodeset referring to a cell-line or multiple cell-lines.}

\item{type}{A character vector specifying which types to output. If \code{NULL},
the function returns all values (i.e., unfiltered).}
}
\description{
Expected values for \code{type} are \code{c("primary", "secondary")}.
}
