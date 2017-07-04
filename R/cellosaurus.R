# Cellosaurus functions for R.
# Copyright Jim Vine



#' rcellosaurus: A package for using the Cellosaurus dataset.
#'
#' The rcellosaurus provides functions for accessing the Cellosaurus dataset in
#' R. Cellosaurus is a knowledge resource on cell lines. It is online at
#' http://web.expasy.org/cellosaurus/
#'
#' This R package is not associated with Cellosaurus, and the good people at
#' Cellosaurus are not responsible for it in any way.
#'
#' The rcellosaurus package provides functions in two broad categories:
#' functions for finding cell-lines that match search terms and functions for
#' accessing information on those cell-lines. In addition,
#' \code{\link{read_cellosaurus_xml}} is used to read the Cellosaurus dataset.
#'
#' @section Finding cell-lines:
#' The following functions are provided to find cell-lines:
#' \itemize{
#'   \item \code{\link{cell_line_find_first}}, to find one cell-lines
#'   \item \code{\link{cell_line_find_all}}, to find multiple cell-lines
#' }
#'
#' @section Accessing cell-line information:
#' The following functions are provided to find cell-lines:
#' \itemize{
#'   \item \code{\link{cell_line_category}},
#'   \item \code{\link{cell_line_sex}},
#'   \item \code{\link{cell_line_names}},
#' }
#'
#' @docType package
#' @name rcellosaurus
NULL




#' Load a Cellosaurus xml dataset
#'
#' Cellosaurus publishes its dataset as an XML file, available for download
#' from ftp://ftp.expasy.org/databases/cellosaurus
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#'
#' @export
#'
read_cellosaurus_xml <- function(data) {
  xml2::read_xml(data)
}



cell_lines_all <- function(cellosaurus) {
  # This also works, but seems slower:
  # xml2::xml_children(xml2::xml_child(cellosaurus, 2))
  xml2::xml_find_all(cellosaurus, "./cell-line-list/*")
}


# =========== Find cell-lines ===========

#' Find cell-line
#'
#' @param cellosaurus
#'   An XML document containing the Cellosaurus dataset.
#' @param text
#'   Some text to search for. Finding is handled using XPath so some
#'   special characters may cause difficulties, including these: '/:[]*.
#'   Can take a character vector of length > 1, in which case will search for
#'   any of the terms (i.e., "or").
#'
#' @return An XML node containing a cell-line (or an \code{xml_missing} node if
#'   the search text is not found). If there are multiple matches, the first
#'   is returned.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' cell_line_find_first(cellosaurus, "CVCL_3449")
#'
#' # Can store the cell-line for use later.
#' CVCL_6873 <- cell_line_find_first(cellosaurus, "CVCL_6873")
#'
#' @export
#'
cell_line_find_first <- function(cellosaurus, text) {

  if(length(text) == 1) {
    xpath <- paste0(".//*[text()[contains(.,'", text,
                    "')]]/ancestor::cell-line")
  } else {
    xpath <- paste0(".//*[text()[contains(.,'",
                    paste(text, collapse = "') or contains(.,'"),
                    "')]]/ancestor::cell-line")
  }
  xml2::xml_find_first(cellosaurus, xpath)

}

#' Find all matching cell-lines
#'
#' @inheritParams cell_line_find_first
#'
#' @return An XML nodeset containing all cell-lines that contain the search
#'   text. If there are no matches, the nodeset will be empty.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#'
#' # Finding all the cell-lines that match "sapiens".
#' # As of Cellosaurus v22.1 (2017-05-17) returned 69593 results.
#' # Takes a minute or two on a reasonably quick modern laptop.
#' cell_line_find_all(cellosaurus, "sapiens")
#'
#' # Supplying a vector of multiple search terms will do an "or" search:
#' two_lines <- cell_line_find_all(cellosaurus, c("CVCL_E548", "CVCL_IW91"))
#' two_species <- cell_line_find_all(cellosaurus, c("Mus musculus",
#'                                                  "Cavia porcellus"))
#'
#' @export
#'
cell_line_find_all <- function(cellosaurus, text) {

  if(length(text) == 1) {
    xpath <- paste0(".//*[text()[contains(.,'", text,
                    "')]]/ancestor::cell-line")
  } else {
    xpath <- paste0(".//*[text()[contains(.,'",
                    paste(text, collapse = "') or contains(.,'"),
                    "')]]/ancestor::cell-line")
  }
  xml2::xml_find_all(cellosaurus, xpath)

}


