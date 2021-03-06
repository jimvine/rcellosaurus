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
#' @param data A Cellosaurus XML data file
#'
#' @return An XML document object.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml(system.file("extdata",
#'                                                 "cellosaurus.xml",
#'                                                 package = "rcellosaurus"))
#'
#' @export
read_cellosaurus_xml <- function(data) {
  xml2::read_xml(data)
}


#' Get all cell-line elements from Cellosaurus xml dataset
#'
#' Having read a Cellosaurus dataset with \code{\link{read_cellosaurus_xml}},
#' this function extracts all the cell-line elements as an \code{xml_nodeset}.
#'
#' Once you have used this function, you will typically want to use
#' \code{\link{cell_lines_filter}} to find the cell-lines that are of interest
#' to you.
#'
#' @param cellosaurus
#'   An XML document containing the Cellosaurus dataset.
#'
#' @return An XML nodeset containing all cell-lines in the dataset.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml(system.file("extdata",
#'                                                 "cellosaurus.xml",
#'                                                 package = "rcellosaurus"))
#' cell_lines <- cell_lines_all(cellosaurus)
#'
#' @export
cell_lines_all <- function(cellosaurus) {
  # This also works, but seems slower:
  # xml2::xml_children(xml2::xml_child(cellosaurus, 2))
  xml2::xml_find_all(cellosaurus, "./cell-line-list/*")
}


# =========== Find cell-lines ===========

#' Find cell-line
#'
#' Simple search function that searches text elements within the Cellosaurus
#' XML document, to find entries that contain any of the terms provided in
#' the \code{text} parameter. For greater control, consider using
#' \code{\link{cell_lines_filter}} instead.
#'
#' This function provides a quick way to find matching text, but is constrained
#' in a couple of important ways. It will only search in text sections of the
#' XML dataset (i.e., the values between XML tags). This means that it cannot
#' find anything that Cellosaurus stores in attributes (e.g. \code{sex=''}
#' entries). Conversely, the function will search in all of the text sections,
#' without discrimination; it cannot search only in specific sections. For
#' more advanced control, use the \code{\link{cell_lines_filter}} function,
#' which has neither of these constraints.
#'
#' @param cellosaurus
#'   An XML document containing the Cellosaurus dataset.
#' @param text
#'   Some text to search for. Finding is handled using XPath so some
#'   special characters may cause difficulties, including these: '/:[]*.
#'   Can take a string or a character vector of length > 1. If providing a
#'   vector, the function will search for cell-lines that match any of the
#'   terms (i.e., "or").
#'
#' @return An XML node containing a cell-line (or an \code{xml_missing} node if
#'   the search text is not found). If there are multiple matches, the first
#'   is returned.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml(system.file("extdata",
#'                                                 "cellosaurus.xml",
#'                                                 package = "rcellosaurus"))
#' cell_line_find_first(cellosaurus, "CVCL_3449")
#'
#' # Can store the cell-line for use later.
#' CVCL_6873 <- cell_line_find_first(cellosaurus, "CVCL_6873")
#'
#' @export
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
#' Simple search function that searches text elements within the Cellosaurus
#' XML document, to find entries that contain any of the terms provided in
#' the \code{text} parameter. For greater control, consider using
#' \code{\link{cell_lines_filter}} instead.
#'
#' This function provides a quick way to find matching text, but is constrained
#' in a couple of important ways. It will only search in text sections of the
#' XML dataset (i.e., the values between XML tags). This means that it cannot
#' find anything that Cellosaurus stores in attributes (e.g. \code{sex=''}
#' entries). Conversely, the function will search in all of the text sections,
#' without discrimination; it cannot search only in specific sections. For
#' more advanced control, use the \code{\link{cell_lines_filter}} function,
#' which has neither of these constraints.
#'
#' @inheritParams cell_line_find_first
#'
#' @return An XML nodeset containing all cell-lines that contain the search
#'   text. If there are no matches, the nodeset will be empty.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml(system.file("extdata",
#'                                                 "cellosaurus.xml",
#'                                                 package = "rcellosaurus"))
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


