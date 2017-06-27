# Cellosaurus functions for R.
# Copyright Jim Vine


# Seems to take an age.
# Not exporting it for now. May not be any use for it.
read_cellosaurus_list <- function(data) {
  xml2::as_list(xml2::read_xml(data))
}


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


#' Find cell-line
#'
#' @param cellosaurus
#'   An XML document containing the Cellosaurus dataset.
#' @param text
#'   Some text to search for. Finding is handled using XPath so some
#'   special characters may cause difficulties, including these: '/:[]*.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' cell_line_find_first(cellosaurus, "CVCL_3449")
#' # Can store the cell-line for use later.
#' CVCL_6873 <- cell_line_find_first(cellosaurus, "CVCL_6873")
#'
#' @export
#'
cell_line_find_first <- function(cellosaurus, text) {
  xml2::xml_find_first(cellosaurus,
                       paste0(".//*[text()[contains(.,'", text,
                              "')]]/ancestor::cell-line"))
}

#' Find all matching cell-lines
#'
#' @examples
#' # Finding all the cell-lines that match "sapiens".
#' # As of Cellosaurus v22.1 (2017-05-17) returned 69593 results.
#' # Takes a minute or two on a reasonably quick modern laptop.
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' cell_line_find_all(cellosaurus, "sapiens")
#'
#' @export
#'
cell_line_find_all <- function(cellosaurus, text) {
  xml2::xml_find_all(cellosaurus,
                     paste0(".//*[text()[contains(.,'", text,
                            "')]]/ancestor::cell-line"))
}

#' Category of a cell-line
#'
#' Use much like cell_line_sex
#'
#' @export
#'
cell_line_category <- function(cell_line) {
  xml2::xml_attr(cell_line, "category")
}

#' Sex of a cell-line
#'
#' @examples
#' # First read the dataset in.
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' # Can call on a nodeset that's already been found. E.g.:
#' CVCL_E548 <- cell_line_find_first(cellosaurus, "CVCL_E548")
#' cell_line_sex(CVCL_E548)
#' # Or could nest the sex with the the finder:
#' cell_line_sex(cell_line_find_first(cellosaurus, "CVCL_3449"))
#' @export
cell_line_sex <- function(cell_line) {
  xml2::xml_attr(cell_line, "sex")
}



#' Find names from a cell-line
#'
#' \code{cell_line_names_all} finds the name entries from a
#' cell-line's name-list. Does not filter based on \code{type} so
#' may include entries marked as types both \code{identifier} and
#' \code{synonym}.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' CVCL_E548 <- cell_line_find_first(cellosaurus, "CVCL_E548")
#' cell_line_names_all(CVCL_E548)
#' cell_line_names_synonym(CVCL_E548)
#' cell_line_names_identifier(CVCL_E548)
#'
#' @export
cell_line_names_all <- function(cell_line) {
  xml2::xml_find_all(cell_line, "./name-list/name/text()")
}


#' \code{cell_line_names_identifier} finds the name entries from a
#' cell-line's name-list, filtered to only include \code{type="identifier"}.
#' We expect that to only return one element, but do not enforce it.
#'
#' @describeIn cell_line_names_all Find identifier name(s) for a cell-line
#' @export
cell_line_names_identifier <- function(cell_line) {
  xml2::xml_find_all(cell_line, "./name-list/name[@type='identifier']/text()")
}

#' \code{cell_line_names_synonym} finds the name entries from a cell-line's
#' name-list, filtered to only include \code{type="synonym"}.
#'
#' @describeIn cell_line_names_all Find synonym names for a cell-line
#' @export
cell_line_names_synonym <- function(cell_line) {
  xml2::xml_find_all(cell_line, "./name-list/name[@type='synonym']/text()")
}

