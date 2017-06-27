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
#' two_species <- cell_line_find_all(cellosaurus, c("Mus musculus", "Cavia porcellus"))
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


#' Category of a cell-line
#'
#' @inheritParams cell_line_sex
#'
#' @return The category attribute associated with the cell-line.
#'
#' @examples
#' # First read the dataset in.
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#'
#' # Can call on a nodeset that's already been found. E.g.:
#' a_mouse_line <- cell_line_find_first(cellosaurus, "CVCL_IW91")
#' cell_line_category(a_mouse_line)
#'
#' # Works with nodesets containing multiple cell-lines:
#' mice_lines <- cell_line_find_all(cellosaurus, "Mus musculus")
#' cell_line_category(mice_lines)
#'
#' # Can nest the caetgory function around the the finder function:
#' cell_line_category(cell_line_find_first(cellosaurus, "CVCL_3449"))
#'
#' @export
#'
cell_line_category <- function(cell_line) {
  xml2::xml_attr(cell_line, "category")
}

#' Sex of a cell-line
#'
#' @param cell_line
#'   A node or nodeset referring to a cell-line.
#'
#' @return The sex attribute associated with the cell-line. \code{NA} if
#'   no sex attribute is present.
#'
#' @examples
#' # First read the dataset in.
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#'
#' # Can call on a nodeset that's already been found. E.g.:
#' a_line <- cell_line_find_first(cellosaurus, "CVCL_E548")
#' cell_line_sex(a_line)
#'
#' # Works with nodesets containing multiple cell-lines:
#' mice_lines <- cell_line_find_all(cellosaurus, "Mus musculus")
#' cell_line_sex(mice_lines)
#'
#' # Can nest the sex function around the the finder function:
#' cell_line_sex(cell_line_find_first(cellosaurus, "CVCL_3449"))
#'
#' @export
cell_line_sex <- function(cell_line) {
  xml2::xml_attr(cell_line, "sex")
}



#' Find names from a cell-line
#'
#' @param cell_line
#'   A node or nodeset referring to a cell-line.
#' @param type
#'   A character vector specifying which types of names to output.
#'   Expected values are \code{c("identifier", "synonym")}. If \code{NULL},
#'   the function returns all names (i.e., unfiltered).
#'
#' @return The name(s) associated with the cell-line.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' a_line <- cell_line_find_first(cellosaurus, "CVCL_E548")
#'
#' # If type is not set, returns all names
#' cell_line_names(a_line)
#'
#' # The expected values of type are "identifier" or "synonym":
#' cell_line_names(a_line, type = "identifier")
#' cell_line_names(a_line, type = "synonym")
#'
#' # Can provide multiple type values as a character vector.
#' # This should be the same as the unfiltered version:
#' cell_line_names(a_line, type = c("identifier", "synonym"))
#'
#' @export
cell_line_names <- function(cell_line, type = NULL) {
  if(is.null(type)) {
    xpath <- "./name-list/name/text()"
  } else if(length(type) == 1) {
    xpath <- paste0("./name-list/name[@type='", type, "']/text()")
  } else {
    xpath <- paste0("./name-list/name[@type='",
                    paste(type, collapse = "' or @type='"),
                    "']/text()")
  }
  xml2::xml_find_all(cell_line, xpath)
}
