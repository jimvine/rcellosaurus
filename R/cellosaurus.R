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


# =========== Details of cell-lines ===========



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



#' Template function for extracting lists of items related to cell lines
#'
#' Internal function, used by functions that gather things that are stored in
#' the XML as lists. Each of these has a particular list container element,
#' e.g. \code{name-list}, and an object element \code{name}.
#'
#' Some (but not all) of the objects support categorisation, e.g. \code{name}s
#' can be of type \code{"identifier"} or type \code{"synonym"}. Where that is
#' the case, this function takes both an argument with the category(ies) being
#' filtered on (\code{attrib}) and the name of the categorisation
#' (\code{attrib_name}), which is used to form the XPath search term.
#'
#' The parameter \code{contents} defines whether to return the contents of the
#' nodes found (using the XPath code \code{/text()}). In cases where the
#' relevant information is stored as an attribute of the node rather than as
#' text within it (e.g. references), we need to return the whole of the nodes,
#' so specify \code{contents = FALSE}.
#'
#' @param cell_line
#'   A node or nodeset referring to a cell-line or multiple cell-lines.
#' @param attrib
#'   A character vector specifying which Types to output. If \code{NULL},
#'   the function returns all values (i.e., unfiltered).
#' @param list_element
#'   The XML element name for the list item
#' @param item_element
#'   The XML element name for the objects within the list
#' @param attrib_name
#'   The XML attribute name for the categorisation of list items
#' @param contents
#'   Whether to report the contents of the list items rather than returning
#'   the whole of each XML node. See Details.
#'
cell_line_lists <- function(cell_line,
                            attrib = NULL,
                            list_element,
                            item_element,
                            attrib_name,
                            contents = TRUE) {

  xpath <- paste0("./", list_element, "/", item_element)

  if(!is.null(attrib)) {
    xpath <- paste0(xpath,
                    "[@", attrib_name, "='",
                    paste(attrib,
                          collapse = paste0("' or @", attrib_name, "='")),
                    "']")
  }

  if(contents == TRUE) {
    xpath <- paste0(xpath, "/text()")
  }

  xml2::xml_find_all(cell_line, xpath)

}

#' Find accessions from cell-lines
#'
#' Expected values for \code{type} are \code{c("primary", "secondary")}.
#'
#' @param cell_line
#'   A node or nodeset referring to a cell-line or multiple cell-lines.
#' @param type
#'   A character vector specifying which types to output. If \code{NULL},
#'   the function returns all values (i.e., unfiltered).
#'
#' @export
cell_line_accessions <- function(cell_line, type = NULL) {

  cell_line_lists(cell_line,
                  attrib = type,
                  list_element = "accession-list",
                  item_element = "accession",
                  attrib_name = "type")

}



#' Find names from cell-lines
#'
#' Expected values for \code{type} are \code{c("identifier", "synonym")}.
#'
#' @inheritParams cell_line_accessions
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

  cell_line_lists(cell_line,
                  attrib = type,
                  list_element = "name-list",
                  item_element = "name",
                  attrib_name = "type")


}

#' Find comments from cell-lines
#'
#' Expected values for \code{category} are
#' \itemize{
#'   \item \code{Anecdotal},
#'   \item \code{Biotechnology},
#'   \item \code{Breed/subspecies},
#'   \item \code{Caution},
#'   \item \code{Characteristics},
#'   \item \code{Derived from metastatic site},
#'   \item \code{Derived from sampling site},
#'   \item \code{Discontinued},
#'   \item \code{Doubling time},
#'   \item \code{From},
#'   \item \code{Group},
#'   \item \code{Knockout cell},
#'   \item \code{Microsatellite instability},
#'   \item \code{Miscellaneous},
#'   \item \code{Misspelling},
#'   \item \code{Monoclonal antibody target},
#'   \item \code{Omics},
#'   \item \code{Part of},
#'   \item \code{Population},
#'   \item \code{Problematic cell line},
#'   \item \code{Registration},
#'   \item \code{Selected for resistance to},
#'   \item \code{Sequence variation},
#'   \item \code{Transfected with},
#'   \item \code{Transformant}
#' }
#'
#' @param category
#'   A character vector specifying which categories to output.
#' @inheritParams cell_line_accessions
#'
#' @return The comment(s) associated with the cell-lines.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' mice_lines <- cell_line_find_all(cellosaurus, "Mus musculus")
#' cell_line_comments(mice_lines, category = "Anecdotal")
#'
#' @export
cell_line_comments <- function(cell_line, category = NULL) {

  cell_line_lists(cell_line,
                  attrib = category,
                  list_element = "comment-list",
                  item_element = "comment",
                  attrib_name = "category")

}



#' Find web pages from cell-lines
#'
#' @inheritParams cell_line_accessions
#'
#' @return The web page(s) associated with the cell-lines.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' a_line <- cell_line_find_first(cellosaurus, "CVCL_E548")
#' cell_line_webpages(a_line)
#'
#' @export
cell_line_webpages <- function(cell_line) {

  cell_line_lists(cell_line,
                  attrib = NULL,
                  list_element = "web-page-list",
                  item_element = "url",
                  attrib_name = NULL)

}

#' Find references from cell-lines
#'
#' @inheritParams cell_line_accessions
#'
#' @return
#'   The resource-internal-ref(s) associated with the cell-lines. These
#'   relate to the publication-list also stored in the Cellosaurus XML data.
#'
#' @examples
#' cellosaurus <- read_cellosaurus_xml("data/cellosaurus.xml")
#' a_line <- cell_line_find_first(cellosaurus, "CVCL_E548")
#' cell_line_webpages(a_line)
#'
#' @export
cell_line_references <- function(cell_line) {

  refs <- cell_line_lists(cell_line,
                          attrib = NULL,
                          list_element = "reference-list",
                          item_element = "reference",
                          attrib_name = NULL,
                          contents = FALSE)

  xml2::xml_attr(refs, "resource-internal-ref")

}



