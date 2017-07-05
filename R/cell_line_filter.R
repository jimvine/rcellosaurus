# Cellosaurus functions for R.
# Function for filtering cell-lines based on criteria
# Copyright Jim Vine


#' Filter cell-lines based on criteria
#'
#' An XML nodeset of cell-line elements can be filtered to produce a subset
#' containing only those elements that match the filter criteria.
#'
#' The following options are available as \code{filter_by} terms:
#'
#' \itemize{
#'   \item \code{accession}
#'   \item \code{accession-primary} (as \code{accession}, but only matching
#'     primary accession codes),
#'   \item \code{accession-secondary} (as \code{accession}, but only matching
#'     secondary accession codes),
#'   \item \code{name}
#'   \item \code{name-identifier} (as \code{name}, but only matching identifier
#'     (i.e., primary) names),
#'   \item \code{name-synonym} (as \code{name}, but only matching synonym
#'     (i.e., secondary) names),
#'   \item \code{category}
#'   \item \code{sex}
#'   \item \code{species}
#'   \item \code{species-accession}
#'   \item \code{comment}
#'   \item \code{comment-category}
#'   \item \code{same-origin-as} (Names of cell-lines that this cell-line
#'     has the same origin as),
#'   \item \code{same-origin-as-accession} (Accession codes of cell-lines that
#'     this cell-line has the same origin as),
#'   \item \code{derived-from} (Name of a cell-line that this cell-line is
#'     derived from),
#'   \item \code{derived-from-accession} (Accession code of a cell-line that
#'     this cell-line is derived from),
#'   \item \code{disease} (Disease by name),
#'   \item \code{disease-accession} (Disease by accession code).
#' }
#'
#' For full details of the meanings of these values, see the Cellosaurus
#' documentation.
#'
#' Because \code{filter_term}s are applied as a logical OR, filtering to retain
#' cell-line elements that match any of the terms, filtering to match two
#' (or more) criteria is achieved using two (or more) calls to this function.
#'
#' @param cell_lines
#'   An XML nodeset containing cell-line elements. Typically either all the
#'   cell-lines in the Cellosaurus dataset, obtained using
#'   \code{\link{cell_lines_all}}, or an existing subset returned by a
#'   previous filtering.
#'
#' @param filter_by
#'   A string specifying which characteristic the filter should be applied to.
#'   Valid options are listed in Details.
#'
#' @param filter_term
#'   A string or character vector specifying the term(s) to filter on. If
#'   multiple values are provided, the filter will be applied as an "OR",
#'   i.e., retaining cell-lines that match any of the items in
#'   \code{filter_term}. The terms are case-sensitive.
#'
#' @param filter_type
#'   Options are \code{c("equals", "contains","starts-with")}.
#'
#' @return
#'   An XML nodeset containing the subset of the cell-lines that match the
#'   filter criteria.
#'
#' @examples
#' # Set up the data
#' cellosaurus <- read_cellosaurus_xml(system.file("extdata",
#'                                                 "cellosaurus.xml",
#'                                                 package = "rcellosaurus"))
#' cell_lines <- cell_lines_all(cellosaurus)
#'
#' # Filter for a particular accession number:
#' cell_lines_filter(cell_lines,
#'                   filter_by = "accession",
#'                   filter_term = "CVCL_E525",
#'                   filter_type = "equals")
#'
#' # Filter for all cell-lines matching any of three sex categories:
#' cell_lines_filter(cell_lines,
#'                   filter_by = "sex",
#'                   filter_term = c("Mixed sex",
#'                                   "Sex ambiguous",
#'                                   "Sex undetermined"),
#'                   filter_type = "equals")
#'
#' # Sequential filtering to match "disease contains dysplasia" AND
#' # "category contains stem cell":
#' dysplasia_lines <- cell_lines_filter(cell_lines,
#'                                      filter_by = "disease",
#'                                      filter_term = "dysplasia",
#'                                      filter_type = "contains")
#' dysplasia_stem_lines <- cell_lines_filter(dysplasia_lines,
#'                                           filter_by = "category",
#'                                           filter_term = "stem cell",
#'                                           filter_type = "contains")
#'
#' @export
cell_lines_filter <- function(cell_lines,
                             filter_by,
                             filter_term,
                             filter_type = c("equals",
                                             "contains",
                                             "starts-with")) {

  xp_precase_text <- function(filter_type) {
    if(filter_type == "equals") {
      return("text()='")
    } else if(filter_type == "contains") {
      return("text()[contains(.,'")
    } else if(filter_type == "starts-with") {
      return("text()[starts-with(.,'")
    }
  }

  xp_postcase_text <- function(filter_type) {
    if(filter_type == "equals") {
      return("'")
    } else if(filter_type == "contains") {
      return("')]")
    } else if(filter_type == "starts-with") {
      return("')]")
    }
  }

  xp_precase_attr <- function(filter_type, attrib_name) {
    if(filter_type == "equals") {
      return(paste0("@", attrib_name, "='"))
    } else if(filter_type == "contains") {
      return(paste0("@", attrib_name, "[contains(.,'"))
    } else if(filter_type == "starts-with") {
      return(paste0("@", attrib_name, "[starts-with(.,'"))
    }
  }

  xp_postcase_attr <- function(filter_type) {
    if(filter_type == "equals") {
      return("'")
    } else if(filter_type == "contains") {
      return("')]")
    } else if(filter_type == "starts-with") {
      return("')]")
    }
  }

  if(filter_by == "accession") {
    xp_start    <- "accession-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if(filter_by == "accession-primary") {
    xp_start    <- "accession-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- " and @type='primary']/ancestor::cell-line"

  } else if(filter_by == "accession-secondary") {
    xp_start    <- "accession-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- " and @type='secondary']/ancestor::cell-line"

  } else if (filter_by == "name") {
    xp_start    <- "name-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "name-identifier") {
    xp_start    <- "name-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- " and @type='identifier']/ancestor::cell-line"

  } else if (filter_by == "name-synonym") {
    xp_start    <- "name-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- " and @type='synonym']/ancestor::cell-line"

  } else if (filter_by == "category" |
             filter_by == "sex" ) {
    xp_start    <- "self::*["
    xp_precase  <- xp_precase_attr(filter_type, filter_by)
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]"

  } else if (filter_by == "species") {
    xp_start    <- "species-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "species-accession") {
    xp_start    <- "species-list/*["
    xp_precase  <- xp_precase_attr(filter_type, "accession")
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "comment") {
    xp_start    <- "comment-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "comment-category") {
    xp_start    <- "comment-list/*["
    xp_precase  <- xp_precase_attr(filter_type, "category")
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "same-origin-as") {
    xp_start    <- "same-origin-as/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "same-origin-as-accession") {
    xp_start    <- "same-origin-as/*["
    xp_precase  <- xp_precase_attr(filter_type, "accession")
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "derived-from") {
    xp_start    <- "derived-from/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "derived-from-accession") {
    xp_start    <- "derived-from/*["
    xp_precase  <- xp_precase_attr(filter_type, "accession")
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "disease") {
    xp_start    <- "disease-list/*["
    xp_precase  <- xp_precase_text(filter_type)
    xp_postcase <- xp_postcase_text(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  } else if (filter_by == "disease-accession") {
    xp_start    <- "disease-list/*["
    xp_precase  <- xp_precase_attr(filter_type, "accession")
    xp_postcase <- xp_postcase_attr(filter_type)
    xp_end      <- "]/ancestor::cell-line"

  }

  xpath <- paste0(xp_start,
                  xp_precase,
                  paste(filter_term, collapse = paste0(xp_postcase,
                                                       " or ",
                                                       xp_precase)),
                  xp_postcase,
                  xp_end)

  xml2::xml_find_all(cell_lines, xpath)

}
