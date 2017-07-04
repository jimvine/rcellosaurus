






# Does not support AND type filters. Only OR. But that's fine - just string
# together. Filter on one condition. Then filter on the other. Should subset
# to only those cases where both predicates are true.

# Searches are case-sensitive

cell_lines_filter <- function(cell_lines,
                             filter_by,
                             filter_term,
                             filter_type = c("equals",
                                             "contains",
                                             "starts-with"),
                             filter_not = FALSE) {

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


  # } else if (filter_by == "sex") {
  #   xp_start <- "self::*[@sex='"
  #   xp_sep <- "' or @sex='"
  #   xp_end <- "']"

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
