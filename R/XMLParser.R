# parse BEAST 2 XML into tibble
# created by Walter Xie
# Accessed on 2 Jun 2020

#' @name XMLParser
#' @title parse BEAST 2 XML
#'
#' @description
#' The functions to parse BEAST 2 XML using \code{\link{xml2}}.
#' The getter will return \code{nodeset} defined in \code{\link{xml2}}.
#'
#' @details
#' \code{getSeqs} convert BEAST XML \code{<sequence>} into a \code{\link{tibble}}.
#'
#' @param xml xml object from \code{\link{read_xml}}.
#' @param xpath A string containing a xpath (1.0) expression.
#' @keywords xml
#' @export
#' @examples
#' xml <- read_xml(file.path("beast2.xml"))
#' seqs <- getSeqs(xml)
#'
#' @rdname XMLParser
getSeqs <- function(xml, xpath="//sequence") {
  require(tidyverse)
  require(xml2)
  if ( !grepl(xml_name(xml), xpath) )
    xml <- xml_find_all(xml, xpath) # "//sequence"
  cat("Load ", length(xml), " squences from BEAST XML.")

  l <- xml_attrs(xml)
  seqs <- NULL
  for (seq in l) {
    seqs <- seqs %>% bind_rows(seq)
  }
  stopifnot(nrow(seqs) == length(l))
  return(seqs)
}

#' @details
#' \code{getTag} get all node sets under xpath.
#'
#' @param ... Other arguments passed to \code{\link{xml_find_all}}.
#' @export
#' @examples
#' getTag(xml , xpath="//data|//alignment")
#'
#' @rdname XMLParser
getTag(xml, xpath="", ...) {
  require(tidyverse)
  require(xml2)
  stopifnot( grepl(xml_name(xml), xpath) )
  xml_find_all(xml, xpath, ...)
}

#' @details
#' \code{getSpec} get all node sets containing spec="???".
#'
#' @param spec The class specified in XML.
#' @export
#' @examples
#' getSpec(xml , xpath="//data|//alignment", spec="FilteredAlignment")
#'
#' @rdname XMLParser
getSpec(xml, xpath="", spec="") {
  xml <- getTag(xml, xpath)
  xml[!is.na(xml_attr(xml, "spec")) & xml_attr(xml, "spec")==spec]
}

#' @details
#' \code{getFilteredAlignment} get all \code{spec="FilteredAlignment"}
#' from <data> and <alignment>.
#' @export
#' @rdname XMLParser
getFilteredAlignment <- function(xml) {
  getSpec(xml , xpath="//data|//alignment", spec="FilteredAlignment")
}

#' @details
#' \code{getParameters} get all <parameter> including <stateNode>.
#' @export
#' @rdname XMLParser
getParameters <- function(xml) {
  getTag(xml , xpath="//parameter|//stateNode")
}

#' @details
#' Filter a node set by \code{id}.
#' @export
#' @rdname XMLParser
#' @examples
#' param <- getParameters(xml)
#' bPopSizes <- getParamByID(param, id="bPopSizes")
#' xml_attr(bPopSizes, "dimension") <- "20"
#'
#' @rdname XMLParser
getParamByID <- function(nodeset, id="") {
  nodeset[!is.na(xml_attr(nodeset, "id")) & xml_attr(nodeset, "id")==id]
}

