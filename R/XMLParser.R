# parse BEAST 2 XML into tibble
# created by Walter Xie
# Accessed on 2 Jun 2020

#' @name XMLParser
#' @title parse BEAST 2 XML
#'
#' @description
#' The functions to parse BEAST 2 XML using \code{\link{xml2}}.
#' The getter will return \code{nodeset} defined in \code{\link{xml2}}.
#' @keywords xml
#'
#' @details
#' \code{getTags} is a generic function to get all node sets under xpath.
#' @param xml xml object from \code{\link{read_xml}}.
#' @param xpath A string containing a xpath (1.0) expression.
#' @param ... Other arguments passed to \code{\link{xml_find_all}}.
#'
#' @keywords xml
#' @export
#' @examples
#' getTags(xml , xpath="//data|//alignment")
#'
#' @rdname XMLParser
getTags <- function(xml, xpath="", ...) {
  require(tidyverse)
  require(xml2)
  xml_find_all(xml, xpath, ...)
}

#' @details
#' \code{filterByAtrr} is a generic function to filter a node set by attributes.
#' return original input, if the attribute is not defined in the XML.
#' @param nodeset the result from getters using \code{\link{xml_find_all}}.
#' @param attr xml object attribute.
#' @param value the value of the xml object attribute.
#' @export
#' @examples
#' param <- getTags(xml, "//parameter")
#' param %>% filterByAtrr(param, attr="id", value="strict.clock")
#'
#' @rdname XMLParser
filterByAtrr <- function(nodeset, attr="id", value="mcmc") {
  attr <- xml_attr(nodeset, attr)
  nodeset[!is.na(attr) & attr==value]
}

#' @details
#' \code{getTagByID} is \code{getTags} + \code{filterByAtrr},
#' which can be used to get a node set by its XML's id.
#' @param id the \code{id} attribute value of the xml object.
#' @examples
#' bPopSizes <- getTagByID(xml, xpath="//parameter|//stateNode", id="bPopSizes")
#' xml_attr(bPopSizes, "dimension") <- "20"
#'
getTagByID <- function(xml, xpath="", id="", ...) {
  require(tidyverse)
  require(xml2)
  tags <- getTags(xml, xpath, ...)
  filterByAtrr(tags, attr="id", value=id)
}


#' @details
#' \code{getSeqsDF} converts BEAST XML \code{<sequence>} into a \code{\link{tibble}}.
#'
#' @export
#' @examples
#' require(xml2)
#' xml <- read_xml(file.path("beast2.xml"))
#' seqs.df <- getSeqsDF(xml)
#'
#' @rdname XMLParser
getSeqsDF <- function(xml, xpath="//sequence") {
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
#' \code{getSeqs} gets all \code{<sequence>}.
#' @export
#' @examples
#' seqs <- getSeqs(xml)
#' @rdname XMLParser
getSeqs <- function(xml) {
  seqs <- getTags(xml, xpath="//sequence")
  cat("Load ", length(seqs), " squences from BEAST XML.")
  return(seqs)
}

#' @details
#' \code{getTaxa} pulls out the taxa name, and is used aftrer \code{getSeqs}.
#'
#' @param seqs the result from \code{getSeqs}.
#' @export
#' @examples
#' taxa.names <- getTaxa(seqs)
#'
#' @rdname XMLParser
getTaxa <- function(seqs, attr="taxon") {
  # taxa names
  xml_attr(seqs, attr)
}

#' @details
#' \code{getParameters} gets all <parameter> including <stateNode>.
#' @export
#' @rdname XMLParser
getParameters <- function(xml) {
  getTags(xml, xpath="//parameter|//stateNode")
}

#' @details
#'\code{getParamByID} is replaced by \code{getTagByID}.
#' @export
#' @rdname XMLParser
getParamByID <- function(xml, xpath="//parameter|//stateNode", id="") {
  getTagByID(xml, xpath=xpath, id=id)
}


#' @details
#' \code{getSpec} get all node sets containing spec="???".
#'
#' @param spec The class is specified in BEAST XML.
#' @export
#' @examples
#' getSpec(xml , xpath="//data|//alignment", spec="FilteredAlignment")
#' getFilteredAlignment(xml)
#'
#' @rdname XMLParser
getSpec <- function(xml, xpath="", spec="") {
  nodeset <- getTags(xml, xpath)
  filterByAtrr(nodeset, attr="spec", value=spec)
}

#' @details
#' \code{getFilteredAlignment} gets all \code{spec="FilteredAlignment"}
#' from <data> and <alignment>.
#' @export
#' @rdname XMLParser
getFilteredAlignment <- function(xml) {
  getSpec(xml , xpath="//data|//alignment", spec="FilteredAlignment")
}

