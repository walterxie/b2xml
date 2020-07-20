#' @name XMLEditor
#' @title edit BEAST 2 XML
#'
#' @description
#' The functions to edit BEAST 2 XML using \code{\link{xml2}}.
#' @keywords xml
#'
#' @details
#' \code{replaceSeqs} replaces the old sequences in XML <data> as the given new sequences.
#'
#' @export
#' @examples
#' xml <- read_xml(file.path(DATA.XML))
#' SEQS <- xml_find_all(xml, "//sequence")
#' template <- read_xml(file.path(TMPL.XML))
#' # replace data in TMPL.XML by data in DATA.XML
#' replaceSeqs(template, SEQS)
#'
#' @rdname XMLEditor
replaceSeqs <- function(xml, node.seqs, xpath="//data") {
  require(tidyverse)
  require(xml2)
  node.data <- xml_find_first(xml, xpath)
  xml_text(node.data) = ""                  # clean xml text
  children <- node.data %>% xml_children() %>% xml_remove() # clean previous child nodes
  rm(children)
  # node.seqs are new xml nodes <data> to insert
  for (seq in node.seqs) {
    xml_add_child(node.data, seq)
  }
  rm(node.data)
}


#' @details
#' \code{replaceChainLength} replaces the chain length in MCMC.
#' @export
#' @rdname XMLEditor
replaceChainLength <- function(xml, value, attr="chainLength", xpath="//run") {
  run <- xml_find_all(xml, xpath)
  chain <- run[xml_has_attr(run, attr)]
  xml_attr(chain, attr) <- value
  rm(run,chain)
}


#' @details
#' \code{replaceStoreEvery} replaces the store frequency in MCMC.
#' @export
#' @rdname XMLEditor
replaceStoreEvery <- function(xml, value, attr="storeEvery", xpath="//state") {
  state <- xml_find_first(xml, xpath)
  xml_attr(state, attr) <- value
  rm(state)
}

#' @details
#' \code{replaceLogEvery} replaces the logging frequency in MCMC.
#' @param ids a vetor of id for logging.
#' @param values a vetor of values for these ids. which must have the same length.
#' @export
#' @rdname XMLEditor
replaceLogEvery <- function(xml, ids=c("tracelog","treelog","screenlog"),
                            values=c("10000","10000","1000000"),
                            attr="logEvery", xpath="//logger") {
  logger <- xml_find_all(template, "//logger")
  stopifnot(length(logger) < 1)
  stopifnot(length(ids) == length(values))

  for(i in 1:length(ids)) {
    tracelog <- logger[xml_attr(logger, "id")==ids[i]]
    xml_attr(tracelog, attr) <- values[i]
  }
  rm(tracelog,logger)
}









