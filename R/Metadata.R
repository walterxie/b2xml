# analyse metadata in taxa names
# created by Walter Xie
# Accessed on 3 Jun 2020

#' @name Metadata
#' @title Metadata
#'
#' @description
#' Analyse the metadata extracted from taxa names in the BEAST 2 XML.
#' The input is \code{\link{tibble}}.
#' @keywords metadata
#'
#' @details
#' \code{extractMeta} extracts meta data from taxa names into a \code{\link{tibble}}.
#' For example,
#' \code{<sequence id="seq1" spec="Sequence" taxon="seq1|2020-04-25|New_Zealand|Auckland"
#' totalcount="4" value="acgt"/>}.
#'
#' @param seqs.df The \code{\link{tibble}} returned by \code{\link{getSeqsDF}}.
#' @param attr.taxon Attribute for a taxon name in BEAST 2 XML.
#' @param sep Separator between columns, see \code{\link{separate}}.
#' @param cols Column names after splitting \code{taxon} into multiple columns.
#' @param ... Other arguments passed to \code{\link{separate}}.
#' @export
#' @examples
#' seqs.df <- getSeqsDF(xml)
#' # seq1|2020-04-25|New_Zealand|Auckland
#' meta <- extractMeta(seqs.df, sep="\\|", cols = c("id","date","country","city"))
#' meta <- meta %>% mutate(month=gsub("(\\d+)-(\\d+)-.*","\\1-\\2",date))
#' print(meta, n=Inf)
#' write_delim(meta, "meta.tsv", delim = "\t")
#'
#' @rdname Metadata
extractMeta <- function(seqs.df, attr.taxon="taxon", sep="\\|",
                        cols = c("id","date","country","city"), ...) {
  require(tidyverse)
  # need manually update regarding to dataset
  meta <- seqs.df %>% select(!!attr.taxon) %>%
    separate(!!attr.taxon, cols, sep=sep, remove = F, ...)
  #%>% mutate(month=gsub("(\\d+)-(\\d+)-.*","\\1-\\2",date))
  if ("date" %in% colnames(meta))
    meta <- meta %>% mutate(date = as.Date(date))
  return(meta)
}




