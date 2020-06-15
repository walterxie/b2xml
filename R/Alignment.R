# analyse alignment
# created by Walter Xie
# Accessed on 3 Jun 2020

#' @name Alignment
#' @title Alignment analyser
#'
#' @description
#' Analyse the alignment extracted from BEAST 2 XML.
#' The input is \code{\link{tibble}}.
#' @keywords alignment
#'
#' @details
#' \code{countCharFreq} splits sequences and counts alphabet frequency.
#'
#' @param seqs.df the \code{\link{tibble}} returned by \code{\link{getSeqsDF}}.
#' @param attr.taxon,attr.seq attribute names required in BEAST 2 XML.
#' @param CASE.FUN the function (to use upper case as default).
#' @export
#' @examples
#' seqs.df <- getSeqsDF(xml)
#' freq <- countCharFreq(seqs.df, CASE.FUN=tolower)
#' print(freq, n=Inf)
#'
#' @rdname Alignment
countCharFreq <- function(seqs.df, attr.taxon="taxon", attr.seq="value", CASE.FUN=toupper) {
  #https://stackoverflow.com/questions/49646809/how-to-split-string-and-count-alphabet-frequency-using-dplyr-pipe
  require(tidyverse)
  freq <- seqs.df %>%
    mutate(split=map(!!as.name(attr.seq), ~unlist(str_split(CASE.FUN(.), "")))) %>% #split col "value" (sequences) into characters
    unnest(cols = c(split)) %>%                           #unnest into a new column
    group_by_at(vars(attr.taxon, attr.seq)) %>%             #group by col "taxon" and "value"
    count(split) %>%                       #count letters for each group
    spread(key=split, value=n, fill=0) %>% #convert to wide format
    ungroup()
  # colnames: taxon value `-` a c d g k n s t w y

  freq %>% select(-value) %>% select(!!attr.taxon, CASE.FUN(c("a", "c", "g", "t", "n")), everything())
}


#' @details
#' \code{writeFasta} write sequences to fasta format.
#' @param file the fasta file name.
#' @export
#' @examples
#' writeFasta(seqs.df, file="seqs.fasta")
#'
#' @rdname Alignment
writeFasta <- function(seqs.df, file="seqs.fasta", attr.taxon="taxon", attr.seq="value", CASE.FUN=toupper) {
  cat("",file=file)
  for(i in 1:nrow(seqs.df)) {
    cat(paste0("> ", seqs.df[i,attr.taxon], "\n"), file=file, append=TRUE)
    cat(paste0(seqs.df[i,attr.seq], "\n"), file=file, append=TRUE)
  }
}

