#' @name Trait
#' @title Trait analyser
#'
#' @description
#' Analyse the trait extracted from BEAST 2 XML.
#' @keywords trait
#'
#' @details
#' \code{getDates} splits value of date trait in XML into a \code{\link{tibble}}.
#'
#' @param nodeset the \code{\link{xml_nodeset}} containing date trait.
#' @param attr.date attribute name of date trait in BEAST 2 XML.
#' @param sep1,sep2 separator to split dates.
#' @param cols column names in returned \code{\link{tibble}}.
#' @export
#' @examples
#' trait.date <- getTags(xml, xpath="//trait")
#' dates.df <- getDatesDF(trait.date)
#' # if date is 2020-03-31
#' dates.df$date <- as.Date(dates.df$date, format="%Y-%m-%d")
#' # check missing dates
#' dates.df[is.na(dates.df$date),]
#'
#' @rdname Trait
getDatesDF <- function(nodeset, attr.date="value", sep1=",", sep2="=", cols = c("taxon","date")) {
  stopifnot(class(nodeset)=="xml_nodeset" && length(nodeset) == 1)

  attr <- xml_attr(nodeset, attr.date) %>% str_split(sep1) %>% unlist %>%
    gsub('\\s+|\\n', '', .) %>% as_tibble %>%
    separate(value, cols, sep=sep2)
}

#' @details
#' \code{addDecimalDate} adds a column in the numeric date (year is integer) into data.frame,
#' given the date column in the format "yyyy-mm-dd".
#'
#' @export
#' @examples
#' dates.df <- addDecimalDate(dates.df, date.format="%Y-%m-%d")
#' # tibble does not show decimals
#' dates.df[1:5,] %>% as.data.frame
#'
#' @rdname Trait
addDecimalDate <- function(dates.df, cols = c("taxon","date"), date.format="%Y-%m-%d") {
  stopifnot( all(cols %in% names(dates.df)) )
  require("lubridate")
  dates.df <- dates.df %>% mutate(date=as.Date(date, format=!!date.format)) %>%
    mutate(date.num=decimal_date(ymd(date)))
}

#' @examples
#' trait.loc <- getTags(xml , xpath="//traitSet")
#'
#'
#'





