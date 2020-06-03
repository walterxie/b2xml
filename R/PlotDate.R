# ggplot
# created by Walter Xie
# Accessed on 3 Jun 2020

#' @name Plot
#' @title Plot
#'
#' @description
#' Visulizations using \code{\link{ggplot2}}. The input is \code{\link{tibble}}.
#' @keywords plot
#'
#' @details
#' \code{barLocDate} plots a bar chart to count the number of samples
#' through the sampling date, and coloured by locations.
#'
#' @param meta The \code{\link{tibble}} returned by \code{\link{extractMeta}}.
#' @param x,fill Data column for x axis and colour.
#' @param x.lab.format x labels, default to month.
#' @param title,xlab,ylab title, x and y axis labels.
#' @param ... Other arguments passed to \code{\link{geom_bar}}.
#' @export
#' @examples
#' meta <- extractMeta(seqs.df, sep="\\|", cols = c("id","date","country","city"))
#' # need both sampling date and location
#' p <- barLocDate(meta, fill = "country")
#' title=paste(nrow(meta),"samples from", length(unique(meta[["region"]])),
#'       "regions from between", date_format("%d-%m-%Y")(min(meta[["date"]])),
#'       "and", date_format("%d-%m-%Y")(max(meta[["date"]])))
#'
#' @rdname Plot
barLocDate <- function(meta, x = "date", fill = "region", x.lab.format = date_format("%m-%Y"),
                        title="", xlab="Date", ylab="Number of samples", ...) {
  require(scales)
  require(ggplot2)
  # count by region coloured by month
  p <- ggplot(meta, aes_string(x = x, fill = fill,
                               group = paste0("interaction(", x, ",", fill,")"))) +
    geom_bar(...) + scale_x_date(labels = x.lab.format) +
    ggtitle(title) + xlab(xlab) + ylab(ylab) +
    theme_minimal()
}




