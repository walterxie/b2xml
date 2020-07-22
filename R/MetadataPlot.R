#' @name Plot
#' @title Visualisations
#'
#' @description
#' Visualisations using \code{\link{ggplot2}}.
#' @keywords plot
#'
#' @details
#' \code{plotLocDate} plots samples through time coloured by traits (locations).
#'
#' @param meta.df data frame to contain samples, dates and locations.
#' @export
#' @examples
#' meta.df <- inner_join(dates.df, loc.df, by = "taxon")
#' plotLocDate(meta.df)
#'
#' @rdname Plot
plotLocDate <- function(meta.df, date.col="date", trait.col="trait",
                        x.lab="Date", y.lab="Number of samples", legend="locations") {
  require(scales)
  require(ggplot2)
  if (!all(is.na(as.Date(as.character(meta.df[date.col]),format="%d/%m/%Y"))))
    stop("Column ", date.col, " requires date, such as yyyy-mm-dd !")
  # count by region coloured by month
  ggplot(meta.df, aes_string(x = date.col, fill = trait.col,
                                  group = paste0("interaction(",date.col,",",trait.col,")"))) +
    geom_bar() + scale_x_date(labels = date_format("%m-%Y")) +
    ggtitle(paste(nrow(meta),"samples from", length(unique(meta[trait.col])), legend,
                  "between", date_format("%d-%m-%Y")(min(meta.df[date.col])),
                  "and", date_format("%d-%m-%Y")(max(meta.df[date.col])))) +
    xlab(x.lab) + ylab(y.lab) + labs(fill=legend) +
    theme_minimal()
}




