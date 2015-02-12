#'
#'Plot incidence time series using histograms
#'
#'This function provides advanced plotting facilities for incidence time series using ggplot2
#' @param x a data.frame containing the data to be plotted.
#' @param
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#' @export
#' @examples
#'
#' data("toy")
#' head(toy)
#'
#' ## basic plot
#' plotIncidence(toy, dates="dateOfOnset")
#'
#' ## change column indication, and bin size to 2 days
#' plotIncidence(toy, dates=1, bin=2)
#' plotIncidence(toy, dates=1, bin=2, split.by="gender", fill.by=3)
#'
#' @import ggplot2 scales
#'

##################
## mapIncidence ##
##################
mapIncidence <- function(x, dates, bin=7, fill.by=NULL,
                          start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence",
                          date.format="%d %b %Y", angle=45, xbreaks="1 week",
                          col.pal=1) {

    ## HANDLE ARGUMENTS ##
    if(is.numeric(dates)) dates <- names(x)[dates]
    if(!is.null(fill.by) && is.numeric(fill.by)) fill.by <- names(x)[fill.by]
    if(!is.null(col.pal) && (col.pal<0 || col.pal>8)) {
        col.pal <- NULL
        warning("col.pal must be an integer from 1 to 8 - setting col.pal=NULL")
    }

    ## GET TIME SERIES MATERIAL ##
    ## FIND OUT THE RIGHT BREAKS
    x.dates <- as.Date(x[,dates])

    ## get range of plotted info
    min.date <- if(is.null(start.at)){
        min.date <- min(x.dates,na.rm=TRUE)
    } else {
        min.date <- as.Date(start.at)
    }
    max.date <- if(is.null(stop.at)){
        max.date <- max(x.dates,na.rm=TRUE)
    } else {
        max.date <- as.Date(stop.at)
    }
    date.range <- c(min.date,max.date)

    ## find breaks
    ts.length <- as.integer(ceiling(diff(date.range)/bin)+1)
    dates.breaks <- seq(from=date.range[1], length=ts.length, by=bin)

    ## ANNOTATIONS
    date.annot <- scale_x_date(limits=date.range, breaks = xbreaks,
                               labels=date_format(date.format))
    date.rota <- theme(axis.text.x = element_text(angle = angle, vjust = .5))
    xy.labs <- labs(x=xlab,y=ylab)


    ## GET MAP MATERIAL ##
    ## fectch map

    ## compute cumulative incidence



    ## GENERATE THE PLOT ##

    ## RETURN OUTPUT ##
    return(out)
} # end mapIncidence

