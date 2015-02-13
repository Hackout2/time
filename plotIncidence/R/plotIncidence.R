#'
#'Plot incidence time series using histograms
#'
#'This function provides advanced plotting facilities for incidence time series using ggplot2
#' @param x a data.frame containing the data to be plotted.
#' @param dates a character string or an integer indicating which column contains the dates to use
#' @param bin an integer indicating the size of the time window (in days) to use for the incidence computation
#' @param fill.by a character string or an integer indicating which column to use to color the bars
#' @param split.by a character string or an integer indicating which column to use to split the charts
#' @param shade.by a character string or an integer indicating which column to use to shade the bars
#' @param start.at a starting date for the incidence; defaults to the earliest date of the dataset
#' @param stop.at a stoping date for the incidence; defaults to the latest date of the dataset
#' @param xlab a label for the x axis
#' @param ylab a label for the y axis
#' @param date.format a character string indicating the format of the dates
#' @param angle an integer indicating the angle of the dates
#' @param xbreaks a character string indicating the time interval between vertical lines
#' @param col.pal an integer between 1 and 8 indicating a color palette to be used; if NULL, the default color palette of ggplot2 is used
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

###################
## plotIncidence ##
###################
plotIncidence <- function(x, dates, bin=7, fill.by=NULL, split.by=NULL, shade.by=NULL,
                          start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence",
                          date.format="%d %b %Y", angle=45, xbreaks="1 week",
                          col.pal=1) {

    ## HANDLE ARGUMENTS ##
    if(is.numeric(dates)) dates <- names(x)[dates]
    if(!is.null(fill.by) && is.numeric(fill.by)) fill.by <- names(x)[fill.by]
    if(!is.null(split.by) && is.numeric(split.by)) split.by <- names(x)[split.by]
    if(!is.null(shade.by) && is.numeric(shade.by)) shade.by <- names(x)[shade.by]
    if(!is.null(col.pal) && col.pal==0) col.pal <- NULL # 0 will be the default palette
    if(!is.null(col.pal) && (col.pal<0 || col.pal>8)) {
        col.pal <- NULL
        warning("col.pal must be an integer from 1 to 8 - setting col.pal=NULL")
    }

    ## FIND OUT THE RIGHT BREAKS ##
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


    ## CUSTOMISATION OF THE PLOT ##
    ## ANNOTATIONS
    date.annot <- scale_x_date(limits=date.range, breaks = xbreaks,
                               labels=date_format(date.format))
    date.rota <- theme(axis.text.x = element_text(angle = angle, vjust = .5))
    xy.labs <- labs(x=xlab,y=ylab)

    ## GENERATE THE PLOT ##
    out <- ggplot(x) +
        geom_histogram(aes_string(x=dates, fill=fill.by, alpha=shade.by),
                       breaks=as.numeric(dates.breaks)) +
                           xy.labs + date.annot + date.rota

    if(!is.null(split.by)) out <- out + facet_grid(paste(split.by, ".", sep="~"))
    if(!is.null(col.pal)) out <- out + scale_fill_brewer(type="qual", palette=col.pal)

    ## RETURN OUTPUT ##
    return(out)
} # end plotIncidence

