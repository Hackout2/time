#'
#'Plot incidence time series using histograms
#'
#'This function provides advanced plotting facilities for incidence time series using ggplot2
#' @param x a data.frame containing the data to be plotted.
#' @param dates a character string or an integer indicating which column contains the dates to use
#' @param lon a character string or an integer indicating which column contains longitudes ('x axis')
#' @param lat a character string or an integer indicating which column contains latitudes ('y axis')
#' @param bin an integer indicating the size of the time window (in days) to use for the incidence computation
#' @param fill.by a character string or an integer indicating which column to use to color the bars
#' @param source a character string indicating the type of maps to be used
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
#'
#'
#' @import ggplot2 scales ggmap gridExtra OutbreakTools
#'

##################
## mapIncidence ##
##################
mapIncidence <- function(x, dates, lon, lat, bin=7, fill.by=NULL, source="osm",
                          start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence",
                          date.format="%d %b %Y", angle=45, xbreaks="1 week",
                          col.pal=1) {

    ## HANDLE ARGUMENTS ##
    if(is.numeric(dates)) dates <- names(x)[dates]
    lon <- x[,lon,drop=TRUE]
    lat <- x[,lat,drop=TRUE]
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

    ## get maximum incidence
    incid <- get.incidence(x[,dates], from=start.at, to=stop.at, interval=bin)
    max.incid <- max(incid, na.rm=TRUE)

    ## annotations
    date.annot <- scale_x_date(limits=date.range, breaks = xbreaks,
                               labels=date_format(date.format))
    date.rota <- theme(axis.text.x = element_text(angle = angle, vjust = .5))
    xy.labs <- labs(x=xlab,y=ylab)


    ## GET MAP MATERIAL ##
    ## get bounding box
    bound.box <- with(x, c(min(lon,na.rm=TRUE),
                            min(lat,na.rm=TRUE),
                            max(lon,na.rm=TRUE),
                            max(lat,na.rm=TRUE)))

    ## fectch map
    baseMap <- ggmap(get_map(bound.box + c(-1,-1,1,1), source=source))

    ## compute cumulative incidence
    xyn <- na.omit(data.frame(xyTable(data.frame(lon,lat))))
    names(xyn)[3] <- "Incidence"
    head(xyn)

    ## get breaks for cum. incidence, force 1 to be in the scale
    breaks <- pretty(1:max(xyn,na.rm=TRUE), n=6)
    breaks <- breaks[breaks>1] # remove potential 0/1
    breaks <- c(1,breaks)

    ## GENERATE THE PLOT ##

    ## RETURN OUTPUT ##
    return(out)
} # end mapIncidence

