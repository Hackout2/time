#'
#'Plot incidence time series using histograms
#'
#'This function provides advanced plotting facilities for incidence time series using ggplot2
#' @param x a data.frame containing the data to be plotted.
#' @param dates a character string or an integer indicating which column contains the dates to use
#' @param lon a character string or an integer indicating which column contains longitudes ('x axis')
#' @param lat a character string or an integer indicating which column contains latitudes ('y axis')
#' @param fill.by a character string or an integer indicating which column to use to color the bars
#' @param col.pal an integer between 1 and 8 indicating a color palette to be used; if 0 or NULL, the default color palette of ggplot2 is used
#' @param alpha a numeric value between 0 and 1 handling transparency, with 0 being fulling transparent and 1 having no transparency
#' @param bin an integer indicating the size of the time window (in days) to use for the incidence computation
#' @param jitter a numeric value indicating the amount of random noise added to the locations of new cases
#' @param source a character string indicating the type of maps to be used
#' @param zoom an integer indicating the level of zoom to be used for the map, from 3 (continent) to 21 (building); see \link[ggmap]{get_map}
#' @param start.at a starting date for the incidence; defaults to the earliest date of the dataset
#' @param stop.at a stoping date for the incidence; defaults to the latest date of the dataset
#' @param xlab a label for the x axis
#' @param ylab a label for the y axis
#' @param date.format a character string indicating the format of the dates
#' @param angle an integer indicating the angle of the dates
#' @param xbreaks a character string indicating the time interval between vertical lines
#' @param heights a vector of two numbers summing to 1 indicating the relative heights of the map and the incidence time series
#' @param ani.width an integer indicating the width of the image, in pixels
#' @param ani.height an integer indicating the height of the image, in pixels; no-square images are not guaranteed to work
#' @param point.size an integer indicating the size of point to be used for incidence
#' @param annot.size an integer indicating the size of the text annotations
#' @param xy.annot a logical indicating whether latitudes and longitudes should be shown
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' @import ggplot2 scales ggmap gridExtra OutbreakTools animation
#'
#' @examples
#' \dontrun{
#' data(zombie_outbreak)
#' mapIncidence(head(zombie_outbreak,1000), 3,"x.coord","y.coord", fill.by="gender",zoom=11)
#' }
#'
#'

##################
## mapIncidence ##
##################
mapIncidence <- function(x, dates, lon, lat, fill.by=NULL, col.pal=0, alpha=.5, bin=7,
                         jitter=0.01, source="google", zoom=NULL,
                         start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence",
                         date.format="%d %b %Y", angle=90, xbreaks="1 week",
                         heights=c(0.75, 0.25), ani.width=800, ani.height=ani.width,
                         point.size=5, annot.size=20, xy.annot=FALSE) {

    ## HANDLE ARGUMENTS ##
    if(is.numeric(dates)) dates <- names(x)[dates]
    if(is.numeric(lon)) lon <- names(x)[lon]
    if(is.numeric(lat)) lat <- names(x)[lat]
    ##lonlat <- data.frame(lon,lat)
    if(!is.null(fill.by) && is.numeric(fill.by)) fill.by <- names(x)[fill.by]
    if(!is.null(col.pal) && col.pal==0) col.pal <- NULL # 0 will be the default palette
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

    ## subset data to match requested range
    toKeep <- x[,dates] >=min.date & x[,dates] <= max.date
    x <- x[toKeep,]

    ## find breaks
    ts.length <- as.integer(ceiling(diff(date.range)/bin)+1)
    dates.breaks <- seq(from=date.range[1]-1, length=ts.length, by=bin)

    ## get maximum incidence
    incid <- get.incidence(x[,dates], from=start.at, to=stop.at, interval=bin)
    max.incid <- max(incid$incidence, na.rm=TRUE)

    ## annotations
    date.annot <- scale_x_date(limits=date.range, breaks = xbreaks,
                               labels=date_format(date.format))
    date.rota <- theme(axis.text.x = element_text(angle = angle, vjust = .5))
    xy.labs <- labs(x=xlab,y=ylab)


    ## GET MAP MATERIAL ##
    ## get bounding box
    bound.box <- with(x, c(min(x[,lon],na.rm=TRUE),
                            min(x[,lat],na.rm=TRUE),
                            max(x[,lon],na.rm=TRUE),
                            max(x[,lat],na.rm=TRUE)))

    ## fectch map
    if(is.null(zoom)) {
        base.map <- ggmap(get_map(bound.box + c(-1,-1,1,1), source=source))
    } else {
        base.map <- ggmap(get_map(bound.box + c(-1,-1,1,1), source=source, zoom=zoom))
    }

    ## compute cumulative incidence
    xyn <- na.omit(data.frame(xyTable(x[,c(lon,lat)])))
    names(xyn)[3] <- "Incidence"

    ## get numbers for max number of cases

    map.max.size <- max(xyn[3],na.rm=TRUE)

    ## get breaks for cum. incidence, force 1 to be in the scale
    map.breaks <- pretty(1:map.max.size, n=6)
    map.breaks <- map.breaks[map.breaks>1] # remove potential 0/1
    map.breaks <- c(1,map.breaks)


    ## GENERATE THE MOVIE ##
    saveHTML({
        for(i in 2:length(dates.breaks)){
            ## TOP PANEL: MAP
            ## data for cumulative incidence
            toKeep <- which(x[,dates] <= dates.breaks[i-1])
            xyn.cum <- data.frame(xyTable(na.omit(x[toKeep,c(lon,lat)])))
            names(xyn.cum)[3] <- "Incidence"

            ## data for current incidence
            toKeep <- which(x[,dates] >= dates.breaks[i-1] &
                            x[,dates] < dates.breaks[i])

            p1 <- base.map +
                suppressWarnings(geom_point(data=xyn.cum, aes(x=x,y=y,size=Incidence),
                                            alpha=.5, col="black")) +
                                                geom_jitter(data=x[toKeep,], aes_string(x=lon,y=lat,colour=fill.by),
                                                            alpha=alpha, size=point.size,
                                                            position = position_jitter(h=jitter/2, w=jitter)) +
            scale_size_continuous("Cumulative \nincidence", range=c(2,15),
                                  limits=c(0,map.max.size), breaks=map.breaks) +
                                      theme_bw() + labs(x=NULL,y=NULL) +
                                      scale_colour_discrete(guide=FALSE) +
                                      theme(text = element_text(size=annot.size))

            if(!xy.annot) p1 <- p1 + theme(axis.text.x = element_blank(),
                                           axis.ticks.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           axis.ticks.y = element_blank())
            if(!is.null(col.pal)) suppressWarnings(p1 <- p1 + scale_colour_brewer(type="qual", palette=col.pal, guide=FALSE))

            ## BOTTOM PANEL: INCIDENCE TIME SERIES
            ## make incidence curve
            tempdat <- x[x[,dates,]<=dates.breaks[i],,drop=FALSE]
            p2 <- ggplot(tempdat) +
                geom_histogram(aes_string(x=dates, fill=fill.by),
                               breaks=as.numeric(dates.breaks)+0.01) +
                               geom_vline(xintercept = as.numeric(dates.breaks[i])) +
                               scale_y_continuous(limits=c(0, max.incid)) +
                               xy.labs + date.annot + date.rota +
                               scale_colour_discrete(drop = FALSE) +
                               theme(text = element_text(size=annot.size))

            if(!is.null(col.pal)) suppressWarnings(p2 <- p2 + scale_fill_brewer(type="qual", palette=col.pal, drop=FALSE))

            suppressWarnings(grid.arrange(arrangeGrob(p1,p2, heights=c(3/4, 1/4), ncol=1)))

        }}, ani.width=ani.width, ani.height=ani.height, verbose=FALSE)

    ## RETURN OUTPUT ##
    return(invisible(NULL))
} # end mapIncidence

