#' None package material experimental code for getting incidence curves.
#' 
#' @author Michael Höhle
#' 
library("dygraphs")
library("xts")
#Overwrite getIncidence method for obkData such that it handles
#new doBy argument. Could also use Thibaut's ggplot code, but
#the advantage is that we get the time series directly (easily convertible to, e.g., xts)
source("getIncidence.R")

#Load example data
load("data/hagelloch.RData")

#Create extra factor variable for AGEGRPS
hagelloch.obk@individuals$AGEGRP <- cut(hagelloch.obk@individuals$AGE, breaks=c(0,5,10,Inf))

#Manual way using the OutbreakTools::get.incidence function
#Note: the get.incidence adds an extra zero at the end
#inci <- get.incidence(hagelloch.obk, "timeERU", add.zero=FALSE)
#Todo: Aggregate like a boss (using formula interface) as follows:
#
#           get.incidence(hagelloch.obk, timeERU ~ SEX)
#
inci <- get.incidence(hagelloch.obk, "timeERU", doBy=c("SEX","AGEGRP"), add.zero=FALSE)

#Convert to xts object
#sts.xts <- xts( x=inci[[1]]$incidence, order.by=inci[[1]]$date, frequency=365)
sts.xts <- inc2xts(inci)
plot(as.zoo(sts.xts), screens=c(1,2))

#You can click and drag to zoom. Double-clicking will zoom you back out. Shift-drag will pan
#See also: http://dygraphs.com/
foo <- dygraph(sts.xts, main = "Hagelloch") %>% 
  dyRangeSelector(dateWindow = range(epoch(sts)))

#Add some clickCallback handler - note that the alert function is javascript
foo$x$attrs$clickCallback = htmlwidgets::JS('function(e,x,pts) { alert(JSON.stringify(pts))}')
foo
