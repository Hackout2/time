#' None package material experimental code for getting incidence curves.
#' 
#' @author Michael Höhle
#' 
library("dygraphs")
library("xts")
library("RColorBrewer")
library("OutbreakTools")
library("scales")
library("ggplot2")

#Overwrite getIncidence method for obkData such that it handles
#new doBy argument. Could also use Thibaut's ggplot code, but
#the advantage is that we get the time series directly (easily convertible to, e.g., xts)
source("getIncidence.R")

#Load example data
load("../data/hagelloch.RData")

#Create extra factor variable for AGEGRPS
hagelloch.obk@individuals$AGEGRP <- cut(hagelloch.obk@individuals$AGE, breaks=c(0,5,10,Inf))

#Manual way using the OutbreakTools::get.incidence function
#Note: the get.incidence adds an extra zero at the end
#inci <- get.incidence(hagelloch.obk, "timeERU", add.zero=FALSE)
#Todo: Aggregate like a boss (using formula interface) as follows:
#
#           get.incidence2(hagelloch.obk, timeERU ~ SEX)
#
#inc <- get.incidence(hagelloch.obk, "timeERU", doBy=c("SEX","AGEGRP"), add.zero=FALSE)
inc <- get.incidence2(hagelloch.obk, "timeERU", doBy=c("SEX","CL"), add.zero=FALSE)

#Convert incList to xts object and plot
sts.xts <- inc2xts(inc)
pal <- brewer.pal(n=ncol(sts.xts),"Set3")
plot(as.zoo(sts.xts), plot.type="single",col=pal,lwd=3,xlab="Onset of rash",ylab="No. individuals")
grid(ny=NULL,nx=NA,col="darkgray")
legend(x="topleft",colnames(sts.xts),col=pal,lwd=3)

##########ggplot2 like plots from zoo objects ############
#(see http://stackoverflow.com/questions/13848218/drawing-a-multiline-graph-with-ggplot2-from-a-zoo-object)
p <- autoplot(sts.xts, facet = NULL)
p
p + aes(linetype = NULL)
p + scale_x_date(labels = date_format("%d-%b"), xlab("Onset of rash")) +  scale_y_continuous(ylab("No. individuals"))

###### Interactive graphics ######################

#You can click and drag to zoom. Double-clicking will zoom you back out. Shift-drag will pan
#See also: http://dygraphs.com/
foo <- dygraph(sts.xts, main = "Hagelloch") %>% 
  dyRangeSelector(dateWindow = range(index(sts.xts)))

#Add some clickCallback handler - note that the alert function is javascript
foo$x$attrs$clickCallback = htmlwidgets::JS('function(e,x,pts) { alert(JSON.stringify(pts))}')
foo
