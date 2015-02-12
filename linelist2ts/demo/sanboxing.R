#' None package material experimental code for getting incidence curves.
#'
#' @author Michael Höhle <http://www.math.su.se/~hoehle>
#'

#Load the package itself.
library("linelist2ts")

#Load extra libraries for the visualization
library("dygraphs")
library("RColorBrewer")
library("scales")
library("ggplot2")

#Load example data form Hagelloch 1861 measles outbreak
data("hagelloch.obk")

#Create extra factor variable for AGEGRPS
hagelloch.obk@individuals$AGEGRP <- cut(hagelloch.obk@individuals$AGE, breaks=c(0,5,10,Inf))

#Todo: Aggregate like a boss (using formula interface) as follows:
#
#           get.incidence2(hagelloch.obk, timeERU ~ SEX)
#
#inc <- get.incidence(hagelloch.obk, "timeERU", doBy=c("SEX","AGEGRP"), add.zero=FALSE)
inc <- get.incidence2(hagelloch.obk, "timeERU", doBy=c("SEX","CL"), add.zero=FALSE)

#Convert incList to xts object and plot (ToDo: improve using dplyr?)
sts.xts <- inc2xts(inc)

################## plot.zoo visualization ################
pal <- brewer.pal(n=ncol(sts.xts),"Set3")
plot(as.zoo(sts.xts), plot.type="single",col=pal,lwd=3,xlab="Onset of rash",ylab="No. individuals",type="l")
grid(ny=NULL,nx=NA,col="darkgray")
legend(x="topleft",colnames(sts.xts),col=pal,lwd=3)

plot(as.zoo(sts.xts), plot.type="multiple",col=pal,lwd=3,xlab="Onset of rash",ylab="No. individuals",type="l",main="")



##########ggplot2 like plots from zoo objects ############
#(see http://stackoverflow.com/questions/13848218/drawing-a-multiline-graph-with-ggplot2-from-a-zoo-object)
p <- autoplot(sts.xts, facet = NULL)
p
#p + aes(linetype = NULL)
p + scale_x_date(labels = date_format("%d-%b-%Y"), xlab("Onset of rash")) +  scale_y_continuous(ylab("No. individuals"))

############# Interactive graphics using dygraph ###########
#You can click and drag to zoom. Double-clicking will zoom you back out. Shift-drag will pan
#See also: http://dygraphs.com/.
#Just show the first two series.
foo <- dygraph(sts.xts[,1:2], main = "Hagelloch") %>%
  dyRangeSelector(dateWindow = range(index(sts.xts)))
foo

#Add some clickCallback handler - note that the alert function is javascript
foo$x$attrs$clickCallback = htmlwidgets::JS('function(e,x,pts) { alert(JSON.stringify(pts))}')
foo
