library("dygraphs")
library("xts")

#Run example data
source("obk2sts.R")

#Convert to plainstyle ts class (doesn't work for dygraph)
sts.ts <- as(sts,"ts")
try.xts(sts.ts)

#Do conversion directly
sts.xts <- xts( x=observed(sts), order.by=epoch(sts), frequency=365)

#You can click and drag to zoom. Double-clicking will zoom you back out. Shift-drag will pan
#See also: http://dygraphs.com/
dygraph(sts.xts, main = "Hagelloch")

dygraph(sts.xts, main = "Hagelloch") %>% 
  dyRangeSelector(dateWindow = range(epoch(sts)))
