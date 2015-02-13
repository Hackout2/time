#Small demo illustrating how to do manual aggregation using the data.table
#function. This should allow for more flexible grouping also.

library("linelist2ts")
library("data.table")
library("scales")
library("ggplot2")
library("surveillance")
library("animation")
library("OutbreakTools")

#Extract the data.frame from the Hagelloch data
dt <- data.table(hagelloch.obk@individuals)
#Select which date column to do the aggregation by and make a column data
col <- "ERU"
dt$date <- dt[[col]]

#To aggregate by ISO weeks we need a function to aggregate by monday of the ISO week
all.equal(data.table::week(dt$date),as.numeric(format(dt$date,"%V")))
all.equal(lubridate::week(dt$date),as.numeric(format(dt$date,"%V")))
all.equal(lubridate::isoweek(dt$date),as.numeric(format(dt$date,"%V")))

#Weekly data based on ISOweek (list not necessary if the column is ok to be called monday)
incidence <- dt[!is.na(date), list(incidence = .N), by = list(date=monday(date))]

## create data.table of all dates, to fill in incince
dates <- data.table(date = seq.Date(monday(min(dt[, date])), monday(max(dt[, date])), by = "1 week"))

## merge
incidence.ts <- merge(incidence, dates, by = "date", all.y = TRUE)

## Convert NA's to zeroes
incidence.ts[is.na(incidence), incidence := 0]

#Compare output
ts <- get.incidence(hagelloch.obk,data="timeERU", where="records", interval=7,
                    from=head(dates[["date"]],n=1),to=tail(dates[["date"]],n=1),add.zero=FALSE)
all.equal(ts$date , incidence.ts[["date"]])

#Daily time series (for all)
#debug("linelist2xts")
xts2 <- linelist2xts(formula=ERU ~ 1, dateProjFun=identity, data=as.data.frame(hagelloch.obk@individuals))
#Weekly time series (for all)
xts3 <- linelist2xts(formula=ERU ~ 1, dateProjFun=monday, data=as.data.frame(dt))
all(ts$date == index(xts3))
all(ts$incidence == xts3)
#Monthly time series (for all)
xts4 <- linelist2xts(formula=ERU ~ 1, dateProjFun=firstOfMonth, data=as.data.frame(dt))

#Weekly time series per gender
xts5 <- linelist2xts(formula=ERU ~ SEX, dateProjFun=monday, data=as.data.frame(dt))

plot(as.zoo(xts5),main="",type="h",xlab="Time of onset (week)")

#ggplot2 version
autoplot(xts5) + scale_x_date(labels = date_format("%G-W%V"), xlab("Time of onset of rash (ISO week)")) +  scale_y_continuous(ylab("No. individuals"))


#Dirty mapping of the households
location <- hagelloch.obk@individuals[,c("x.loc","y.loc")]
locationFactor <- apply(location, 1, paste0, collapse="-")
hagelloch.obk@individuals$locationFactor <- as.factor(locationFactor)
xts <- linelist2xts(formula=ERU ~ locationFactor, dateProjFun=monday, data=hagelloch.obk@individuals)

#Small fake map for the locations
box <- 10*cbind(c(0,1,1,0,0),c(0,0,1,1,0))
households <- list()
uLoc <- unique(cbind(location,locationFactor))
for (i in 1:nrow(uLoc)) {
  households[[i]] <- Polygons(list(Polygon(t(apply(box,1, function(x) x + as.numeric(uLoc[i,1:2]))))),ID=uLoc[i,3])
}
map <- SpatialPolygons(households)
plot(map)

#Convert xts object to sts class
sts <- new("sts", observed=as.matrix(xts), epoch=as.numeric(index(xts)), epochAsDate=TRUE, map=map)
plot(sts, xlab="Date of rash (week)",
     xaxis.tickFreq = list("%V" = atChange, "%m" = atChange,"%G" = atChange),
     xaxis.labelFreq = list("%V" = atChange),
     xaxis.labelFormat = "%d-%b", type = observed ~ time, legend.opts=NULL)

#Do an animation
saveHTML(animate(sts, tps = 1:8, total.args = list()),
         title = "Evolution of the measles epidemic in Hagelloch",
         ani.width = 500, ani.height = 600)

###Demo using surveillance
data("measlesWeserEms")
saveHTML(animate(measlesWeserEms, tps = 1:52, total.args = list()),
            title = "Evolution of the measles epidemic in the Weser-Ems region",
            ani.width = 500, ani.height = 600)
