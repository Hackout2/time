#Small demo illustrating how to do manual aggregation using the data.table
#function. This should allow for more flexible grouping also.

library("linelist2ts")
library("data.table")

#Extract the data.frame from the Hagelloch data
dt <- data.table(hagelloch.obk@individuals)
#Select which date column to do the aggregation by and make a column data
col <- "ERU"
dt$date <- dt[[col]]


### calculate incidence by summarizing per date
#incidence <- dt[!is.na(date), list(incidence = .N), by = date]
##Summarizing by week (problem if outbreak is longer than a year)
#incidence <- dt[!is.na(date), list(incidence = .N), by = week(date)]
#dates <- data.table(date = seq.Date(monday(min(dt[, date])), monday(max(dt[, date])), by = "1 week"))

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

xts <- xts(x=incidence.ts$incidence, order.by=incidence.ts[["date"]])
autoplot(xts) + scale_x_date(labels = date_format("%G-W%V"), xlab("Time of onset of rash (ISO week)")) +  scale_y_continuous(ylab("No. individuals"))

