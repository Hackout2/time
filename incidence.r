library('OutbreakTools')
library('data.table')
data(HorseFlu)

## get data
dt <- data.table(HorseFlu@records$shedding)

## calculate incidence
incidence <- dt[!is.na(date), list(incidence = .N), by = date]

## create data.table of all dates, to fill in incidnce
dates <- data.table(date = seq.Date(min(dt[, date]), max(dt[, date]), by = "day"))

## merge
incidence <- merge(incidence, dates, by = "date", all.y = TRUE)

## convert NA's to zeroes
incidence[is.na(incidence), incidence := 0]
