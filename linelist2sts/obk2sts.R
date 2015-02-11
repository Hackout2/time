library("OutbreakTools")
library("surveillance")
source("hagelloch2obkData.R")
data("ToyOutbreak", package="OutbreakTools")

#Extract entire data.frame with individual information
#For some reason the date of infection(=onset) is part of the individuals
#slot, and not the records slot. Not sure why it's the case
df <- slot(ToyOutbreak,"individuals")
class(df$DateInfected)
#Not sure why there is a difference here...
get.data(ToyOutbreak, "DateInfected")


#Make hagelloch an obkData object and extract individual info
hagelloch.obk <- hagelloch2obkData()
save(file="data/hagelloch.RData", list=c("hagelloch.obk"))
df <- hagelloch.obk@individuals

#Look at the records entry.
names(hagelloch.obk@records)
length(get.dates(hagelloch.obk, data="records"))
get.data(hagelloch.obk, "timeERU")

hagelloch.obk@records$timeERU$date

#Use the surveillance::linelist2sts function to convert this to an sts object
#Todo: Add a splitBy argument
sts <- surveillance::linelist2sts(df, dateCol="ERU",aggregate.by="1 day")
print(sts)

#Plot using surveillance functionality
#Possible actions: do plot in ggplot2 instead
plot(sts,xaxis.tickFreq = list("%d"=at2ndChange, "%m"=atChange),
     xaxis.labelFreq = list("%d" = atChange, "%m"= atChange),
     xaxis.labelFormat = "%d-%m", #Todo: #list("%m"="%m","%Y"="%Y"), 
     legend.opts=NULL, 
     col=c("lightblue"),
     xlab="Date of rash",ylab="No. individuals")
grid(nx=NA,ny=NULL,col="darkgray")
#Same but now as monthly data (not so useful)
sts2 <- surveillance::linelist2sts(df, dateCol="ERU",aggregate.by="1 month")

plot(sts2,xaxis.tickFreq = list("%m"=atChange),
     xaxis.labelFreq = list( "%m"= atChange),
     xaxis.labelFormat = "%b-%Y",
     legend.opts=NULL, 
     xlab="Date of rash",ylab="No. individuals")
