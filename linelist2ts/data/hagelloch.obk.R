#Script to generate Hagelloch data

library("OutbreakTools")

hagelloch.obk <- (function() {
  #Use Hagelloch measles data (as available in the surveillance package) instead
  load(system.file("data","hagelloch.RData", package="surveillance"))

  #The variable PN contains the ID, use the OutbreakTools name 'individualID' instead
  names(hagelloch.df)[pmatch("PN",names(hagelloch.df))] <- "individualID"
  #Remove the individual, which must have gotten infected for other sources than from the outbreak
  diff(sort(hagelloch.df$ERU))
  hagelloch.df <- hagelloch.df[-which.max(hagelloch.df$ERU),]
  nrow(hagelloch.df)

  #Variables with date information in the Hagelloch data.set
  dateVars <- c("PRO", "ERU", "DEAD")
  records <- lapply(dateVars, function(varName) {
    data.frame(individualID=hagelloch.df$individualID, date=hagelloch.df[,varName])
  })
  #Give the list appropriate names (ensure names are not the same as in 'individuals')
  names(records) <- paste0("time",dateVars)

  #Create obkData object
  hagelloch.obk <- methods::new("obkData", individuals=hagelloch.df, records=records)

  #Consistency checks
  class(foo <- OutbreakTools::get.dates(hagelloch.obk, data="records"))
  all.equal(hagelloch.obk@records$PRO$date,foo[1:nrow(hagelloch.df)])

  return(hagelloch.obk)
})()

#
#hagelloch.obk <- create.hagelloch.obk()

