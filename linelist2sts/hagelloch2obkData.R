#' Convert Hagelloch 1861 measles outbreak into obkData
#' 
#' Small converter function to take the hagelloch.df data.frame
#' available in the surveillance package and make an obkData object
#' out of it.
#' 
hagelloch2obkData <- function() {
  #Use Hagelloch measles data (as available in the surveillance package) instead
  data("hagelloch", package="surveillance")
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
  hagelloch.obk <- new("obkData", individuals=hagelloch.df, records=records)
  
  #Consistency checks
  class(foo <- get.dates(hagelloch.obk, data="records"))
  all.equal(hagelloch.obk@records$PRO$date,foo[1:nrow(hagelloch.df)])
  
  return(hagelloch.obk)
}
