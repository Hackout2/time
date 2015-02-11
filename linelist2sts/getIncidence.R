#' Overwrites the get.incidence function for obkData objects to support doBy
#' 
#' Creates different time series based on the list of factor variables.
#' This function should eventually migrate back into the OutbreakTools package.
#' 
#' @author Michael Höhle
#' 
####################
## obkData method ##
####################
##
## based on 'dates' associated to a given field
## 'values' are optional and can be used to subset the retained 'dates'
## (e.g. define what a positive case is)
setMethod("get.incidence", "obkData", function(x, data, where=NULL, val.min=NULL, val.max=NULL, val.kept=NULL, regexp=NULL,
                                               from=NULL, to=NULL, interval=1, add.zero=TRUE, doBy=NULL, ...){
  ## HANDLE ARGUMENTS ##
  if(is.null(val.min)) val.min <- -Inf
  if(is.null(val.max)) val.max <- Inf
  
  
  ## GET DATA ##
  df <- get.data(x, data=data, where=where, showSource=TRUE)
  if(is.null(df)) stop(paste("Data",data,"cannot be found in this obkData object"))
  
  ## call specific procedures if applicable ##
  if(inherits(df, c("obkSequences", "obkContacts"))) {
    return(get.incidence(df, from=from, to=to,
                         interval=interval, add.zero=add.zero))
  }
  
  
  ## OTHERWISE: DATA ASSUMED TAKEN FROM RECORDS ##
  ## if data=='records', keep the first data.frame of the list ##
  if(is.list(df) && !is.data.frame(df) && is.data.frame(df[[1]])) df <- df[[1]]
  
  ## get dates ##
  if(!"date" %in% names(df)) stop("no date in the data")
  dates <- df$date
  
  ## get optional values associated to the dates ##
  ## keep 'data' if it is there
  if(data %in% names(df)){
    values <- df[[data]]
  } else { ## else keep first optional field
    temp <- !names(df) %in% c("individualID","date") # fields being not "individualID" or "date"
    if(any(temp)) {
      values <- df[,min(which(temp))]
    } else {
      values <- NULL
    }
  }
  
  
  ## EXTRACT RELEVANT DATES ##
  if(!is.null(values)){
    toKeep <- rep(TRUE, length(values))
    
    ## if 'values' is numeric ##
    if(is.numeric(values)){
      toKeep <- toKeep & (values>=val.min & values<=val.max)
    }
    
    ## if val.kept is provided ##
    if(!is.null(val.kept)) {
      toKeep <- toKeep & (values %in% val.kept)
    }
    
    ## if regexp is provided ##
    if(!is.null(regexp)) {
      temp <- rep(FALSE, length(values))
      temp[grep(regexp, values, ...)] <- TRUE
      toKeep <- toKeep & temp
    }
    
    dates <- dates[toKeep]
  }
  
  ##If there is nothing.
  if(length(dates)==0) return(NULL)
  
  ##hoehle -- 
  res <- list()
  if (!is.null(doBy)) {
    for (i in 1:length(doBy)) {
     # browser()
      theData <- get.data(x, data=doBy[[i]], showSource=TRUE)
      
      if (is.factor(theData[,doBy[[i]]])) {
        res[[doBy[i]]] <- tapply(dates, INDEX=theData[,doBy[[i]]], FUN=get.incidence, from=from, to=to, interval=interval, add.zero=add.zero)        
      } else {
        stop("The variable ",doBy[[i]]," is not a factor.")
      }
    } 
  } else {
    res <- list(get.incidence(dates, from=from, to=to, interval=interval, add.zero=add.zero))
  }
  
  ## CALL THE DATE PROCEDURE ##
  #if(length(dates)==0) return(NULL)
  #out <- get.incidence(dates, from=from, to=to,
  #                     interval=interval, add.zero=add.zero)
  
  ## RETURN OUTPUT ##
  return(res)
}) # end obkData method

inc2xts <- function(inci) {
  xts.list <- lapply(inci, function(list) lapply(list, function(df) {
    print(dim(df))
    with(df,  as.xts(incidence, order.by=date))
  }))
  #lapply(xts.list, merge). This is currently not working!
  xts <- merge(xts.list[["SEX"]]$male, xts.list[["SEX"]]$female, fill=0)
  return(xts)
}

doIt <- function() {
  source("getIncidence.R")
  hagelloch.obk@individuals$AGEGRP <- cut(hagelloch.obk@individuals$AGE, breaks=c(0,10,20,Inf))
  inci <- get.incidence(hagelloch.obk, "timeERU", doBy=c("SEX","AGEGRP"), add.zero=FALSE)
  length(inci)
  str(inci)
  
  #Show the time series.
  plot(inc2xts(inci))  
  plot(as.zoo(inc2xts(inci)), screens=c(1,2))
  plot(as.zoo(inc2xts(inci)), plot.type='multiple')
  
  
  foo <- dygraph(inc2xts(inci), main = "Hagelloch") %>% 
    dyRangeSelector(dateWindow = range(epoch(sts)))
  foo  
}