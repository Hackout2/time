#' Overwrites the get.incidence function for obkData objects to support doBy
#'
#' Creates different time series based on the list of factor variables.
#' This function should eventually migrate back into the OutbreakTools package.
#'
#' @param x The object (typically obkdata) to compute the incidence for.
#' @param ... Whatever
#'
#' @author Michael Höhle
#' @export
setGeneric("get.incidence2", function(x, ...) standardGeneric("get.incidence2"))

#' Modified obkData method such that doBy statements are possible
#'
#' @param x  See obkData::get.incidence
#' @param data  See obkData::get.incidence
#' @param where  See obkData::get.incidence
#' @param val.min  See obkData::get.incidence
#' @param val.max  See obkData::get.incidence
#' @param val.kept  See obkData::get.incidence
#' @param regexp  See obkData::get.incidence
#' @param from  See obkData::get.incidence
#' @param to  See obkData::get.incidence
#' @param interval  See obkData::get.incidence
#' @param add.zero See obkData::get.incidence
#' @param doBy A character list of factor variables in the individuals slot to compute the ts for.
#' @param ...  See obkData::get.incidence
#' @export
setMethod("get.incidence2", "obkData", function(x, data, where=NULL, val.min=NULL, val.max=NULL, val.kept=NULL, regexp=NULL,
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

  ##If there are no dates we are done.
  if(length(dates)==0) return(NULL)

  ##Prepare the return list
  res <- list()

  #If there is no from-to specification make
  #sure it's not data subset dependend, but is the
  #same for each subset.
  if (is.null(from) & is.null(to)) {
    from <- min(dates)
    to <- max(dates)
  }

  ##Loop over all variables in doBy
  if (!is.null(doBy)) {
    for (i in seq_len(length(doBy))) {
   #   browser()
      theData <- get.data(x, data=doBy[[i]], showSource=TRUE)

      if (is.null(theData)) stop(paste0("Data for ",doBy[[i]]," cannot be found in this obkData object."))
      if (!is.factor(theData[,doBy[[i]]])) stop("The variable ",doBy[[i]]," is not a factor.")

      res[[doBy[i]]] <- tapply(dates, INDEX=theData[,doBy[[i]]], FUN=get.incidence, from=from, to=to, interval=interval, add.zero=add.zero,simplify=FALSE)
    }
  } else {
    res <- list(get.incidence(dates, from=from, to=to, interval=interval, add.zero=add.zero))
  }



  ## RETURN OUTPUT ##
  return(res)
}) # end obkData method

#' Helper function to format a get.incidence list of data.frames
#' to a multivariate xts object
#'
#' @param incList List of lists containing the data.frames from get.incidence2
#' @return An xts object corresponding to the flattened incList
#' @export
inc2xts <- function(incList) {

  #This was really the most efficient way of collapsing it.
  df <- sapply(unlist(incList,recursive=FALSE), "[[", 2)

  #Create xts object
  xts <- xts(x=df, order.by=incList[[1]][[1]]$date)

  return(xts)
}

