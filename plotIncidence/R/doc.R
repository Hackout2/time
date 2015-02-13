#' plotIncidence: A package for plotting incidence data.
#'
#' This package provides functions for computing and plotting incidence data from outbreak line lists.
#'
#'
#' @section Available functions include:
#'  \itemize{
#'  \item{plotIncidence}{Plot incidence time series using histograms}
#' }
##
#'
#' @docType package
#' @name plotIncidence
NULL




#' #' #' Toy outbreak dataset
#'
#' This is a simplified version of Michael Hohle's dataset Hagelloch
#'
#' \itemize{
#'   \item dateOfOnset a vector of dates of symptom onset in Date format
#'   \item gender the gender of the patient
#'   \item complication an indication of possible clinical complications
#' }
#'
#' @format A data frame with 187 rows and 3 variables
#' @source Michael Hohle's dataset Hagelloch
#' @name toy
NULL


#' #' #' Zombie outbreak dataset
#'
#' This is a dataset simulating an outbreak with geo-localisation of the cases (6 localities)  
#'
#' \itemize{
#'   \item x.coord a vector of x coordinates
#'   \item y.coord a vector of y coordinates
#'   \item date a vector of dates of recorded cases in Date format
#'   \item gender the gender of the patient
#' }
#'
#' @format A data frame with 154013 rows and 4 variables
#' @name zombie_outbreak
NULL
