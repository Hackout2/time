% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/getIncidence.R
\docType{methods}
\name{get.incidence2,obkData-method}
\alias{get.incidence2,obkData-method}
\title{Modified obkData method such that doBy statements are possible}
\usage{
\S4method{get.incidence2}{obkData}(x, data, where = NULL, val.min = NULL,
  val.max = NULL, val.kept = NULL, regexp = NULL, from = NULL,
  to = NULL, interval = 1, add.zero = TRUE, doBy = NULL, ...)
}
\arguments{
\item{x}{See obkData::get.incidence}

\item{data}{See obkData::get.incidence}

\item{where}{See obkData::get.incidence}

\item{val.min}{See obkData::get.incidence}

\item{val.max}{See obkData::get.incidence}

\item{val.kept}{See obkData::get.incidence}

\item{regexp}{See obkData::get.incidence}

\item{from}{See obkData::get.incidence}

\item{to}{See obkData::get.incidence}

\item{interval}{See obkData::get.incidence}

\item{add.zero}{See obkData::get.incidence}

\item{doBy}{A character list of factor variables in the individuals slot to compute the ts for.}

\item{...}{See obkData::get.incidence}
}
\description{
Modified obkData method such that doBy statements are possible
}

