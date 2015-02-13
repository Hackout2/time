#' Version of the wday function from lubridate, where the monday is the 1.
#'
#' The function returns a vector of integer, where mondays are 1 and sundays are 7.
#'
#'@note Function could have been written with pure strptime commands in case
#'one does not want the lubridate dependencies.
#'
#' @param date - vector of dates
#' @return vector of numeric with the weekdays (mon=1, sun=7)
#' @export
wdaymon <- function(date) { lubridate::wday(date - 1) }

#' Function to get the monday of the corresponding week a date belongs to.
#'
#' @note Function could have been written with pure strptime commands in case
#' one does not want the lubridate dependencies. This is the function Sebastian
#' always wanted, sorry it's not in a standalone package.
#'
#' @param date vector of dates
#' @return vector of dates containing the corresponding mondays
#' @export
monday  <- function(date) { date - wdaymon(date) + 1}

#' Function to get the first of the month.
#'
#' @param date vector of dates
#' @return vector of dates containing the corresponding mondays
#' @note Function could have been written with pure strptime commands in case
#' one does not want the lubridate dependencies.
#' @export
firstOfMonth <- function(date) { date - lubridate::mday(date) + 1}
