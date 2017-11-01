#' @title bizhours
#' @author Sujeet G Pillai
#' @keywords business hours, business, hours, calculate, bizhours, business, time,
#' difftime, time, difference
#' @description Calculate the business hours between two datetime vectors
#' @details Calculates the difference between two vectors of date times in terms of
#' business hours, where business hours is determined using the start_time and end_time
#' parameters.
#' @param from_dates A vector of POSIXct objects that are date-times from when to start
#' calculating business hours.
#' @param to_dates A vector of POSIXct objects that are date-times till when to
#' calculate business hours.
#' @param start_time Start of work shift. A string in format %H:%M that shows the time -
#' in 24 hour format - of the day when the business hours start.
#' @param end_time End of work shift. A string in format %H:%M that shows the time -
#' in 24 hour format- of the day when the business hours end.
#' @param timezone A string that represents the timezone. Default: "" - local timezone
#' @param calendar A string that represents the name of the \code{bizdays} calendar to use.
#'  Default: "" - uses \code{Brazil/ANBIMA} calendar of package \code{bizdays} with
#'  workweek from Monday through Friday.
#' @param through_night_shift A boolean, default : False. Set true if work shift spans
#' through midnight. For example: For a 9am-5pm job or for a 2am to 12pm job set this as
#' \code{False} and for a 9pm-5am job set this as \code{True}.
#' @return A numeric vector that shows the difference in business hours between
#' \code{from_dates} and \code{to_dates}
#' @example
#' from_dates <- c(as.POSIXct(x = "10/19/2017 15:00", format = "%m/%d/%Y %H:%M"), as.POSIXct(x = "10/25/2017 15:00", format = "%m/%d/%Y %H:%M"))
#' to_dates <- c(as.POSIXct(x = "10/23/2017 17:00", format = "%m/%d/%Y %H:%M"), as.POSIXct(x = "10/25/2017 17:00", format = "%m/%d/%Y %H:%M"))
#' start_time <- "9:00"
#' end_time <- "18:00"
#' bizhours(from_dates = from_dates, to_dates = to_dates, start_time = start_time, end_time = end_time)
#' @export
bizhours <- function(from_dates, to_dates, start_time, end_time, timezone = "", calendar = "", through_night_shift = FALSE){

  offset_days <- 0
  if(through_night_shift){
    offset_days <- 1
  }

  if(calendar == ""){
    bizdays::create.calendar(name = "Brazil/ANBIMA", holidays = bizdays::holidaysANBIMA, weekdays = c("saturday", "sunday"))
    calendar <- "Brazil/ANBIMA"
  }

  daily_business_hours <- as.numeric(
    difftime(time1 = as.POSIXct(x = paste0(as.Date(Sys.Date() + offset_days), end_time), tz = timezone, format = "%Y-%m-%d %H:%M"),
             time2 = as.POSIXct(x = paste0(as.Date(Sys.Date()), start_time), tz = timezone, format = "%Y-%m-%d %H:%M"),
             units = "hours")
  )

  intermediate_business_hours <- (bizdays::bizdays(from = from_dates, to = to_dates, cal = calendar) - 1) * daily_business_hours

  from_dates_start <- do.call("c",
                              lapply(X = from_dates, FUN = function(X){
                                as.POSIXct(x = paste0(as.Date(X), " ", start_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                              }))
  from_dates_end <- do.call("c",
                            lapply(X = from_dates, FUN = function(X){
                              as.POSIXct(x = paste0(as.Date(X) + offset_days, " ", end_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                            }))
  to_dates_start <- do.call("c",
                            lapply(X = to_dates, FUN = function(X){
                              as.POSIXct(x = paste0(as.Date(X) - offset_days, " ", start_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                            }))
  to_dates_end <- do.call("c",
                          lapply(X = to_dates, FUN = function(X){
                            as.POSIXct(x = paste0(as.Date(X), " ", end_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                          }))



  first_day_hours <- unlist(lapply(X = c(1:length(from_dates)), FUN = function(X){
    if(from_dates[X] < from_dates_start[X]){
      daily_business_hours
    }else if(from_dates_end[X] < from_dates[X]){
      0
    }else{
      as.numeric(difftime(time1 = from_dates_end[X], time2 = from_dates[X], tz = timezone, units = "hours"))
    }
  }))

  last_day_hours <- unlist(lapply(X = c(1:length(to_dates)), FUN = function(X){
    if(to_dates[X] < to_dates_start[X]){
      0
    }else if(to_dates_end[X] < to_dates[X]){
      daily_business_hours
    }else{
      as.numeric(difftime(time1 = to_dates[X], time2 = to_dates_start[X], tz = timezone, units = "hours"))
    }
  }))

  business_hours <- intermediate_business_hours + first_day_hours + last_day_hours

  return(business_hours)

}
