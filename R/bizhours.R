#' @title bizhours
#' @author Sujeet G Pillai
#' @keywords business hours, business, hours, calculate, bizhours, business, time,
#' difftime, time, difference
#' @description Calculate the business hours between two datetime vectors
#' @details Calculates the difference between two vectors of date times in terms of
#' business hours, where business hours is determined using the start_time and end_time
#' parameters. The function can be used seamlessly for night-shift calculations, eg. for 
#' a 9pm to 6am shift, \code{start_time} is 21:00 and \code{end_time} is 06:00
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
#' @return A numeric vector that shows the difference in business hours between
#' \code{from_dates} and \code{to_dates}
#' @example
#' from_dates <- c(as.POSIXct(x = "10/19/2017 15:00", format = "%m/%d/%Y %H:%M"), as.POSIXct(x = "10/25/2017 15:00", format = "%m/%d/%Y %H:%M"))
#' to_dates <- c(as.POSIXct(x = "10/23/2017 17:00", format = "%m/%d/%Y %H:%M"), as.POSIXct(x = "10/25/2017 17:00", format = "%m/%d/%Y %H:%M"))
#' start_time <- "9:00"
#' end_time <- "18:00"
#' bizhours(from_dates = from_dates, to_dates = to_dates, start_time = start_time, end_time = end_time)
#' @export
bizhours <- function(from_dates, to_dates, start_time, end_time, timezone = "", calendar = ""){
  
  offset_days <- 0
  if(as.POSIXct(x = paste0(as.Date(Sys.Date()), start_time), tz = timezone, format = "%Y-%m-%d %H:%M") >
     as.POSIXct(x = paste0(as.Date(Sys.Date()), end_time), tz = timezone, format = "%Y-%m-%d %H:%M")){
    offset_days <- 1
  }
  
  if(calendar == ""){
    bizdays::create.calendar(name = "Brazil/ANBIMA", holidays = bizdays::holidaysANBIMA, weekdays = c("saturday", "sunday"))
    calendar <- "Brazil/ANBIMA"
  }
  
  ##  Calculating the number of HOURS in a work shift (i.e. between start_time and end_time)
  daily_business_hours <- as.numeric(
    difftime(time1 = as.POSIXct(x = paste0(as.Date(Sys.Date() + offset_days), end_time), tz = timezone, format = "%Y-%m-%d %H:%M"),
             time2 = as.POSIXct(x = paste0(as.Date(Sys.Date()), start_time), tz = timezone, format = "%Y-%m-%d %H:%M"),
             units = "hours")
  )
  
  ##  Calculating the number of BUSINESS DAYS between from_dates and to_dates 
  intermediate_business_hours <- (bizdays::bizdays(from = from_dates, to = to_dates, cal = calendar) - 1) * daily_business_hours
  
  ##  Calculating the business start and end times for all from_dates and all end_dates
  #  BUSINESS START TIMES of all from_dates
  from_dates_start <- do.call("c",
                              lapply(X = from_dates, FUN = function(X){
                                as.POSIXct(x = paste0(as.Date(X), " ", start_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                              }))
  #  BUSINESS END TIMES of all from_dates
  from_dates_end <- do.call("c",
                            lapply(X = from_dates, FUN = function(X){
                              as.POSIXct(x = paste0(as.Date(X), " ", end_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                            }))
  #  BUSINESS START TIMES of all end_dates
  to_dates_start <- do.call("c",
                            lapply(X = to_dates, FUN = function(X){
                              as.POSIXct(x = paste0(as.Date(X), " ", start_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                            }))
  #  BUSINESS END TIMES of all end_dates
  to_dates_end <- do.call("c",
                          lapply(X = to_dates, FUN = function(X){
                            as.POSIXct(x = paste0(as.Date(X), " ", end_time), tz = timezone, format = "%Y-%m-%d %H:%M")
                          }))
  
  
  ##  Calculating the BUSINESS HOURS SPENT IN THE FIRST DAY
  first_day_hours <- unlist(lapply(X = c(1:length(from_dates)), FUN = function(X){
    if(from_dates[X] < from_dates_start[X]){
      daily_business_hours
    }else if(from_dates_end[X] < from_dates[X]){
      0
    }else{
      as.numeric(difftime(time1 = from_dates_end[X], time2 = from_dates[X], tz = timezone, units = "hours"))
    }
  }))
  ##  Calculating the BUSINESS HOURS SPENT IN THE LAST DAY
  last_day_hours <- unlist(lapply(X = c(1:length(to_dates)), FUN = function(X){
    if(to_dates[X] < to_dates_start[X]){
      0
    }else if(to_dates_end[X] < to_dates[X]){
      daily_business_hours
    }else{
      as.numeric(difftime(time1 = to_dates[X], time2 = to_dates_start[X], tz = timezone, units = "hours"))
    }
  }))
  
  ##  Finally 
  ##  Business Hours Spent = 
  ##  Business Hours Spent between first and last day + 
  ##  Business Hours Spent in First Day + 
  ##  Business Hours Spent in Last Day
  business_hours <- intermediate_business_hours + first_day_hours + last_day_hours
  
  return(business_hours)
  
}
