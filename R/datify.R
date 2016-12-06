#' Turn two dates into a clean word description of the time
#' 
#' @param startdate First date
#' @param enddate Last date
#' @export
#' @examples 
#' \dontrun{datify(startdate, enddate)}

datify = function(startdate, enddate) {
  months = c("January","February","March","April","May","June","July","August","September","October","November","December")
  startdate = lubridate::date(startdate)
  enddate = lubridate::date(enddate)
  if (month(startdate) == 1 & day(startdate) == 1 & month(enddate) == 12 & day(enddate) == 31) {
    if (year(startdate) == year(enddate)) {
      time = year(startdate)
    } else {
      time = paste0(year(startdate),"-",year(enddate))
    }
  } else if (day(startdate) == 1 & day(enddate) %in% c(30,31)) {
    if (year(startdate) == year(enddate)) {
      if (month(startdate) == month(enddate)) {
        time = paste(months[month(startdate)], year(startdate))
      } else {
        time = paste(months[month(startdate)], "-", months[month(enddate)], year(startdate))
      }
    } else {
      time = paste(months[month(startdate)], year(enddate),
                   "-", months[month(enddate)], year(enddate))
    }
  } else {
    startdate = redo_date(startdate)
    enddate = redo_date(enddate)
    if (startdate == enddate) {
      time = startdate
    } else {
      time = paste(startdate,"-",enddate)
    }
  }
  return(time)
}