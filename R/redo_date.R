#' Reorder a date so it's not in dumb yyyy-mm-dd format
#' 
#' @param date A date to be passed to the function for reordering
#' @export
#' @examples 
#' \dontrun{redo_date(2016-05-02)}

redo_date = function(date) {
  return(paste0(month(date),"-",day(date),"-",year(date)))
}