# Reorder a date so it's not in dumb yyyy-mm-dd format
redo_date = function(date) {
  return(month(date),"-",day(date),"-",year(date))
}