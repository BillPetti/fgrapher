# Logical - is it in proper format or not?
dateformat = function(x) {
  if (!is.na(as.numeric(substring(x,1,4))) & 
      substring(x,5,5) == "-" & 
      !is.na(as.numeric(substring(x,6,7))) &
      substring(x,8,8) == "-" &
      !is.na(as.numeric(substring(x,9,10))) &
      is.Date(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}