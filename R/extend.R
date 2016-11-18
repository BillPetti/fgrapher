extend = function(n) {
  if (nchar(n) == 1) {
    n = paste0("0",n)
  } else {
    n = as.character(n)
  }
  return(n)
}