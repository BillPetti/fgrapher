#' Extend a day or month number into a 2-character string
#' 
#' @param n character string?
#' @export
#' @examples 
#' \dontrun{extend(n)}

extend = function(n) {
  if (nchar(n) == 1) {
    n = paste0("0",n)
  } else {
    n = as.character(n)
  }
  return(n)
}