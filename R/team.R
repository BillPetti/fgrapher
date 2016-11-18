#' Something about team :)
#' 
#' @param x Not sure
#' @param hometeam ID for the hometeam
#' @param awayteam ID for the awayteam
#' @export

team = function(x, hometeam, awayteam) {
  if (x <= 9) {
    return(awayteam)
  } else {
    return(hometeam)
  }
}