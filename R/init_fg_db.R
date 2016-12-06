#' Initialize a username and password for the FanGraphs database
#'
#' @export
#' @examples
#' \dontrun{FGQuery(query)}

init_fg_db = function() {
  fg_username <- readline("Please provide your username for the FanGraphs database: ")
  assign("fg_username", fg_username, envir = .GlobalEnv)
  
  fg_password <- readline("Please provide your password for the FanGraphs database: ")
  assign("fg_password", fg_password, envir = .GlobalEnv)
  
  con <- dbConnect(RMySQL::MySQL(), dbname = "tht", username = fg_username, password = fg_password, host = "db.fangraphs.com")
  
  assign("con", con, envir = .GlobalEnv)
  
  # player ids
  ids <- FGQuery('select PlayerId, concat(firstname," ",lastname) as Name from player_info')
  assign("ids", ids, envir = .GlobalEnv)
  
  #umpire ids
  uids <- FGQuery('select distinct UName, UmpireId from umpires_daily_batting_full_pfx')
  assign("uids", uids, envir = .GlobalEnv)
  print("Successfully connected to the FanGraphs database!")
}