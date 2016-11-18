#' Send a query to the FanGraphs database
#'
#' @param query A query written in a way that is compatible with MySQL syntax
#' @export
#' @examples
#' \dontrun{FGQuery(query)}

FGQuery = function(query) {
  
  if (!exists("con")) {
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
    
    data = dbGetQuery(con, query)
    dbDisconnect(con)
    return(data)
  }
  else 
    con <- dbConnect(RMySQL::MySQL(), dbname = "tht", username = fg_username, password = fg_password, host = "db.fangraphs.com")
  
    data = dbGetQuery(con, query)
    dbDisconnect(con)
    return(data)
}
