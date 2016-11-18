FGQuery = function(query) {
  con = dbConnect(RMySQL::MySQL(), dbname = "tht", username = fg_username, password = fg_password, host = "db.fangraphs.com")
  data = dbGetQuery(con, query)
  cons = dbListConnections(MySQL())
  for(con in cons) {dbDisconnect(con)}
  return(data)
}
