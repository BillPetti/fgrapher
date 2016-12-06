#' Graph a histogram of all players for a given stat; highlight selected player.
#' 
#' @param player The player to examine.
#' @param stat The stat to examine.
#' @param playertype Either "batter" or "pitcher". Defaults to batter.
#' @param year The year to graph.
#' @param denom The type of denominator to set the minimum. Defaults to plate appearances/total batters faced depending on the player type. Other options are "Pitches", "AB" (batters), "BIPcount", "GB", "FB", "LD"
#' @param mindenom The minimum number of whatever the denominator is to create the set of players to compare to. Default is 200 (plate appearances).
#' @param bins The number of bins on the histogram.
#' @param save Whether to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory.
#' @export

hist_graph = function(player, stat, 
                      playertype = "batter", 
                      year = 2016,
                      denom = "PA", mindenom = 200,
                      bins = 10,
                      save = FALSE, path = getwd()) {
  
  if (player %in% ids[["Name"]]) {
    id = filter(ids, Name == player)$PlayerId[1]
    name = player
  } else if (player %in% ids[["PlayerId"]]) {
    id = player
    name = filter(ids, PlayerId == player)$Name[1]
  } else {
    stop("Invalid player")
  }
  
  if (is.na(as.numeric(year)) || nchar(year) != 4) {
    stop("Invalid year")
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  }
  
  if (playertype == "batter") {
    df.name = "stats_batting_master_pfx"
    pa.name = "PA"
    title.playertype = "Batters"
  } else {
    df.name = "stats_pitching_master_pfx"
    pa.name = "TBF"
    title.playertype = "Pitchers"
  }
  
  if (denom == "PA") {
    denom = pa.name
  }
  
  query = paste0("select PlayerId as PlayerId, `", stat, "`, ", denom, ", case when playerid = ", id, " then 1 else 0 end as flag from ", df.name, " where season = ", year, " and type = 0 and ", denom, " >= ", mindenom)
  data = FGQuery(query)
  n = nrow(data)
  
  if (!id %in% data[["PlayerId"]]) {stop("Player not found in data")}

  old.names = c("ValueW","ValueR","HR/FB","pfxvFA")
  new.names = c("WAR","RAR","HR/FB%","Fastball Vel")
  for (i in seq(length(old.names))) {
    if (old.names[i] == colnames(data)[2]) {
      colnames(data)[2] = new.names[i]
    }
  }
  colnames(data) = gsub("pfx","",colnames(data))
  stat.name = colnames(data)[2]
  colnames(data)[2] = "stat"
  
  s = filter(data,flag == 1)[["stat"]][1]
  max.s = max(data[["stat"]])
  min.s = min(data[["stat"]])
  range.s = max.s - min.s
  bin.range = range.s / bins
  range.list = c(min.s, rep(bin.range, bins))
  range.list = cumsum(range.list)
  for (i in range.list) {
    if (s >= i) {
      bin.highlight = which(range.list == i)
    }
  }
  bin.colors = rep(fg_green,bins)
  bin.colors = replace(bin.colors,bin.highlight,fg_orange)
  
  if (grepl("%",stat)) {
    scale.x = scale_x_continuous(labels = percent)
  } else {
    scale.x = scale_x_continuous()
  }

  title = paste0(name,"'s ",stat.name, " Rank: ",year)
  capt = paste0("Minimum ",mindenom," ",denom," (",n," Players)")
  
  g = ggplot(data, aes(x=stat)) +
    geom_histogram(color="black",fill=bin.colors,bins=bins) +
    fgt + theme(panel.grid.major.x = element_line(size=0),
                panel.background=element_rect(color="white",fill="white"),
                axis.line = element_line(color="black")) +
    labs(x=stat.name, y="Players", caption = capt) +
    expand_limits(y = 0) +
    scale.x +
    ggtitle(title)

  fname = paste0(gsub(" ","_",gsub("\\.|:","",title)),".png")
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}