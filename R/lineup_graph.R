#' Graph projected wRC+ for lineups in a game
#' 
#' @param year The year of the game.
#' @param month The month of the game.
#' @param day The day of the game.
#' @param hometeam Name or abbrevation for the home team.
#' @param awayteam Name or abbrevation for the away team
#' @param dh Doubleheader. Defaults to 1. 1 means either no doubleheader or the first game of a doubleheader; 2 means the second game.
#' @param title Title to place on the graph. Defaults to automatic titling.
#' @param save Whether to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory.
#' @export
#' @examples 
#' \dontrun{lineup_graph(2016, 04, 07, 12, 11, dh = 1, save = FALSE, path = getwd(), title = "")}

lineup_graph = function(year, month, day, 
                        hometeam, awayteam, dh = 1, title = NA,
                        save = FALSE, path = getwd()) {
  
  
  if (!hometeam %in% color[["MLBid"]]) {
    for (row in seq(1,nrow(color))) {
      if (hometeam %in% as.vector(t(color[row,]))) {
        hometeam = color[row,7]
      }
    }
  }
  
  if (!awayteam %in% color[["MLBid"]]) {
    for (row in seq(1,nrow(color))) {
      if (awayteam %in% as.vector(t(color[row,]))) {
        awayteam = color[row,7]
      }
    }
  }
  
  
  if (is.na(as.numeric(year)) || nchar(year) != 4) {
    stop("Invalid year")
  } else if (is.na(as.numeric(month)) || nchar(month) > 2) {
    stop("Invalid month")
  } else if (is.na(as.numeric(day)) || nchar(day) > 2) {
    stop("Invalid day")
  } else if (!hometeam %in% color[["MLBid"]]) {
    stop("Invalid home team")
  } else if (!awayteam %in% color[["MLBid"]]) {
    stop("Invalid away team")
  } else if (!dh %in% c(1,2)) {
    stop("Invalid doubleheader number. Must be 1 or 2.")
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  }
  
  month = extend(month)
  day = extend(day)
  
  home = as.character(filter(color, MLBid == hometeam)$TeamAbbrev[1])
  away = as.character(filter(color, MLBid == awayteam)$TeamAbbrev[1])
  home.full = as.character(filter(color, MLBid == hometeam)$TeamNickname[1])
  away.full = as.character(filter(color, MLBid == awayteam)$TeamNickname[1])
  
  gameid = paste0(year,"_",month,"_",day,"_",awayteam,"mlb_",hometeam,"mlb_",dh)
  url = paste0("http://gd2.mlb.com/components/game/mlb/year_",
               year,"/month_",month,"/day_",day,
               "/gid_",gameid,
               "/players.xml")
  
  data = xmlParse(url)
  xml_data = xmlToList(data)
  
  curyear = year(Sys.time())
  
  query = paste0('select concat(firstname," ",lastname) as Name, a.`wrc+` as wrcp, b.team, id.mlbamid as id, a.type 
                 from stats_batting_master_pfx a 
                 join stats_batting_master_pfx b on b.playerid = a.playerid 
                 join player_info n on n.playerid = a.playerid 
                 join playerid_lookup id on id.playerid = n.playerid 
                 where a.season = ', curyear, ' and b.season = ',curyear,' and (a.type = 1105 or a.type = -108) and b.type = 0 
                 having (id, type) in (select mlbamid as id, max(type) as Type from stats_batting_master_pfx a join playerid_lookup id on id.playerid = a.playerid where a.season = 2016 and a.type in (1105,-108) group by a.playerid) 
                 union all 
                 select concat(firstname," ",lastname) as Name, 0 as wrcp, a.team, id.mlbamid as id, 0 as Type 
                 from stats_batting_master_pfx a 
                 join player_info n on n.playerid = a.playerid 
                 join playerid_lookup id on id.playerid = n.playerid 
                 where n.position = "P" and a.type = 0 and a.season = ', curyear)
  
  wrcp = FGQuery(query)
  
  a.id = c()
  a.ord = c()
  h.id = c()
  h.ord = c()
  
  for (i in seq(1,length(xml_data[1]$team)-1)) {
    if (!is.null(xml_data[1]$team[i]$player)) {
      if (!is.na(xml_data[1]$team[i]$player["bat_order"]) & xml_data[1]$team[i]$player["bat_order"] != 0) {
        a.id[length(a.id)+1] = as.double(xml_data[1]$team[i]$player["id"])
        a.ord[length(a.ord)+1] = as.double(xml_data[1]$team[i]$player["bat_order"])
      }
    }  
  }
  
  for (i in seq(1,length(xml_data[2]$team)-1)) {
    if (!is.null(xml_data[2]$team[i]$player)) {
      if (!is.na(xml_data[2]$team[i]$player["bat_order"]) & xml_data[2]$team[i]$player["bat_order"] != 0) {
        h.id[length(h.id)+1] = as.double(xml_data[2]$team[i]$player["id"])
        h.ord[length(h.ord)+1] = as.double(xml_data[2]$team[i]$player["bat_order"])+9
      }
    }  
  }
  
  a = data.frame(id = a.id, ord = a.ord)
  h = data.frame(id = h.id, ord = h.ord)
  
  df = rbind(a,h)
  df = df %>% left_join(wrcp, by="id")
  df$Team = sapply(df$ord,team,hometeam=home,awayteam=away)
  
  team1 = min(home,away)
  team2 = max(home,away)
  
  team1.main = as.character(filter(color,TeamAbbrev == team1)$Hex[1])
  team1.sec = as.character(filter(color,TeamAbbrev == team1)$Hex[2])
  team2.main = as.character(filter(color,TeamAbbrev == team2)$Hex[1])
  team2.sec = as.character(filter(color,TeamAbbrev == team2)$Hex[2])
  
  if (team1 == home) {
    flip = guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))
  } else {
    flip = guides(fill = guide_legend(reverse = FALSE), color = guide_legend(reverse = FALSE))
  }
  
  max.wrcp = max(df$wrcp)
  
  if (is.na(title)) {
    title = paste(away.full, "at", home.full, "Lineups: Projected wRC+")
    subtitle = paste(month,day,year,sep="-")
  } else {
    title = paste0(title,": Projected wRC+")
  }
  
  g=ggplot(df, aes(x=reorder(Name,-ord),y=wrcp)) + 
    geom_bar(width=1,stat="identity",aes(label=Name,fill=Team,color=Team)) + 
    coord_flip(ylim=c(6,max.wrcp+15)) + 
    scale_y_continuous(breaks=seq(0,200,20)) + 
    labs(y="wRC+",x="", title = title, caption = "FanGraphs depth chart projections") +
    geom_hline(yintercept = 100,color="white") + 
    fgt + theme(panel.grid.major = element_line(size=0),axis.line.x=element_line(size=0),plot.background=element_rect(fill=fg_green,color=fg_green),panel.background=element_rect(fill=fg_green,color=fg_green),legend.background=element_rect(fill=fg_green,color=fg_green),axis.title=element_text(color="white"),axis.text=element_text(color="white"),legend.text=element_text(color="white"),legend.title=element_text(color="white"),axis.ticks=element_line(color="white"),plot.title=element_text(color="white")) + 
    scale_fill_manual(values=c(team1.main,team2.main)) + 
    scale_color_manual(values=c(team1.sec,team2.sec)) + 
    flip 
  
  fname = paste0(gsub(" ","_",title),".png")
  if (save) {
    ggsave(filename = fname,
           path=path, 
           height = 4, 
           width = 8.2)
  }
  return(g)
}