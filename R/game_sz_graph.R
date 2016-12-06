#' Graph balls/called strikes for a certain game
#' 
#' @param date Date of game Format yyyy-mm-dd.
#' @param hometeam The home team in the game. Can be nickname ("Padres"), full name ("San Diego Padres"), or abbreviation ("SD").
#' @param dh Doubleheader. 0 or 1 for first game, 2 for second game. Defaults to 1.
#' @param count Ball-strike count. Defaults to all. Input x instead of a number to get all counts; e.g. "3-x" for all 3-ball counts
#' @param stand Batter handedness. Defaults to both.
#' @param throws Pitcher handedness. Defaults to both.
#' @param save Whether to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory.
#' @export

game_sz_graph = function(date, hometeam, dh = 1, 
                    count = "all", stand = "R','L", throws = "R','L", 
                    save = FALSE, path = getwd()) {
  
  if (!dateformat(date)) {
    stop("startdate not valid (must be yyyy-mm-dd)")
  } else if (!stand %in% c("R','L","R","L")) {
    stop('Enter a valid handedness for stand ("R" or "S")\nLeave default for both')
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  }

  ball.query = ""
  strike.query = ""
  if (count != "all") {
    balls = substring(count,1,1)
    strikes = substring(count,3,3)
    if (balls != "x") {
      ball.query = paste(" and startballs =",balls)
    }
    if (strikes != "x") {
      strike.query = paste(" and startstrikes =",strikes)
    }
  }
  
  hometeam.id = 0
  if (!hometeam %in% color[["TeamId"]]) {
    for (row in seq(1,nrow(color))) {
      if (hometeam %in% as.vector(t(color[row,]))) {
        hometeam.id = color[row,8]
      }
    }
  }
  if (hometeam.id == 0) {stop("Invalid team")}
  
  if (dh <= 1) {
    dh = "0,1"
  } else {
    dh = "2"
  }
  
  query = paste0("select px, pz, hometeamid, awayteamid, case when g.des2 like 'Called strike' then 1 else 0 end as CS from gd_pitch g join playerid_lookup b on b.mlbamid = g.batter join playerid_lookup p on p.mlbamid = g.pitcher where gamedate = '",
                 date,"' and hometeamid = ",hometeam.id,
                 " and dh in (",dh,") and pitch_type not in ('AB','PO','IN','UN') and pitch_type is not null",
                 ball.query,strike.query,
                 " and stand in ('", stand,"')",
                 " and p_throws in ('", throws,"') and des2 in ('Ball','Ball in dirt','Called Strike','Pitchout')")
  pitches = FGQuery(query)

  if (nrow(pitches) == 0) {
    stop("No pitches match given parameters")
  }
  
  awayteam.id = pitches[1,4]
  hometeam.df = filter(color, TeamId == hometeam.id)
  hometeam.name = as.character(hometeam.df[["TeamNickname"]][1])
  awayteam.df = filter(color, TeamId == awayteam.id)
  awayteam.name = as.character(awayteam.df[["TeamNickname"]][1])
  
  sz_top = sz_top
  sz_bot = sz_bot
  
  
  time = datify(date, date)
  title = paste0("Called Strike Map: ",awayteam.name," vs. ",hometeam.name,", ",time)

  subt = ""
  if (stand != "R','L") {
    standdesc = ifelse(stand == "R","Righty","Lefty")
    subt = paste0(subt,standdesc," batters only. ")
  }
  if (throws != "R','L") {
    throwsdesc = ifelse(throws == "R","Righty","Lefty")
    subt = paste0(subt,throwsdesc," pitchers only. ")
  }
  if (count != "all") {
    if (substring(count,1,1) == "x") {
      subt = paste0(subt,strikes,"-strike counts only. ")
    } else if (substring(count,3,3) == "x") {
      subt = paste0(subt,balls,"-ball counts only. ")
    } else {
      subt = paste0(subt,count," counts only. ")
    }
  }

  plot.title = ggtitle(title, subtitle = subt)
  capt = "Catcher's perspective. Source: PITCHf/x"
  
  g = ggplot(pitches, aes(x=px, y=pz)) +
    geom_point(alpha = .6, size = 3, aes(color = factor(CS))) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_bot, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_top, yend = sz_top, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_left, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_right, xend=sz_right, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    fgt +
    coord_cartesian(xlim=c(-2,2),ylim=c(1,5)) +
    geom_segment(x=-5,xend=5,y=0,yend=0,size=.75,color="black") +
    scale_color_manual(values = c("lightblue",fg_red), name = "", labels = c("Ball","Called Strike")) +
    labs(x="",y="",caption = capt) +
    plot.title +
    theme(axis.text=element_text(size=0), 
          panel.grid.major = element_line(size=0), 
          axis.ticks = element_line(size=0),
          panel.background = element_rect(color="white")) +
    guides(colour = guide_legend(override.aes = list(size=4)))

  fname = paste0(gsub("\\.","",gsub(" ","_",gsub(", ","_",title))),".png")
  
  if (save) {
    ggsave(filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}