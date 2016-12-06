#' Graph the called strike zone for an umpire
#' 
#' @param umpire Umpire name or ID. Defaults to "all". WARNING: Leaving as all will make graph take a long time to create.
#' @param startdate Start date. Defaults to beginning of 2015. Format yyyy-mm-dd.
#' @param enddate End date. Defaults to end of 2015. Format yyyy-mm-dd.
#' @param count Ball-strike count. Defaults to all. Input x instead of a number to get all counts; e.g. "3-x" for all 3-ball counts
#' @param stand Batter handedness. Defaults to both.
#' @param save Whether to save the graph. Defaults to FALSE
#' @param path Where to save the graph. Defaults to the current working directory
#' @export

umpire_graph = function(umpire = "all", 
                        startdate = "2015-01-01", enddate = "2015-12-31",
                        count = "all", stand = 'R","S',
                        save = FALSE, path = getwd()) {
  
  if (umpire == "all") {
    warning("Plotting all umpires will take a long time.")
  }
  
  if (umpire != "all") {
    if (umpire %in% uids$UName) {
      umpire.id = filter(uids,UName == umpire)$UmpireId[1]
      name = umpire
    } else if (umpire %in% uids$UmpireId) {
      umpire.id = umpire
      name = filter(uids,UmpireId == umpire)$UName[1]
    } else {
      stop("Enter a valid umpire name or ID")
    }
    u.query = paste(" and umpireid =",umpire.id)
  } else {u.query = ""}
  
  if (!dateformat(startdate)) {
    stop("startdate not valid (must be yyyy-mm-dd)")
  } else if (!dateformat(enddate)) {
    stop("enddate not valid (must be yyyy-mm-dd)")
  } else if (startdate > enddate) {
    stop("Start date is after the end date")
  } else if (!stand %in% c('R","S',"R","S")) {
    stop("Enter a valid handedness for stand (\"R\" or \"S\")\nLeave default for both")
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
  
  query = paste0('select u.UmpireId, u.UName, u.GameDate, case when homeaway = "H" then teamid else oppteamid end as HomeTeamId, g.px, g.pz, case when g.des2 like "ball%" then 0 else 1 end as CS from umpires_daily_batting_full_pfx u 
                join gd_pitch g on g.gamedate = u.gamedate and g.hometeamid = case when u.homeaway = "H" then u.teamid else u.oppteamid end and (g.dh = 0 or (g.dh = 1 and u.daynight = "D") or (g.dh = 2 and u.daynight = "N"))
                where uposition = "home" and (g.des2 like "called%" or g.des2 like "ball%") and px is not null and pz is not null',
                u.query,' and u.gamedate > "', startdate, '" and u.gamedate < "', enddate,'"',ball.query,strike.query," and stand in (\"",stand,"\")")
  data = FGQuery(query)

  data = with(data, interp(x=px,y=pz,z=CS,duplicate="mean", nx = 100, ny = 100))
  data.melt = melt(data$z, na.rm = TRUE)
  names(data.melt) = c("x", "y", "CS")
  data.melt$px = data$x[data.melt$x]
  data.melt$pz = data$y[data.melt$y]
  data.melt$CS = data.melt$CS + 0.00001
  
  time = datify(startdate, enddate)
  title = ifelse(umpire == "all", paste("Called Strike Map,",time), paste0(name," Called Strike Map"))
  
  subt = ""
  if (count != "all") {
    if (substring(count,1,1) == "x") {
      subt = paste0(strikes,"-strike counts only.")
    } else if (substring(count,3,3) == "x") {
      subt = paste0(balls,"-ball counts only.")
    } else {
      subt = paste0(count," counts only.")
    }
  }
  if (stand !='R","S') {
    if (stand == "R") {stand.name = "Righty batters only."}
    if (stand == "L") {stand.name = "Lefty batters only."}
    subt = paste(subt,stand.name)
  }
  
  capt = paste("Catcher's Perspective. Source: PITCHF/x. Timeframe:", time)
  
  if (subt == "") {
    plot.title = ggtitle(title)
  } else {
    plot.title = ggtitle(title, subtitle = subt)
  }
  
  g = ggplot(data = data.melt, aes(x = px, y = pz, z = CS*100)) + 
    stat_contour(geom="polygon", aes(fill=..level..)) + 
    scale_fill_gradient2(low="mediumblue",high="red",mid="#8F0094",midpoint=60,name="Called Strike%") + 
    labs(x="X position (ft.), 0 = middle of the strike zone", y="Height (ft.) off ground",caption=capt) +
    plot.title +
    fgt + theme(panel.background=element_rect(fill="#0000FF"), 
                panel.grid.major=element_line(size = 0)) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_bot, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_top, yend = sz_top, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_left, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_right, xend=sz_right, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    coord_cartesian(xlim=c(-1.3,1.3),ylim=c(1.3,3.8))
  
  fname = paste0(gsub(", ","_",gsub(" ","_",title)),".png")
  
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
  

}