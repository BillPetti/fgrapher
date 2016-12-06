#' Graph called strike zone map by catcher
#' 
#' @param umpire Umpire name or ID. Defaults to "all". WARNING: Leaving as all will make graph take a long time to create.
#' @param startdate Start date. Defaults to beginning of 2015. Format yyyy-mm-dd.
#' @param enddate End date. Defaults to end of 2015. Format yyyy-mm-dd.
#' @param year The year. Overrides startdate and enddate. Can only be one full year -- use startdate and enddate for full control over date range.
#' @param count Ball-strike count. Defaults to all. Input x instead of a number to get all counts; e.g. "3-x" for all 3-ball counts
#' @param stand Batter handedness. Defaults to both.
#' @param save Whether to save the graph. Defaults to FALSE
#' @param path Where to save the graph. Defaults to the current working directory
#' @export

catcher_graph = function(catcher = "all", 
                        startdate = "2016-01-01", enddate = "2016-12-31", year = 0,
                        count = "all", stand = 'R","S',
                        save = FALSE, path = getwd()) {
  
  if (catcher == "all") {
    warning("Plotting all catchers will take a long time.")
  }
  
  if (year != 0) {
    startdate = paste0(year,"-01-01")
    enddate = paste0(year,"-12-31")
  }
  
  if (catcher != "all") {
    if (catcher %in% ids$Name) {
      catcher.id = filter(ids,Name == catcher)$PlayerId[1]
      name = catcher
    } else if (catcher %in% ids$PlayerId) {
      catcher.id = catcher
      name = filter(ids, PlayerId == catcher)$Name[1]
    } else {
      stop("Enter a valid catcher name or ID")
    }
    c.query = paste(" and s.C =",catcher.id)
  } else {c.query = ""}
  

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
  
  query = paste0('select px, pz, case when g.des2 like "Called strike" then 1 else 0 end as CS, s.C as Catcher, g.gamedate 
                 from gd_pitch g 
                 join playerid_lookup idp on idp.mlbamid=g.pitcher 
                 join playerid_lookup idb on idb.mlbamid=g.batter 
                 join splits_pbp s on s.gamedate = g.gamedate and s.hometeamid = g.hometeamid and s.dh = g.dh and s.batterid = idb.playerid and s.pitcherid = idp.playerid and s.linning = g.inning and s.outs = g.o 
                 where g.des2 is not null and g.des2 in ("Ball","Ball in dirt","Called Strike","Pitchout") and px is not null and pz is not null',
                 c.query,ball.query,strike.query," and g.gamedate between \"",startdate,"\" and \"",enddate,"\" and stand in (\"",stand,"\")")
  data = FGQuery(query)
  
  data = with(data, interp(x=px,y=pz,z=CS,duplicate="mean", nx = 100, ny = 100))
  data.melt = melt(data$z, na.rm = TRUE)
  names(data.melt) = c("x", "y", "CS")
  data.melt$px = data$x[data.melt$x]
  data.melt$pz = data$y[data.melt$y]
  data.melt$CS = data.melt$CS + 0.00001
  
  time = datify(startdate, enddate)
  title = ifelse(catcher == "all", paste("Called Strike Map,",time), paste0(name," Called Strike Map"))
  
  subt = ""
  if (count != "all") {
    if (substring(count,1,1) == "x") {
      subt = paste0(strikes,"-strike counts only. ")
    } else if (substring(count,3,3) == "x") {
      subt = paste0(balls,"-ball counts only. ")
    } else {
      subt = paste0(count," counts only. ")
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
    labs(x="", y="",caption=capt) +
    plot.title +
    fgt + theme(panel.background=element_rect(fill="#0000FF"),
                panel.grid.major=element_line(size = 0),
                axis.ticks = element_line(size=0)) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_bot, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_right, y=sz_top, yend = sz_top, color = "black", size = 1) +
    geom_segment(x=sz_left, xend=sz_left, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    geom_segment(x=sz_right, xend=sz_right, y=sz_top, yend = sz_bot, color = "black", size = 1) +
    coord_cartesian(xlim=c(-1.3,1.3),ylim=c(1.3,3.8))
  
  # g = ggplot(data, aes(x=px, y=pz)) +
  #   geom_point(alpha = .5, size = 3, aes(color = CS)) +
  #   fgt +
  #   theme(axis.text=element_text(size=0), 
  #         panel.grid.major = element_line(size=0), 
  #         axis.ticks = element_line(size=0)) +
  #   labs(x="",y="",caption = capt) +
  #   plot.title +
  #   geom_segment(x=sz_left, xend=sz_right, y=sz_bot, yend = sz_bot, color = "black", size = 1) +
  #   geom_segment(x=sz_left, xend=sz_right, y=sz_top, yend = sz_top, color = "black", size = 1) +
  #   geom_segment(x=sz_left, xend=sz_left, y=sz_top, yend = sz_bot, color = "black", size = 1) +
  #   geom_segment(x=sz_right, xend=sz_right, y=sz_top, yend = sz_bot, color = "black", size = 1) +
  #   coord_cartesian(xlim=c(-3,3),ylim=c(0,6)) +
  #   geom_segment(x=-5,xend=5,y=0,yend=0,size=.75,color="black") +
  #   scale_color_manual(values = c("lightblue",fg_red), name = "")
    
  fname = paste0(gsub(", ","_",gsub(" ","_",title)),".png")
  
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
  
  
}