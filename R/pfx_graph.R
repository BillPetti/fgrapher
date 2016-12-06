#' Graph pitchf/x data -- 2 variables on different axes
#' 
#' @param pitcher Pitcher name or FanGraphs playerid.
#' @param x The variable to go on the x-axis.
#' @param y The variable to go on the y-axis.
#' @param startdate Start date. Defaults to beginning of 2016. Format yyyy-mm-dd.
#' @param enddate End date. Defaults to end of 2016. Format yyyy-mm-dd.
#' @param pitchtypes Defaults to all. To include multiple pitch types, create an array: c("Four-seam fastball","Changeup","Breaking ball")
#' @param stand Batter handedness. Defaults to both.
#' @param color.code Whether or not to color code the pitches by pitch type. Defaults to TRUE.
#' @param save Whether to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory.
#' @export
#' @examples 
#' \dontrun{pfx_graph()}

pfx_graph = function(pitcher,
                     x, y,
                     startdate = "2016-01-01", enddate = "2016-12-31", 
                     pitchtypes = "all", stand = "R','L", color.code = TRUE,
                     save = FALSE, path = getwd()) {
  
  if (!(pitcher %in% ids$PlayerId)) {
    pitcher.id = filter(ids,Name == pitcher)$PlayerId[1]
  } else if (pitcher != "") {
    pitcher.id = pitcher
  }
  if (is.na(pitcher.id)) {
    stop("Invalid pitcher name or ID")
  }
  
  pitcher.query = paste(" and playerid =",pitcher.id)

  old.names = c("x0","z0","pfx_x","pfx_z","start_speed","end_speed","ax","ay","az","vx0","vy0","vz0","break_angle","break_length","spin_rate","px","pz","start_speed-end_speed","spin_dir")
  
  if (!dateformat(startdate)) {
    stop("startdate not valid (must be yyyy-mm-dd)")
  } else if (!dateformat(enddate)) {
    stop("enddate not valid (must be yyyy-mm-dd)")
  } else if (startdate > enddate) {
    stop("Start date is after the end date")
  } else if (!stand %in% c("R','L","R","S")) {
    stop("Enter a valid handedness for stand (\"R\" or \"S\")\nLeave default for both")
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  } else if (!x %in% old.names) {
    stop(paste("Invalid stat name for x. Stat must be in the following:\n",paste(old.names,collapse=", ")))
  } else if (!y %in% old.names) {
    stop(paste("Invalid stat name for y. Stat must be in the following:\n",paste(old.names,collapse=", ")))
  }
  
  
  pitcher.name = filter(ids,PlayerId == pitcher.id)$Name
  
  if ("all" %in% pitchtypes) {
    query.add = ""
  } else {
    pitchlist = paste(pitchtypes, collapse="','")
    query.add = paste0(" and pitch in ('",pitchlist,"')")
  }
  
  query = paste0("select ", x, " as x, ", y, " as y, case when pitch_type in ('FF','FA') then 'Four-seam fastball' when pitch_type in ('SI','FT') then 'Two-seam fastball' when pitch_type in ('CU','KC','SC') then 'Curveball' when pitch_type = 'CH' then 'Changeup' when pitch_type = 'FC' then 'Cutter' when pitch_type = 'SL' then 'Slider' when pitch_type = 'FS' then 'Splitter' when pitch_type = 'KN' then 'Knuckleball' when pitch_type = 'EP' then 'Eephus' when pitch_type = 'FO' then 'Forkball' else NULL end as pitch from gd_pitch g join playerid_lookup id on id.mlbamid = g.pitcher where des2 not like 'automatic%' and des2 != 'foul pitchout' and des2 not like 'intent%' and des2 not like 'pi%' and des2 != 'unknown strike' and des2 != 'strike' and des2 is not null and des1 not like '%bunt%' and pitch_type is not null",
                 pitcher.query," and gamedate between '",startdate,"' and '",enddate,"' and stand in ('",stand,"') having x is not null and y is not null", query.add)
  pitches = FGQuery(query)

  new.names = c("Horizontal Release Point","Vertical Release Point","Horizontal Movement","Vertical Movement","Velocity","End Velocity",
                "Horizontal Acceleration","Forwards Acceleration","Vertical Acceleration",
                "Initial Horizontal Velocity","Initial Forwards Velocity","Initial Vertical Velocity",
                "Break Angle", "Break Length", "Spin Rate",
                "Horizontal Location","Vertical Location","Difference between Initial and Final Velocities","Spin Direction")
  new.names.full = c("Horizontal Release Point (Feet from Center of Mound)","Vertical Release Point (Feet from Ground)","Horizontal Movement (Inches)","Vertical Movement (Inches)","Velocity (MPH)","End Velocity (MPH)",
                     "Horizontal Acceleration (Feet/Second^2)","Forwards Acceleration (Feet/Second^2)","Vertical Acceleration (Feet/Second^2)",
                     "Initial Horizontal Velocity (Feet/Second)","Initial Forwards Velocity (Feet/Second)","Initial Vertical Velocity (Feet/Second)",
                     "Break Angle (Degrees)", "Break Length (Inches)", "Spin Rate (RPM)",
                     "Horizontal Location (Feet from Center of Plate)", "Vertical Location (Feet off Ground)", "Difference between Initial and Final Velocities (MPH)", "Spin Direction (Degrees)")
  x.name = x
  y.name = y
  x.name.full = x
  y.name.full = y
  for (i in seq(length(old.names))) {
    if (old.names[i] == x) {
      x.name = new.names[i]
    }
  }
  for (i in seq(length(old.names))) {
    if (old.names[i] == y) {
      y.name = new.names[i]
    }
  }
  for (i in seq(length(old.names))) {
    if (old.names[i] == x) {
      x.name.full = new.names.full[i]
    }
  }
  for (i in seq(length(old.names))) {
    if (old.names[i] == y) {
      y.name.full = new.names.full[i]
    }
  }

  title = paste0(pitcher.name, "'s ", x.name, " vs. ", y.name)
  
  subt = ""
  if (stand != "R','L") {
    subt = paste0("Vs. ",stand," only. ")
  }
  if (!"all" %in% pitchtypes) {
    subt = paste0(subt,substring(pitchtypes[1],1,1),
                  substring(tolower(paste0(paste(pitchtypes,collapse=", ")," only.")),2))
  }
  
  time = datify(startdate, enddate)
  capt = paste("Source: PITCHf/x. Timeframe:",time)
  
  if (color.code) {
    point = geom_point(alpha = .5, aes(color = pitch))
  } else {
    point = geom_point(alpha = .5)
  }
  
  if (subt == "") {
    plot.title = ggtitle(title)
  } else {
    plot.title = ggtitle(title, subtitle = subt)
  }
  
  g = ggplot(pitches, aes(x=x,y=y)) +
    point +
    fgt +
    scale_color_manual(values=c(fg_green, fg_orange, fg_blue, fg_red, "black", "yellow", "lightblue","purple","blue","gray"),
                       name="Pitch Type") +
    labs(x=x.name.full, y=y.name.full, caption = capt) +
    plot.title +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  fname = paste0(gsub(":","",gsub("\\.","",gsub(" ","_",gsub(", ","_",title)))),".png")
  if (save) {
    ggsave(filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}