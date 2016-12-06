#' Graph pitcher release points
#' 
#' @param pitcher Pitcher name or FanGraphs playerid.
#' @param startdate Start date. Defaults to beginning of 2016. Format yyyy-mm-dd.
#' @param enddate End date. Defaults to end of 2016. Format yyyy-mm-dd.
#' @param pitchtypes Defaults to all. To include multiple pitch types, create an array: c("Four-seam fastball","Changeup","Breaking ball")
#' @param stand Batter handedness. Defaults to both.
#' @param save Whether to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory.
#' @export
#' @examples 
#' \dontrun{release_graph()}

release_graph = function(pitcher, 
                         startdate = "2016-01-01", enddate = "2016-12-31", 
                         pitchtypes = "all", stand = "R','L",
                         save = FALSE, path = getwd()) {
  
  if (!(pitcher %in% ids$PlayerId) & pitcher != "") {
    pitcher.id = filter(ids,Name == pitcher)$PlayerId[1]
  } else if (pitcher != "") {
    pitcher.id = pitcher
  }
  if (is.na(pitcher.id) & pitcher != "") {
    stop("Invalid pitcher name or ID")
  }
  
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
  }
  
  pitcher.name = filter(ids,PlayerId == pitcher.id)$Name
  
  if ("all" %in% pitchtypes) {
    query.add = ""
  } else {
    pitchlist = paste(pitchtypes, collapse="','")
    query.add = paste0(" having pitch in ('",pitchlist,"')")
  }
  
  query = paste0("select x0, z0, case when pitch_type in ('FF','FA') then 'Four-seam fastball' when pitch_type in ('SI','FT') then 'Two-seam fastball' when pitch_type in ('CU','KC','SC') then 'Curveball' when pitch_type = 'CH' then 'Changeup' when pitch_type = 'FC' then 'Cutter' when pitch_type = 'SL' then 'Slider' when pitch_type = 'FS' then 'Splitter' when pitch_type = 'KN' then 'Knuckleball' when pitch_type = 'EP' then 'Eephus' when pitch_type = 'FO' then 'Forkball' else NULL end as pitch from gd_pitch g join playerid_lookup id on id.mlbamid = g.pitcher where des2 not like 'automatic%' and des2 != 'foul pitchout' and des2 not like 'intent%' and des2 not like 'pi%' and des2 != 'unknown strike' and des2 != 'strike' and des2 is not null and des1 not like '%bunt%' and pitch_type is not null and playerid = ",
                 pitcher.id," and gamedate between '",startdate,"' and '",enddate,"' and stand in ('",stand,"')", query.add)
  pitches = FGQuery(query)
  time = datify(startdate, enddate)
  title = paste(pitcher.name, "Release Points:", time)
  
  subt = ""
  if (stand != "R','L") {
    subt = paste0("Vs. ",stand," only. ")
  }
  if (!"all" %in% pitchtypes) {
    subt = paste0(subt,substring(pitchtypes[1],1,1),
                  substring(tolower(paste0(paste(pitchtypes,collapse=", ")," only.")),2))
  }
  
  if (subt == "") {
    plot.title = ggtitle(title)
  } else {
    plot.title = ggtitle(title, subtitle = subt)
  }
  
  g = ggplot(pitches, aes(x=x0,y=z0)) +
    geom_point(alpha = .5, aes(color = pitch)) +
    fgt + coord_cartesian(xlim=c(-4,4),ylim=c(0,7.5)) +
    scale_color_manual(values=c(fg_green, fg_orange, fg_blue, fg_red, "black", "yellow", "lightblue","purple"),
                       name="Pitch Type") +
    labs(x="Horizontal Release Point", y="Vertical Release Point", caption = "Source: PITCHf/x. Values in feet.") +
    plot.title

  fname = paste0(gsub(":","",gsub("\\.","",gsub(" ","_",gsub(", ","_",title)))),".png")
  if (save) {
    ggsave(filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}