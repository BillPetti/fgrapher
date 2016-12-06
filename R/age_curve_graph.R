#' Create a graph for aging curves, based on particular stats
#'
#' @param stat The stat to generate the age curve for. Must be a metric available in the FanGraphs database.
#' @param playertype Either "batter" or "pitcher". Defaults to "batter"
#' @param startyear The first year from which to calculate the aging curve. Defaults to 2000.
#' @param endyear The final year from which to calculate the aging curve. Defaults to 2016.
#' @param save Whether or not to save the graph. Defaults to FALSE.
#' @param path Where to save the graph. Defaults to the current working directory
#' @export
#' @examples
#' \dontrun{age_curve_graph(stat, playertype = "batter", startyear = 2000, endyear = 2016, save = FALSE, path = getwd())}

age_curve_graph = function(stat, 
                           playertype = "batter", 
                           startyear = 2000, endyear = 2016,
                           save = FALSE, path = getwd()) {
  
  if (!playertype %in% c("batter","pitcher")) {
    stop("playertype must be either 'batter' or 'pitcher' (case sensitive)")
  } else if (is.na(as.numeric(startyear))) {
    stop("startyear must be a number")
  } else if (is.na(as.numeric(endyear))) {
    stop("endyear must be a number")
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  }
  
  if (stat == "HR/FB%") {stat = "HR/FB"}
  if (stat == "WAR") {stat = "ValueW"}
  if (stat %in% c("FBvel","vFA")) {stat = "pfxvFA"}
  if (stat %in% c("O-Swing%","Z-Swing%","Swing%","O-Contact%","Z-Contact%","Contact%","Zone%")) {
    stat = paste0("pfx",stat)
  }
  
  
  if (playertype == "batter") {
    df.name = "stats_batting_master_pfx"
    pa.name = "PA"
    title.playertype = "Batters"
    denom_df = denom_batter
  } else {
    df.name = "stats_pitching_master_pfx"
    pa.name = "TBF"
    title.playertype = "Pitchers"
    denom_df = denom_pitcher
  }
  
  if (stat %in% denom_df[["Stat"]] || (substring(stat,1,3) == "pfx" & substring(stat,1,4) != "pfxv" & !substring(stat,nchar(stat)-1) %in% c("-X","-Z"))) {
    if (stat %in% denom_df[["Stat"]]) {
      denom = as.character(filter(denom_df, Stat == stat)[["Denom"]])
    } else {
      denom = "Pitches"
    }
    query = paste0("select Age, sum((StatDiff-LgDiff)*meanpa)/sum(meanpa) as StatDiff from
                   (select b.`", stat, "` - a.`", stat, "` as StatDiff, b.Age, 2/(1/a.",pa.name,"+1/b.",pa.name,") as MeanPA, lg2.lg2 - lg1.lg1 as LgDiff
                   from ", df.name, " a
                   join ", df.name, " b on b.season = a.season + 1 and b.playerid = a.playerid 
                   join (select season, sum(`",stat,"`*",denom,")/sum(",denom,") as lg1 from ", df.name, " where type = 0 group by season) lg1 on lg1.season = a.season
                   join (select season, sum(`",stat,"`*",denom,")/sum(",denom,") as lg2 from ", df.name, " where type = 0 group by season) lg2 on lg2.season = b.season
                   where a.type = 0 and b.type = 0 and a.season between ", startyear, " and ", endyear, " and a.",pa.name," > 0 and b.",pa.name, "> 0 and a.age >= 20) c
                   group by Age")
    capt = "Adjusted for year-to-year changes in league average"
  } else {
    query = paste0("select Age, sum(StatDiff*meanpa)/sum(meanpa) as StatDiff from
                   (select b.`", stat, "` - a.`", stat, "` as StatDiff, b.Age, 2/(1/a.",pa.name,"+1/b.",pa.name,") as MeanPA
                   from ", df.name, " a
                   join ", df.name, " b on b.season = a.season + 1 and b.playerid = a.playerid 
                   where a.type = 0 and b.type = 0 and a.season between ", startyear, " and ", endyear, " and a.",pa.name," > 0 and b.",pa.name, "> 0 and a.age >= 20) c
                   group by Age")
    capt = ""
  }
  data = FGQuery(query)
  
  minage = min(data[["Age"]])
  data = rbind(c(minage-1,0), data)
  data[["StatDiff"]] = cumsum(data[["StatDiff"]])
  
  data = filter(data, Age <= 42)
  
  stat.name = stat
  old.names = c("ValueW","ValueR","HR/FB","pfxvFA")
  new.names = c("WAR","RAR","HR/FB%","Fastball Velocity")
  for (i in seq(length(old.names))) {
    if (old.names[i] == stat.name) {
      stat.name = new.names[i]
    }
  }
  stat.name = gsub("pfx","",stat.name)
  
  if (grepl("%",stat.name)) {
    scale.y = scale_y_continuous(labels = percent)
  } else {
    scale.y = scale_y_continuous()
  }
  
  title = paste(stat.name,"Aging Curve for",title.playertype)
  subt = paste(startyear,"to",endyear)
  
  if (subt == "") {
    plot.title = ggtitle(title)
  } else {
    plot.title = ggtitle(title, subtitle = subt)
  }
  
  g = ggplot(data, aes(x=Age,y=StatDiff)) +
    geom_path(color = fg_green, size = 2, lineend = "round",linejoin = "round") +
    fgt + theme(panel.background=element_rect(color="white",fill="white"),
                axis.line = element_line(color="black")) +
    labs(x = "Age", y = paste("Increase/Decrease in",stat.name), caption = capt) +
    plot.title +
    scale_x_continuous(breaks=20:42) +
    scale.y +
    annotate("point",x=data[1,1],y=data[1,2],size = 5, color = fg_green)
  
  fname = paste0(gsub(" ","_",title),".png")
  if (save) {
    ggsave(filename = fname,
           path = path,
           height = 5,
           width = 8)
  }
  
  return(g)
}