#' Graph the year-to-year correcltions between stats
#' 
#' @param stat1 First statistic to correlate. Must be a metric available in the FanGraphs database.
#' @param stat2 Second statistic to correlate. Defaults to the same statistic as stat1
#' @param playertype Either "batter" or "pitcher". Defaults to "batter".
#' @param startyear First year to generate data from. Defaults to 2000.
#' @param endyear Last year to generate data from. Defaults to 2016.
#' @param minpa Minimum number of plate appearances for a player's season to be included. Defaults to 0.
#' @param year.difference Number of years between the two years being correlated. Must be between 0 and 4. When set to 0, the correlation between 2 stats in the same year will be graphed. Defaults to 1 if stat1 equals stat2 and 0 otherwise.
#' @param line Whether or not to include a trendline. Defaults to TRUE.
#' @param r2 Whether or not to include the r^2 of the fit. Defaults to TRUE. Can be TRUE even if line = FALSE.
#' @param yx If TRUE, applies the trendline y = x instead of the fitted one from the regression. Generally should only be used if stat1 and stat2 are the same. Defaults to TRUE if stat1 equals stat2 and false otherwise.
#' @param highlight.player A player to highlight on the graph. Defaults to no highlight.
#' @param highlight.year.1 The year of the player to highlight. Use the first year -- for example, when trying to highlight a player's 2015-2016 seasons, input 2015. Defaults to 2015.
#' @param text.position Where around the highlighted point to situate the text. Options are "top", "bottom", "left", "right", and "none".
#' @param scale.distance How far away to put the text for the label for the highlight. Over 1 is farther away from the highlighted point; under 1 is closer. Defaults to 1.
#' @param save Whether to save the graph. Defaults to FALSE
#' @param path Where to save the graph. Defaults to the current working directory
#' @export

yty_graph = function(stat1, stat2 = stat1, 
                     playertype = "batter", 
                     startyear = 2000, endyear = 2016, 
                     minpa = 0, year.difference = ifelse(stat1 == stat2, 1, 0),
                     line = TRUE, r2 = TRUE, yx = ifelse(stat1 == stat2, TRUE, FALSE),
                     highlight.player = "", highlight.year.1 = 2015, text.position = "top", scale.distance = 1,
                     save = FALSE, path = getwd()) {
  
  if (playertype == "batter") {
    df.name = "stats_batting_master_pfx"
    pa.name = "PA"
    title.playertype = "Batters"
  } else {
    df.name = "stats_pitching_master_pfx"
    pa.name = "TBF"
    title.playertype = "Pitchers"
  }
  
  statname.query = paste("select * from",df.name, "limit 1")
  all.stats = FGQuery(statname.query)
  all.stats = colnames(all.stats)
  
  if (!stat1 %in% all.stats) {
    stop("stat1 not valid")
  } else if (!stat2 %in% all.stats) {
    stop("stat2 not valid")
  } else if (!playertype %in% c("batter","pitcher")) {
    stop("playertype must be either 'batter' or 'pitcher' (case sensitive)")
  } else if (is.na(as.numeric(startyear)) || nchar(startyear) != 4) {
    stop("Invalid start year")
  } else if (is.na(as.numeric(endyear)) || nchar(endyear) != 4) {
    stop("Invalid end year")
  } else if (!is.numeric(minpa)) {
    stop("minpa must be a number")
  } else if (year.difference < 0 || year.difference > 4) {
    stop("year.difference must be between 0 and 4")
  } else if (!is.logical(save)) {
    stop("Input a logical value for save (TRUE or FALSE)")
  }
  
  pctls.query = paste0("select * from ", df.name, " where ", pa.name, ">=", minpa, " and season between ", startyear, " and ", endyear, " having `", stat1, "` is not null and `", stat2, "` is not null")
  pctls.df = FGQuery(pctls.query)
  
  query = paste0("select a.playerid as PlayerId, a.season as Year1, a.`",stat1,"` as a, b.`",stat2,"` as b, a.",pa.name,"+b.",pa.name," as PA
                 from ",df.name," a join ",df.name," b on a.playerid = b.playerid and a.season + ",year.difference," = b.season and a.type = 0 and b.type = 0
                 and a.season >= ",startyear," and b.season <= ",endyear," and a.",pa.name," >= ",minpa," and b.",pa.name," >= ",minpa,
                 " having a is not null and b is not null")
  data = FGQuery(query)
  
  if (year.difference == 0) {
    data[["PA"]] = data[["PA"]]/2
    eachyear = "."
  } else {
    eachyear = " Each Year."
  }
  
  lm = lm(b ~ a, data = data, weights = PA)
  intercept = lm$coefficients[1]
  slope = lm$coefficients[2]
  rsquared = round(summary(lm)[["r.squared"]],3)
  
  if (yx) {
    intercept = 0
    slope = 1
  }
  
  if (year.difference == 0) {
    title = paste("Correlation between",stat1,"and",stat2,"in the Same Year")
  } else if (year.difference == 1) {
    if (stat1 == stat2) {
      title = paste(stat1,"Year-to-Year Correlation")
    } else {
      title = paste("Year-to-Year Correlation between",stat1,"and",stat2)
    }
  } else if (stat1 == stat2) {
    title = paste0("Correlation between ",stat1," in Year N and in Year N+",year.difference)
  } else {
    title = paste0("Correlation between ",stat1," in Year N and ",stat2," in Year N+",year.difference)
  }
  title = paste0(title,": ",toupper(substring(playertype,1,1)),substring(playertype,2),"s")
  
  scale.name = pa.name
  if (year.difference != 0) {
    scale.name = paste(scale.name,"Total\n(Both Years)")
  }
  
  subt = paste0(startyear," to ",endyear)
  if (minpa != 0) {
    subt = paste0(subt,". Minimum ",minpa," ",pa.name,eachyear)
  }
  
  if (grepl("%",stat1)) {
    scale.x = scale_x_continuous(labels = percent)
  } else {
    scale.x = scale_x_continuous()
  }

  if (grepl("%",stat2)) {
    scale.y = scale_y_continuous(labels = percent)
  } else {
    scale.y = scale_y_continuous()
  }
  
  if (year.difference != 0) {
    label.x = paste(stat1,"Year N")
    label.y = paste0(stat2," Year N+",year.difference)
  } else {
    label.x = stat1
    label.y = stat2
  }
  
  capt = paste("R^2 =",rsquared)
  
  x.bound = c(quantile(pctls.df[[stat1]],.01), quantile(pctls.df[[stat1]],.99))
  y.bound = c(quantile(pctls.df[[stat2]],.01), quantile(pctls.df[[stat2]],.99))
  
  if (line) {
    graphline = geom_abline(slope = slope, intercept = intercept)
  } else {
    graphline = NULL
  }
  
  if (r2) {
    labels = labs(x = label.x, y = label.y, caption = capt)
  } else {
    labels = labs(x = label.x, y = label.y)
  }
  
  if (subt == "") {
    plot.title = ggtitle(title)
  } else {
    plot.title = ggtitle(title, subtitle = subt)
  }
  
  if (highlight.player != "") {
    if (highlight.player %in% ids[["Name"]]) {
      highlight.player.id = filter(ids,Name == highlight.player)$PlayerId[1]
      highlight.name = highlight.player
    } else if (highlight.player %in% ids[["PlayerId"]]) {
      highlight.player.id = highlight.player
      highlight.name = filter(ids,PlayerId == highlight.player)$Name[1]
    } else {
      stop("Invalid player to highlight")
    }
    data[["Highlight.c"]] = 0
    data[["Highlight.a"]] = 0
    r = which(data[["PlayerId"]] == highlight.player.id & data[["Year1"]] == highlight.year.1)
    data[["Highlight.c"]][r] = 1
    data[["Highlight.a"]][r] = 1
    data = data %>% arrange(Highlight.c)
    textdf = filter(data, PlayerId == highlight.player.id & Year1 == highlight.year.1)
    textdf[["PlayerId"]] = highlight.name
    text.lbl = paste(highlight.name,"\n",highlight.year.1,ifelse(year.difference == 0,"",paste("-",highlight.year.1+year.difference)))
  }
  
  if (text.position == "top") {xjust = 1; yjust = 1.1}
  if (text.position == "bottom") {xjust = 1; yjust = .9}
  if (text.position == "right") {xjust = 1.25; yjust = 1.04}
  if (text.position == "left") {xjust = .75; yjust = 1.04}
  xjust = (xjust-1)*scale.distance + 1
  yjust = (yjust-1)*scale.distance + 1
  
  
  if(text.position != "none" & highlight.player != "") {
    highlight.text = geom_text(data = textdf, aes(x = text.x, y = text.y, label = text.lbl))
    text.x = ifelse(textdf[["a"]][1] != 0, textdf[["a"]][1]*xjust, (x.bound[2]-x.bound[1])/10)
    text.y = ifelse(textdf[["b"]][1] != 0, textdf[["b"]][1]*yjust, (y.bound[2]-y.bound[1])/10)
  } else {
    highlight.text = NULL
  }
  
  if (highlight.player != "") {
    point = geom_point(aes(size = PA, color = factor(Highlight.c), alpha = factor(Highlight.a)))
    color.scale = scale_color_manual(values=c(fg_green,fg_blue),guide=FALSE)
    alpha.scale = scale_alpha_manual(values=c(.1,1),guide=FALSE)
  } else {
    point = geom_point(aes(size = PA), color = fg_green, alpha = .1)
    color.scale = NULL
    alpha.scale = NULL
  }

  g = ggplot(data, aes(x=a, y=b)) +
    point +
    labels +
    fgt + theme(panel.background=element_rect(color="white",fill="white"),
                axis.line = element_line(color="black")) +
    plot.title +
    scale_size(name=scale.name) +
    color.scale +
    alpha.scale +
    graphline +
    scale.x + scale.y +
    coord_cartesian(xlim = x.bound, ylim = y.bound, expand = FALSE) +
    highlight.text +
    guides(size = guide_legend(override.aes = list(color = fg_green, alpha = .6)))
  
  fname = paste0(stat1,"_",stat2,"_yty_corr.png")
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}