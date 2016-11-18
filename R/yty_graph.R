#' Graph the year-to-year correcltions between metrics
#' 
#' @param stat1 First statistic to correlate
#' @param stat2 Second statistic to correlate
#' @param playertype Either "batter" or "pitcher". Defaults to "batter".
#' @param startyear Defaults to 2000
#' @param endyear Defaults to 2016
#' @param minpa Minimum number of plate appearances for a player's season to be included. Defaults to 0.
#' @param year.difference Defaults to 1
#' @param save Whether to save the graph. Defaults to FALSE
#' @param path Where to save the graph. Defaults to the current working directory

yty_graph = function(stat1, stat2 = stat1, 
                     playertype = "batter", 
                     startyear = 2000, endyear = 2016, 
                     minpa = 0, year.difference = 1,
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
  
  pctls.query = paste("select * from", df.name, "where", pa.name, ">=", minpa, "and season between", startyear, "and", endyear)
  pctls.df = FGQuery(pctls.query)
  
  query = paste0("select a.`",stat1,"` as a, b.`",stat2,"` as b, a.",pa.name,"+b.",pa.name," as PA
                 from ",df.name," a join ",df.name," b on a.playerid = b.playerid and a.season + ",year.difference," = b.season and a.type = 0 and b.type = 0
                 and a.season >= ",startyear," and b.season <= ",endyear," and a.",pa.name," >= ",minpa," and b.",pa.name," >= ",minpa)
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
  
  subt = paste0(startyear," to ",endyear,".")
  if (minpa != 0) {
    subt = paste0(subt," Minimum ",minpa," ",pa.name,eachyear)
  }
  subt = paste(subt,"R^2 =",rsquared)
  
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
  
  x.bound = c(quantile(pctls.df[[stat1]],.01), quantile(pctls.df[[stat1]],.99))
  y.bound = c(quantile(pctls.df[[stat2]],.01), quantile(pctls.df[[stat2]],.99))
  
  g = ggplot(data, aes(x=a, y=b)) +
    geom_point(color = fg_green, aes(size = PA), alpha = .1) +
    fgt +
    labs(x = label.x, y = label.y) +
    ggtitle(bquote(atop(.(title), 
                        atop(.(subt)), ""))) +
    geom_abline(slope = slope, intercept = intercept) +
    scale_size(name=scale.name) +
    scale.x + scale.y +
    coord_cartesian(xlim = x.bound, ylim = y.bound)
  
  fname = paste0(stat1,"_",stat2,"_yty_corr.png")
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}