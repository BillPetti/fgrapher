#' Graph percentiles
#' 
#' @param player XXXXX
#' @export

percentile_graph = function(player, player2 = "", 
                            playertype = "batter", 
                            year = 2016,
                            stats = "standard", 
                            denom = "PA", mindenom = 200,
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
  
  if (player2 != "") {
    if (player2 %in% ids[["Name"]]) {
      id2 = filter(ids, Name == player2)$PlayerId[1]
      name2 = player2
    } else if (player %in% ids[["PlayerId"]]) {
      id2 = player2
      name2 = filter(ids, PlayerId == player2)$Name[1]
    } else {
      stop("Invalid player 2")
    }
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
  
  if ("standard" %in% stats) {
    if (playertype == "batter") {
      statlist = c("OBP","wOBA","K%","BB%")
    } else {
      statlist = c("ERA","FIP","K%","BB%","GB%")
    }
  } else if ("slash" %in% stats) {
    statlist = c("AVG","OBP","SLG")
  } else if ("bip" %in% stats) {
    statlist = c("GB%","FB%","LD%","IFFB%","BABIP")
  } else if ("qoc" %in% stats) {
    statlist = c("Soft%","Med%","Hard%")
  } else if ("pfx" %in% stats) {
    statlist = c("pfxO-Swing%","pfxZ-Swing%","pfxSwing%","pfxO-Contact%","pfxZ-Contact%","pfxContact%","pfxZone%","SwStr%")
  } else if ("eras" %in% stats) {
    statlist = c("ERA","FIP","xFIP","SIERA")
  } else if ("luck" %in% stats) {
    statlist = c("BABIP","HR/FB","LOB%")
  } else {
    statlist = stats
  }
  statlist = factor(statlist, levels = statlist)

  statlist.query = paste0("`",paste0(statlist, collapse = "`, `"),"`")
  query = paste0("select PlayerId as PlayerId, ", statlist.query, " from ", df.name, " where season = ", year, " and type = 0 and ", denom, " >= ", mindenom)
  data = FGQuery(query)
  n = nrow(data)
  
  old.names = c("ValueW","ValueR","HR/FB","pfxvFA")
  new.names = c("WAR","RAR","HR/FB%","Fastball Vel")
  for (i in seq(length(old.names))) {
    if (old.names[i] %in% colnames(data)) {
      colnames(data) = gsub(old.names[i],new.names[i],colnames(data))
      statlist = gsub(old.names[i],new.names[i],statlist)
    }
  }
  
  colnames(data) = gsub("pfx","",colnames(data))
  statlist = gsub("pfx","",statlist)
  
  pctls = c()
  player.stats = filter(data, PlayerId == id)
  if (nrow(player.stats) == 0) {
    stop("Player does not appear in the data")
  }
  for (i in statlist) {
    s = player.stats[[i]]
    l = ecdf(data[[i]])
    p = l(s)
    pctls[length(pctls)+1] = p
  }
  
  a = data.frame(name = name, stat = statlist, pctl = pctls*100)
  
  if (player2 != "") {
    pctls = c()
    player.stats = filter(data, PlayerId == id2)
    if (nrow(player.stats) == 0) {
      stop("Player 2 does not appear in the data")
    }
    for (i in statlist) {
      s = player.stats[[i]]
      p = ecdf(data[[i]])(s)
      pctls[length(pctls)+1] = p
    }
    
    b = data.frame(name = name2, stat = statlist, pctl = pctls*100)
    a = cbind(a,b)
    a = a[,c(2,3,6)]
    a = melt(a, id.vars = "stat")
  }
  
  r = nrow(a)
  if (player2 != "") {r = r/2 + 2}
  
  ang = 0
  h = 0
  if (r >= 8) {ang = 45}
  if (r >= 12) {ang = 90}

  if (player2 == "") {
    title = paste(name,"Percentile Ranks:",year)
  } else {
    title = paste(name,"vs.",name2,"Percentile Ranks:",year)
  }
  subt = paste0("Minimum ",mindenom," ",denom," (",n," Players)")
  if (player2 == "") {
    g = ggplot(a, aes(x=stat,y=pctl)) +
      geom_bar(stat="identity",fill=fg_blue) +
      fgt + theme(panel.grid.major.x = element_line(size=0),
                  axis.text.x = element_text(color="black",family="Lato-Black", angle = ang, vjust = 0)) +
      labs(x="", y="Percentile") +
      ggtitle(bquote(atop(.(title),
                          atop(.(subt)), ""))) +
      scale_y_continuous(breaks=seq(0,100,10)) +
      scale_x_discrete() +
      coord_cartesian(ylim=c(0,100), expand = F) +
      geom_hline(yintercept = 50, color = fg_orange)
  } else {
    g = ggplot(a, aes(x=stat,y=value,group=variable)) +
      geom_bar(stat="identity",position="dodge",aes(fill=variable)) +
      fgt + theme(panel.grid.major.x = element_line(size=0),
                  axis.text.x = element_text(color="black",family="Lato-Black", angle = ang, vjust = 0)) +
      labs(x="", y="Percentile") +
      ggtitle(bquote(atop(.(title), 
                          atop(.(subt)), ""))) +
      scale_y_continuous(breaks=seq(0,100,10)) + 
      scale_x_discrete() +
      coord_cartesian(ylim=c(0,100), expand = F) +
      geom_hline(yintercept = 50, color = fg_orange) +
      scale_fill_manual(values = c(fg_blue, fg_red), labels = c(name,name2), name = "Player")
  }
  
  fname = paste0(gsub(" ","_",gsub("\\.|:","",title)),".png")
  if (save) {
    ggsave(g, filename = fname,
           path = path,
           height = 7,
           width = 8)
  }
  
  return(g)
}