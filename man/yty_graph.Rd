% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yty_graph.R
\name{yty_graph}
\alias{yty_graph}
\title{Graph the year-to-year correcltions between stats}
\usage{
yty_graph(stat1, stat2 = stat1, playertype = "batter", startyear = 2000,
  endyear = 2016, minpa = 0, year.difference = ifelse(stat1 == stat2, 1,
  0), line = TRUE, r2 = TRUE, yx = ifelse(stat1 == stat2, TRUE, FALSE),
  highlight.player = "", highlight.year.1 = 2015, text.position = "top",
  scale.distance = 1, save = FALSE, path = getwd())
}
\arguments{
\item{stat1}{First statistic to correlate. Must be a metric available in the FanGraphs database.}

\item{stat2}{Second statistic to correlate. Defaults to the same statistic as stat1}

\item{playertype}{Either "batter" or "pitcher". Defaults to "batter".}

\item{startyear}{First year to generate data from. Defaults to 2000.}

\item{endyear}{Last year to generate data from. Defaults to 2016.}

\item{minpa}{Minimum number of plate appearances for a player's season to be included. Defaults to 0.}

\item{year.difference}{Number of years between the two years being correlated. Must be between 0 and 4. When set to 0, the correlation between 2 stats in the same year will be graphed. Defaults to 1 if stat1 equals stat2 and 0 otherwise.}

\item{line}{Whether or not to include a trendline. Defaults to TRUE.}

\item{r2}{Whether or not to include the r^2 of the fit. Defaults to TRUE. Can be TRUE even if line = FALSE.}

\item{yx}{If TRUE, applies the trendline y = x instead of the fitted one from the regression. Generally should only be used if stat1 and stat2 are the same. Defaults to TRUE if stat1 equals stat2 and false otherwise.}

\item{highlight.player}{A player to highlight on the graph. Defaults to no highlight.}

\item{highlight.year.1}{The year of the player to highlight. Use the first year -- for example, when trying to highlight a player's 2015-2016 seasons, input 2015. Defaults to 2015.}

\item{text.position}{Where around the highlighted point to situate the text. Options are "top", "bottom", "left", "right", and "none".}

\item{scale.distance}{How far away to put the text for the label for the highlight. Over 1 is farther away from the highlighted point; under 1 is closer. Defaults to 1.}

\item{save}{Whether to save the graph. Defaults to FALSE}

\item{path}{Where to save the graph. Defaults to the current working directory}
}
\description{
Graph the year-to-year correcltions between stats
}

