% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game_sz_graph.R
\name{game_sz_graph}
\alias{game_sz_graph}
\title{Graph balls/called strikes for a certain game}
\usage{
game_sz_graph(date, hometeam, dh = 1, count = "all", stand = "R','L",
  throws = "R','L", save = FALSE, path = getwd())
}
\arguments{
\item{date}{Date of game Format yyyy-mm-dd.}

\item{hometeam}{The home team in the game. Can be nickname ("Padres"), full name ("San Diego Padres"), or abbreviation ("SD").}

\item{dh}{Doubleheader. 0 or 1 for first game, 2 for second game. Defaults to 1.}

\item{count}{Ball-strike count. Defaults to all. Input x instead of a number to get all counts; e.g. "3-x" for all 3-ball counts}

\item{stand}{Batter handedness. Defaults to both.}

\item{throws}{Pitcher handedness. Defaults to both.}

\item{save}{Whether to save the graph. Defaults to FALSE.}

\item{path}{Where to save the graph. Defaults to the current working directory.}
}
\description{
Graph balls/called strikes for a certain game
}

