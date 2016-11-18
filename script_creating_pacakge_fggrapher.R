# script for creating fggrapher package

# load required packages

if(is.null("devtools")) {
  require(devtools)
}

if(is.null("roxygen2")) {
  require(devtools)
}

# make sure your functions are saved in the R folder

# document for the first time

document()

# in order to properly document your functions with roxygen2, see this guide: http://r-pkgs.had.co.nz/man.html.

# data files must be saved as .Rdata files using save. You can also use devtools::use_data to save the data properly. They will be saved in the /data folder.

Colors <- read.csv("~/fg_graphs/Graph functions/data/Colors.csv")
denoms_batter <- read.csv("~/fg_graphs/Graph functions/data/denoms_batter.csv")
denoms_pitcher <- read.csv("~/fg_graphs/Graph functions/data/denoms_pitcher.csv")

use_data(Colors)
use_data(denoms_batter)
use_data(denoms_pitcher)

document()

# these data files will automatically load when the package is loaded as long as the LazyData field in the DESCRIPTION file is set to LazyData: false.

# if there are other objects that need to be loaded when the package is loaded they can be placed in an .onLoad function, also placed in the /R folder

# see the zzz.R file I created in the /R folder

# for package dependencies, it is typically best practice to note within each function what functions it needs to import from other packages. When using document(), the functions will be added to the NAMESPACE file and load as the package is loaded. If you want to, for now, you can use the 'Depends" field and that will load all the packages listed when this packge is loaded with library() or require()
