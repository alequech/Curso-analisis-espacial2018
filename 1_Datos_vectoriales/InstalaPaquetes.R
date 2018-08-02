rm(list=ls(all=TRUE))

packages<-c("sf","sp","rgdal","rgl","rgeos","maptools","animation","igraph",
	"plyr","msm","png","fields","tiff","gstat","lattice","latticeExtra","gridExtra",
	"ggplot2","snow","maps","fBasics","data.table","raster","rasterVis","knitr",
	"htmltools","caTools","bitops","mapview","jpeg","gWidgets","leaflet","spData","tidyverse")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
ipak(packages)
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

###############################
#########END OF SCRIPT#########
###############################