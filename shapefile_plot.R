require(maptools)
library(rgdal)
require(sp)
require(rgeos)
library(ggplot2)
library(tmap)
library(scales)
library(dplyr)
library(plyr)
library(spatialEco)



## Read in shapefile--readOGR command allows most info to be imported in
shapefile<-readOGR(".", layer="tl_2009_us_cbsa")


## Keep only Metro areas of shapefile
shapefile<-shapefile[(grep("Metro", shapefile$NAMELSAD)),]
data<-read.csv("test_data.csv")

## Add group id dummy to shapefile
shapefile@data = data.frame(shapefile@data, data[match(shapefile@data$CBSAFP, data$Id2),])


## plot shapefiel with groups by color
plot(shapefile, col=shapefile@data$dummy)