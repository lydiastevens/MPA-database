library(sdmpredictors)
library(leaflet)
require(maps)
require(mapdata)
require(mapproj)
require(maptools)
require(plotrix)
require(raster)
require(gdistance)
require(rgdal)
require(sp)
require(ggplot2)
require(marmap)
require(ggrepel)
require(tmap)
require(tmaptools)
require(dplyr)
library(reshape)
list(canada$NAME_1)

??raster

##I Tried reading in the following folder. It didn't work.
tt<-raster("C:/Users/stevensly/Desktop/Bio ORACLE/Present.Surface.Dissolved.oxygen.Max.asc")

#Loading datasets in sdmpredictors package package 
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

#Layers in package
list_layers(datasets, terrestrial=FALSE, marine=TRUE, monthly=TRUE,
            version=NULL)


map <- fortify(map("worldHires", fill=TRUE, plot=FALSE, add=Tr))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
map2 <- fortify(canada)
map3 <- fortify(ca.provinces)
map3$region <-rep("Canada", nrow(coastmap3))


##################################################################
#Load dissolved oxygen data (Dissolved oxygen concentration [DO])
#Load Map with the lat/long extent
#Lay dissolved oxygen data over map
dissolved_oxygen <- load_layers(c("BO_dissox")) 
dissolved_oxygen.crop <- crop(dissolved_oxygen, ca.provinces) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(dissolved_oxygen.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Dissolved Oxygen")


################################################################
#Load dissolved pH data (measure of acidity in the ocean)
#Load Map with the lat/long extent
#Lay pH data over map
pH <- load_layers(c("BO_ph")) 
pH.crop <- crop(pH, ca.provinces) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(pH.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "pH")


#################################################################
#Load bathymetric slope data ( Bathymetric slope was measured in degrees ranging from 0¤ (flat surface) to 90¤ (vertical slope))
#Load Map with the lat/long extent
#Lay bathymetric slope data over map
bathymetric_slope <- load_layers(c("MS_biogeo06_bathy_slope_5m")) 
subsetb <- subset(bathymetric_slope[7,]>0.5)
ne.atlantic.ext <- extent(-75, -50, 39, 55)
bathymetric_slope.crop <- crop(subsetb, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathymetric_slope.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Bathymetric Slope")



###############################################################



 





















