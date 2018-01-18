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
library(ggmap)
list(canada$NAME_1)



#######---Load this Base Map----########
coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador", "Ontario")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
coastmap2 <- fortify(canada)
coastmap3<- fortify(ca.provinces)
coastmap3$region <-rep("Canada", nrow(coastmap3))
################################################

#Loading datasets in sdmpredictors package package 
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

#Layers in package
tt <- list_layers(datasets, terrestrial=FALSE, marine=TRUE, monthly=TRUE,
            version=NULL)

#There appears to be 362 layers
tt$layer_code[1:362]


##################################################################
#Load dissolved oxygen data (Dissolved oxygen concentration [DO])
#Load Map with the lat/long extent
#Lay dissolved oxygen data over map
dissolved_oxygen <- load_layers(c("BO_dissox"))
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
dissolved_oxygen.crop <- crop(dissolved_oxygen, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(dissolved_oxygen.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Dissolved Oxygen")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
#################################################################



################################################################
#Load dissolved pH data (measure of acidity in the ocean)
#Load Map with the lat/long extent
#Lay pH data over map
pH <- load_layers(c("BO_ph"))
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
pH.crop <- crop(pH, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(pH.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "pH")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
#################################################################



#################################################################
#Load bathymetric slope data ( Bathymetric slope was measured in degrees ranging from 0¤ (flat surface) to 90¤ (vertical slope))
#Load Map with the lat/long extent
#Lay bathymetric slope data over map
bathymetric_slope <- load_layers(c("MS_biogeo06_bathy_slope_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
bathymetric_slope.crop <- crop(bathymetric_slope, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathymetric_slope.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Bathymetric Slope")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


#################################################################
#Load Mean Sea Surface Temperature (Sea surface temperature is the temperature of the water at the ocean surface. This parameter indicates the temperature of the topmost meter of the ocean water column)
#Load Map with the lat/long extent
#Lay Mean Sea Surface Temperature data over map
surfacetemp_mean <- load_layers(c("BO_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
surfacetemp_mean.crop <- crop(surfacetemp_mean, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(surfacetemp_mean.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Mean Surface Temperature")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)

###############################################################
 
#################################################################
#Load Mean Sea Surface Salinity (Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09; Antonov et al. 2010))
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_salinity <- load_layers(c("BO_salinity")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_salinity.crop <- crop(seasurface_salinity, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_salinity.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Mean Surface Salinity (Annual Mean)")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (coldest ice-free month) Data. 
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_temperature_cold <- load_layers(c("MS_biogeo14_sst_min_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_temperature_cold.crop <- crop(seasurface_temperature_cold, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_temperature_cold.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (Coldest Ice-Free Month)")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (warmest ice-free month) Data. 
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_temperature_warm <- load_layers(c("MS_biogeo15_sst_max_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_temperature_warm.crop <- crop(seasurface_temperature_warm, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_temperature_warm.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (Warmest Ice-Free Month)")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)

###############################################################


###############################################################
#Load Max temperature bottom Data. 
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
max_temp_bottom <- load_layers(c("BO2_templtmax_bdmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
max_temp_bottom.crop <- crop(max_temp_bottom, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(max_temp_bottom.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Maximum bottom temperature")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
#################################################################



################################################################################################################################
################################################################################################################################
###########################################----FUTURE LAYERS----################################################################
################################################################################################################################
################################################################################################################################

future_layers <- list_layers_future(datasets=c(), scenario=NA, year=NA,
                   terrestrial=FALSE, marine=TRUE, monthly=TRUE, version=NULL)

##Definitions of all the layer codes
listlayers <- list_layers()
write.csv(listlayers, file="layers.csv")
View(listlayers)


##--NOTE: WorldClim indicates it is terrestrial--##
##Mapping to see how widespread the data is##

###############################################################
#Load Mean Temperature Data: Terrestrial. 
#Load Map with the lat/long extent
#Lay Mean Temperature data over map
annual_mean_temperature <- load_layers(c("WC_bio1_cc26_2050")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
annual_mean_temperature.crop <- crop(annual_mean_temperature, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(annual_mean_temperature.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Annual Mean Temperature 2050")

###############################################################



###############################################################
#Load Sea water temperature (longterm max at mean depth) Data. 
#Load Map with the lat/long extent
#Lay Sea water temperature (longterm max at mean depth) data over map
longterm_max_meandepth <- load_layers(c("BO2_templtmax_bdmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
longterm_max_meandepth.crop <- crop(longterm_max_meandepth, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(longterm_max_meandepth.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea water temperature (long-term max at mean depth)")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################




###############################################################
#Load Sea water temperature (longterm min at mean depth) Data. 
#Load Map with the lat/long extent
#Lay Sea water temperature (longterm min at mean depth) data over map
longterm_min_meandepth <- load_layers(c("BO2_templtmin_bdmax")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
longterm_min_meandepth.crop <- crop(longterm_min_meandepth, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(longterm_min_meandepth.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea water temperature (long-term min at mean depth)")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################



###############################################################
#Load Salinity 2100 Data. 
#Load Map with the lat/long extent
#Lay Salinity 2100 data over map
salinity_future_2100 <- load_layers(c("BO_A1B_2100_salinity")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
salinity_future_2100.crop <- crop(salinity_future_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(salinity_future_2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Future Salinity Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Salinity 2200 Data. 
#Load Map with the lat/long extent
#Lay Salinity 2200 data over map
salinity_future_2200 <- load_layers(c("BO_A1B_2200_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
salinity_future_2200.crop <- crop(salinity_future_2200, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(salinity_future_2200.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Future Salinity Year 2200")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (mean) 2100 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (mean) 2100 data over map
sea_surface_temp_mean_2100 <- load_layers(c("BO_A1B_2100_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_mean_2100.crop <- crop(sea_surface_temp_mean_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_mean_2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (mean) Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (mean) 2200 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (mean) 2200 data over map
sea_surface_temp_mean_2200 <- load_layers(c("BO_A1B_2200_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_mean_2200.crop <- crop(sea_surface_temp_mean_2200, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_mean_2200.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (mean) Year 2200")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (max) 2100 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (max) 2100 data over map
sea_surface_temp_max_2100 <- load_layers(c("BO_A1B_2100_sstmax")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_max_2100.crop <- crop(sea_surface_temp_max_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_max_2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (max) Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (max) 2200 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (max) 2200 data over map
sea_surface_temp_max_2200 <- load_layers(c("BO_A1B_2200_sstmax")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_max_2200.crop <- crop(sea_surface_temp_max_2200, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_max_2200.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (max) Year 2200")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (min) 2100 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (min) 2100 data over map
sea_surface_temp_min_2100 <- load_layers(c("BO_A1B_2100_sstmin")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_min_2100.crop <- crop(sea_surface_temp_min_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_min_2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (min) Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (min) 2200 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (min) 2200 data over map
sea_surface_temp_min_2200 <- load_layers(c("BO_A1B_2200_sstmin")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_min_2200.crop <- crop(sea_surface_temp_min_2200, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_min_2200.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (min) Year 2200")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################



###############################################################
#Load Sea surface temperature (range) 2100 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (range) 2100 data over map
sea_surface_temp_range_2100 <- load_layers(c("BO_A1B_2100_sstrange")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_range_2100.crop <- crop(sea_surface_temp_range_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_range_2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (range) Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (range) 2200 Data. 
#Load Map with the lat/long extent
#Lay Sea surface temperature (range) 2200 data over map
sea_surface_temp_range_2200 <- load_layers(c("BO_A1B_2200_sstrange")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temp_range_2200.crop <- crop(sea_surface_temp_range_2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sea_surface_temp_range_2200.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (range) Year 2200")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################



