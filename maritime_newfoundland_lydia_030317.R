library(dplyr)
library(rfishbase)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)

newdata4 <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata020317_Lydia.csv",stringsAsFactors = F)
head(newdata4)
View(newdata4)



table(newdata4$Invertebrate==1) #65798 OCCASIONS WITH INVERTEBRATES
table(newdata4$Invertebrate==0) #405960 OCCASIONS WITH VETEBRATES
table(is.na(newdata4$Invertebrate)) #25627 OCCASIONS WITH NA (OCCASIONS WHERE PLANTS WERE FOUND OR SPECIFIC SPECIES COULD NOT BE IDENTIFIED)
table(newdata4$species)

#rows that have "Regions" == NA
#newdata4[c(3021,53285,54393,78940,86230,87032,110857,200042,292270,301313,350307,350308),]
##Remove these rows they have no trawl data. They were from the functional database that were never caught
#newdata4 <- newdata4[-c(3021,53285,54393,78940,86230,87032,110857,200042,292270,301313,350307,350308),]
#View(newdata4)

#Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
#This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
#trawl surveys catching fewer fish (I think?)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Create dataframe. Fill it with nothing
#Make for loop to filter by region and year and calculate frequency (percentage)
fulldata <- NULL #empty data
for (i in unique(newdata4$REGION)[!is.na(unique(newdata4$REGION))]){
    for (y in unique(newdata4[!is.na(newdata4$REGION) & newdata4$REGION==i,"year_final"])){

    
    temp <- dplyr::filter(newdata4,REGION==i,year_final==y)
    
    freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
    colnames(freq_obs) <- c("species","frequency")
    
    freq_obs$Region=i
    freq_obs$year=y
    
    freq_obs$freq_stand <- range01(freq_obs$frequency)
    
    fulldata <- rbind(fulldata,freq_obs)
    
    
  } #end of y 'year_final' loop
  
} #end of i 'REGION' loop

fulldata <- fulldata[order(fulldata$Region,fulldata$year,fulldata$freq_stand),]
View(fulldata)




#Create a dummy plot related to a specific year
plotdata1970 <- fulldata[fulldata$year==1970,]
plot(plotdata1970$freq_stand)

#Changing species from factor to character (As a factor it ordered the species alphabetically )
plotdata1970$species <- factor(plotdata1970$species,levels=as.character(plotdata1970$species))

#1970 Plot Species vs Standardized Freq.
plot1970<-ggplot(plotdata1970)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+
  coord_flip()+
  theme_bw();plot1970

##The above comments follow for the remaining years

plotdata1980 <- fulldata[fulldata$year==1980 ,]
plot(plotdata1980 $freq_stand)
plotdata1980 $species <- factor(plotdata1980$species,levels=as.character(plotdata1980$species))
plot1980<-ggplot(plotdata1980)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+
  coord_flip()+
  theme_bw();plot1980


data1990 <- fulldata[fulldata$year==1990,]
plot(plotdata1990$freq_stand)
plotdata1990$species <- factor(plotdata1990$species,levels=as.character(plotdata1990$species))
plot1990<-ggplot(plotdata1990)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+
  coord_flip()+
  theme_bw();plot1990

##After 1995 Newfoundland data gets incorportated. It is now sepated by year and region 
##This makes the plots more legiable

plotdata2000 <- fulldata[fulldata$year==2000 & fulldata$Region=="NEWFOUNDLAND",]
plot(plotdata2000$freq_stand)
plotdata2000$species <- factor(plotdata2000$species,levels=as.character(plotdata2000$species))
plot2000N <- ggplot(plotdata2000)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Newfoundland")+
  coord_flip()+
  theme_bw();plot2000N

plotdata2000 <- fulldata[fulldata$year==2000 & fulldata$Region=="MARITIME",]
plot(plotdata2000$freq_stand)
plotdata2000$species <- factor(plotdata2000$species,levels=as.character(plotdata2000$species))
plot2000M<-ggplot(plotdata2000)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Maritime")+
  coord_flip()+
  theme_bw();plot2000M

grid.arrange(plot2000M,plot2000N)


plotdata2010 <- fulldata[fulldata$year==2010 & fulldata$Region=="NEWFOUNDLAND",]
plot(plotdata2010$freq_stand)
plotdata2010$species <- factor(plotdata2010$species,levels=as.character(plotdata2010$species))
plot2010N <- ggplot(plotdata2010)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Newfoundland")+
  coord_flip()+
  theme_bw();plot2010N

plotdata2010 <- fulldata[fulldata$year==2010 & fulldata$Region=="MARITIME",]
plot(plotdata2010$freq_stand)
plotdata2010$species <- factor(plotdata2010$species,levels=as.character(plotdata2010$species))
plot2010M<-ggplot(plotdata2010)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Maritime")+
  coord_flip()+
  theme_bw();plot2010M

grid.arrange(plot2010M,plot2010N)


###Determining which species were captured more than 1% of the time in 
###trawl sets for each region

##MARITIME##
#combining region, date, lat, long into a tag
newdata4$tag <- paste(newdata4$REGION,paste(newdata4$year_final,newdata4$month_final,newdata4$day_final,sep="-"),
                      newdata4$SLONG,newdata4$SLAT,sep="_")
#filter dataset by anything after 2005 (last decade) and Maritime region
subdata <- filter(newdata4,year_final>2005,REGION=="MARITIME")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than 1% of the time
#118 species
goodspecies <- names(which(table(subdata$species)>Precent_1_stations))
pointdata <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("SLONG","SLAT","year_final")]


##NEWFOUNDLAND##
newdata4$tag <- paste(newdata4$REGION,paste(newdata4$year_final,newdata4$month_final,newdata4$day_final,sep="-"),
                      newdata4$SLONG,newdata4$SLAT,sep="_")
#filter dataset by anything after 2005 (last decade) and Newfoundland region
subdata <- filter(newdata4,year_final>2005,REGION=="NEWFOUNDLAND")
#what is 1% of the unique species
Precent_1_stations_Newfoundland <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than 1% of the time
#118 species
goodspecies_Newfoundland <- names(which(table(subdata$species)>Precent_1_stations_Newfoundland))
pointdata_Newfoundland <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations_Newfoundland)),c("SLONG","SLAT","year_final")]

#list of maritime species
goodspecies
#list of newfoundland species
goodspecies_Newfoundland

##Combining species from each regions
listone2 <- unique(c(goodspecies, goodspecies_Newfoundland))
setdiff(goodspecies_Newfoundland, goodspecies)




