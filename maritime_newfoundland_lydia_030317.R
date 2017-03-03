library(dplyr)
library(rfishbase)
library(lubridate)
library(tidyr)
library(ggplot2)

newdata4 <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata020317_Lydia.csv",stringsAsFactors = F)
head(newdata4)
View(newdata4)



table(newdata4$Invertebrate==1) #65798 OCCASIONS WITH INVERTEBRATES
table(newdata4$Invertebrate==0) #405960 OCCASIONS WITH VETEBRATES
table(is.na(newdata4$Invertebrate)) #25627 OCCASIONS WITH NA (OCCASIONS WHERE PLANTS WERE FOUND OR SPECIFIC SPECIES COULD NOT BE IDENTIFIED)


#rows that have "Regions" == NA
newdata4[c(3021,53285,54393,78940,86230,87032,110857,200042,292270,301313,350307,350308),]
##Remove these rows they have no trawl data. They were from the functional database that were never caught
newdata4 <- newdata4[-c(3021,53285,54393,78940,86230,87032,110857,200042,292270,301313,350307,350308),]
View(newdata4)

#Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
#This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
#trawl surveys catching fewer fish (I think?)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Create dataframe. Fill it with nothing
#Make for loop to filter by region and year and calculate frequency (percentage)
fulldata <- NULL #empty data
for (i in unique(newdata4$REGION)){
for (y in unique(newdata4[newdata4$REGION==i,"year_final"])){

    
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


plotdata1 <- fulldata[fulldata$year==1970,]
plot(plotdata1$freq_stand)

plotdata1$species <- factor(plotdata1$species,levels=as.character(plotdata1$species))


#test species
testplot1<-ggplot(plotdata1)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+
  coord_flip()+
  theme_bw();testplot1




years <- 1

plotdata1 <- fulldata[fulldata$year==1970,]
plot(plotdata1$freq_stand)

plotdata1$species <- factor(plotdata1$species,levels=as.character(plotdata1$species))


#test species
testplot1<-ggplot(plotdata1)+
  geom_point(aes(x=species,y=freq_stand))+
  ylab("Standardized Frequency")+xlab("Species")+
  coord_flip()+
  theme_bw();testplot1








head(fulldata)
str(fulldata)
