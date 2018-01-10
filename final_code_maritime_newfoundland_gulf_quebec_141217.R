library(lubridate)
library(dplyr)
library(rfishbase)
library(taxize)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(reshape)

#load datasets from different regions for analysis
maritimenewfoundland <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata170317_Lydia3.csv",stringsAsFactors = F)
quebec<-read.csv("C:/Users/StevensLy/Documents/Database/Data/Archived/Quebec Region RV DFO Survey_SLGO.csv",stringsAsFactors = F)
gulf<-read.csv("C:/Users/StevensLy/Documents/Database/Data/Archived/Gulf Region RV DFO Survey.csv",stringsAsFactors = F)

#subset maritime and newfoundland data into separate regions
maritime<-maritimenewfoundland[maritimenewfoundland$REGION=="MARITIME",]
newfoundland<-maritimenewfoundland[maritimenewfoundland$REGION=="NEWFOUNDLAND",]

#we now have four regions
#quebec, gulf, maritime, newfoundland

head(quebec)
head(gulf)
head(newfoundland)
head(maritime)

#######MARITIME#######
min(maritime$year_final) #1970
max(maritime$year_final) #2016
unique(maritime$species) #395 unique species
table(maritime$species)
max(table(maritime$species)) #Hippoglossoides platessoides occurred the most frequently (10067 occasions)
View(table(maritime$species))

#######NEWFOUNDLAND#######
min(newfoundland$year_final) #1995
max(newfoundland$year_final) #2013
unique(newfoundland$species) #134 unique species
table(newfoundland$species)
max(table(newfoundland$species)) #Hippoglossoides platessoides occurred the most frequently (14180 occasions)
View(table(newfoundland$species))

#######QUEBEC#######
min(quebec$Date) #1995
max(quebec$Date) #2015
unique(quebec$Latin.name) #103 unique species
table(quebec$Latin.name)
max(table(quebec$Latin.name)) #Gadus Morhua occurred the most frequently (2943 occasions)
View(table(quebec$Latin.name))

#######GULF#######
min(gulf$year) #1971
max(gulf$year) #2016



#Cleaning Dates in quebec dataset
Date<-as.POSIXct(quebec$Date, format="%Y-%m-%d", tz ="UTC")
quebec$year_final <- year(Date)
quebec$month_final <- month(Date)
quebec$day_final <- day(Date)

#Fixing columns
quebec$region<-"QUEBEC"
colnames(quebec)[7]<-paste("species")
head(quebec) 

unique(quebec$species)

##Running Classify function for Gulf Species
##Create new colum for species that are going to be taxize
quebec$newsciname = quebec$species
quebec$newsciname<-tolower(quebec$newsciname)
head(quebec)
unique(quebec$newsciname)


quebec$newsciname <- gsub("ammodytes sp.","ammodytes",quebec$newsciname)
SpeciesList <- unique(quebec$newsciname)

#used an old function to expand species name into taxonomic groups 
# source("C:/Users/StevensLy/Documents/Database/Code/MPA database/ClassifyFunction.R")
# SpeciesList <- unique(quebec$newsciname)
# outlist <- lapply(SpeciesList[which(SpeciesList==""):length(SpeciesList)],FUN=Classify)
# outlist <- lapply((SpeciesList), FUN=Classify)
# taxInfo <- do.call("rbind", outlist)
#write.csv(taxInfo, file='C:/Users/StevensLy/Documents/Database/Data/taxinfoquebec.csv')
taxinfo_quebec <- read.csv("C:/Users/StevensLy/Documents/Database/Data/taxinfoquebec.csv",stringsAsFactors = F)
names(taxinfo_quebec)


# quebec$phylum <- NA
# quebec$subphylum <- NA
# quebec$class <- NA
# quebec$order <- NA
# quebec$family <- NA
# quebec$genus <- NA
# quebec$speciesname <- NA
# names(quebec)

#merging column data from taxinfo_quebec to quebec.
quebec2 <- merge(quebec[,-grep("species",x = names(quebec))], taxinfo_quebec, by="newsciname")
quebec2$X <- NULL
quebec2$Format <- NULL
quebec2$Biomass <- NULL
quebec2$Density <- NULL
quebec2$Coverage <- NULL
names(quebec2)
names(quebec2)[4] <- paste("longitude")
names(quebec2)[5] <- paste("latitude")
head(quebec2)  

#######GULF#######
min(gulf$year) #1971
max(gulf$year) #2016

##Replace Column Name with Species
names(gulf)

speciescode<-read.csv("C:/Users/StevensLy/Documents/Database/Data/Species Code Info.csv",stringsAsFactors = F)
View(speciescode)

#Match species names from "speciescode data" to the codes in gulf data
colnames(gulf) <- gsub("X_","",colnames(gulf))
gulf2 <- gulf #so the code doesn't bug out

for(i in 16:103){
  colnames(gulf)[i]=speciescode[speciescode$CODE==as.numeric(colnames(gulf2)[i]),"SPEC"]
}

head(gulf)

#Subsetted data
t<-gulf[1:15]
tt<-gulf[16:103]
#melted so species weren't individual columns, but rather one column (wide to long)
meltgulf<-melt(tt)
totalgulf<-data.frame(t, meltgulf)
colnames(totalgulf)[16]<-paste("species")
colnames(totalgulf)[1]<-paste("year_final")
colnames(totalgulf)[6]<-paste("month_final")
colnames(totalgulf)[7]<-paste("day_final")
totalgulf$region<-"GULF"
head(totalgulf)

unique(totalgulf$species) #88 unique species


#changed species from upper case to lower case
totalgulf2$species<-tolower(totalgulf2$species)
head(totalgulf2)


#remove rows that had NA or 0 listed for weight
totalgulf2 <- totalgulf[is.na(totalgulf$value) == FALSE,]
totalgulf2 <- totalgulf[totalgulf$value>0,]
length(table(totalgulf2$species))

##Running Classify function for Gulf Species
##Create new colum for species that are going to be taxized
totalgulf2$newsciname = totalgulf2$species
totalgulf2$newsciname<-tolower(totalgulf2$newsciname)
head(totalgulf2)
unique(totalgulf2$newsciname)

#removed taxominc groups at the end of each species
totalgulf2$newsciname <- gsub("sebastes sp.","sebastes",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("lycodes sp.","lycodes",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("paralepididae f.","paralepididae",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("tunicata s.p.","tunicata",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("decapoda o.","decapoda",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("hyas sp.","hyas",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("paguroidea s.f.","paguroidea",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("aphrodita sp.","aphrodita",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("gastropoda o.","gastropoda",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("bivalvia c.","bivalvia",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("pectinidae f.","pectinidae",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("octopoda o.","octopoda",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("asteroidea s.c.","asteroidea",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("ophiuroidea s.c.","ophiuroidea",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("gorgonocephalidae,asteronychidae f.","euryalida",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("strongylocentrotus sp.","strongylocentrotus",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("clypeasteroida o.","clypeasteroida",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("holothuroidea c.","holothuroidea",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("clypeasteroida o.","clypeasteroida",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("anthozoa c.","anthozoa",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("scyphozoa c.","scyphozoa",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("porifera p.","porifera",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("liparis sp.","liparidae",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("gymnelis viridis","gymnelus viridis",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("lumpenus lumpretaeformis","lumpenus lampretaeformis",totalgulf2$newsciname)
totalgulf2$newsciname <- gsub("enchelyopus cimbrius","enchelyopus cimbrius",totalgulf2$newsciname)

totalgulf2 <- totalgulf2[is.na(totalgulf2$newsciname) == FALSE,]
SpeciesList <- unique(totalgulf2$newsciname)

#used an old function to expand species name into taxonomic groups 
# source("C:/Users/StevensLy/Documents/Database/Code/MPA database/ClassifyFunction.R")
# SpeciesList <- unique(totalgulf2$newsciname)
# outlist <- lapply(SpeciesList[which(SpeciesList=="gadus morhua"):length(SpeciesList)],FUN=Classify)
# outlist <- lapply((SpeciesList), FUN=Classify)
# taxInfo <- do.call("rbind", outlist)
#write.csv(taxInfo, file='C:/Users/StevensLy/Documents/Database/Data/taxinfogulf.csv')
taxinfo_gulf <- read.csv("C:/Users/StevensLy/Documents/Database/Data/taxinfogulf.csv",stringsAsFactors = F)
names(taxinfo_gulf)

#merging column data from taxinfo_gulf to gulf.
totalgulf3 <- merge(totalgulf2[,-grep("species",x = names(totalgulf2))], taxinfo_gulf, by="newsciname")

head(totalgulf3)
totalgulf3$X <- NULL
names(totalgulf3)


# #This is one way to change the column names
# colnames(gulf)[16:18]<-c("GADUS MORHUA","MELANOGRAMMUS AEGLEFINUS","UROPHYCIS TENUIS")
# colnames(gulf)[19:21]<-c("MERLUCCIUS BILINEARIS","POLLACHIUS VIRENS","SEBASTES SP.")
# colnames(gulf)[22:24]<-c("HIPPOGLOSSUS HIPPOGLOSSUS","REINHARDTIUS HIPPOGLOSSOIDES","HIPPOGLOSSOIDES PLATESSOIDES")
# colnames(gulf)[25:27]<-c("GLYPTOCEPHALUS CYNOGLOSSUS","LIMANDA FERRUGINEA","PSEUDOPLEURONECTES AMERICANUS")
# colnames(gulf)[28:30]<-c("ANARHICHAS LUPUS","ANARHICHAS MINOR","ANARHICHAS DENTICULATUS")
# colnames(gulf)[31:33]<-c("CLUPEA HARENGUS","ALOSA SAPIDISSIMA","ALOSA PSEUDOHARENGUS")
# colnames(gulf)[34:36]<-c("OSMERUS MORDAX","MALLOTUS VILLOSUS","SCOMBER SCOMBRUS")
# colnames(gulf)[37:39]<-c("BOREOGADUS SAIDA","UROPHYCIS CHESTERI","ENCHELYOPUS CIMBRIUS")
# colnames(gulf)[40:42]<-c("GADUS OGAC","TAUTOGOLABRUS ADSPERSUS","SCOPHTHALMUS AQUOSUS")
# colnames(gulf)[43:45]<-c("AMBLYRAJA RADIATA","MALACORAJA SENTA","LEUCORAJA OCELLATA")
# colnames(gulf)[46:48]<-c("SQUALUS ACANTHIAS","CENTROSCYLLIUM FABRICII","MYXINE GLUTINOSA")
# colnames(gulf)[49:51]<-c("MYOXOCEPHALUS OCTODECEMSPINOSUS","MYOXOCEPHALUS SCORPIU","GYMNOCANTHUS TRICUSPIS")
# colnames(gulf)[52:54]<-c("TRIGLOPS MURRAYI","ARTEDIELLUS UNCINATUS","COTTUNCULUS MICROPS")
# colnames(gulf)[55:57]<-c("ICELUS SPATULA","MYOXOCEPHALUS SCORPIOIDES","HEMITRIPTERUS AMERICANUS")
# colnames(gulf)[58:60]<-c("ASPIDOPHOROIDES MONOPTERYGIUS","LEPTAGONUS DECAGONUS","GASTEROSTEUS ACULEATUS")
# colnames(gulf)[61:63]<-c("LOPHIUS AMERICANUS","NEZUMIA BAIRDII","LIPARIS SP.")
# colnames(gulf)[64:66]<-c("CYCLOPTERUS LUMPUS","EUMICROTREMUS SPINOSUS","AMMODYTES DUBIUS")
# colnames(gulf)[67:69]<-c("GYMNELIS VIRIDIS","LUMPENUS LUMPRETAEFORMIS","LUMPENUS MACULATUS")
# colnames(gulf)[70:72]<-c("STICHAEUS PUNCTATUS","ULVARIA SUBBIFURCATA","EUMESOGRAMMUS PRAECISUS")
# colnames(gulf)[73:75]<-c("CRYPTACANTHODES MACULATUS","LUMPENUS FABRICII","ZOARCES AMERICANUS.")
# colnames(gulf)[76:78]<-c("LYCODES SP.","MELANOSTIGMA ATLANTICUM","PEPRILUS TRIACANTHUS")
# colnames(gulf)[79:81]<-c("PARALEPIDIDAE F.","TUNICATA S.P.","DECAPODA O.")
# colnames(gulf)[82:84]<-c("CANCER IRRORATUS","HYAS SP.","LITHODES MAJA")
# colnames(gulf)[85:87]<-c("CHIONOECETES OPILIO","HOMARUS AMERICANUS","PAGUROIDEA S.F.")
# colnames(gulf)[88:90]<-c("APHRODITA SP.","GASTROPODA O.","BIVALVIA C.")
# colnames(gulf)[91:93]<-c("PECTINIDAE F.","ILLEX ILLECEBROSUS","OCTOPODA O.")
# colnames(gulf)[94:96]<-c("ASTEROIDEA S.C.","OPHIUROIDEA S.C.","GORGONOCEPHALIDAE,ASTERONYCHIDAE F")
# colnames(gulf)[97:99]<-c("STRONGYLOCENTROTUS SP.","CLYPEASTEROIDA O.","HOLOTHUROIDEA C.")
# colnames(gulf)[100:103]<-c("ANTHOZOA C.","PENNATULACEA","SCYPHOZOA C.","PORIFERA P.")

#used a function to keep matching columns from one dataset and keep non matching columns
rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}

#combined gulf data with quebec data. Columns that were the same matched together
#columns that were different from each regions were also added and filled with NAs where there was not data
gulfquebec <- rbind.all.columns(totalgulf3, quebec2)
head(gulfquebec)

#clean up gulfquebec data. Remove uncessary columns
names(gulfquebec)
gulfquebec$NAME_ <- NULL
gulfquebec$Owner.Institution <- NULL

#next step is to combine gulfquebecdata with maritimenewfounald data
names(gulfquebec)
names(maritimenewfoundland)


View(maritimenewfoundland)
View(gulfquebec)
#write.csv(gulfquebec, file='C:/Users/StevensLy/Documents/Database/Data/gulfquebec.csv')
#write.csv(maritimenewfoundland, file='C:/Users/StevensLy/Documents/Database/Data/maritimenewfoundland.csv')

#determining what species are found in gulf and quebec regions but not in maritime newfoundland
setdiff(gulfquebec$species, maritimenewfoundland$species)
#update species where there names have changed
gulfquebec$species <- gsub("Urophycis chesteri","Phycis chesteri",gulfquebec$species)
gulfquebec$species <- gsub("Lumpenus maculatus","Leptoclinus maculatus",gulfquebec$species)
setdiff(gulfquebec$species, maritimenewfoundland$species)
setdiff(gulfquebec$genus, maritimenewfoundland$genus)
setdiff(gulfquebec$family, maritimenewfoundland$family)
setdiff(gulfquebec$order, maritimenewfoundland$order)
setdiff(gulfquebec$class, maritimenewfoundland$class)
setdiff(gulfquebec$subphylum, maritimenewfoundland$subphylum)
setdiff(gulfquebec$phylum, maritimenewfoundland$phylum)



#making column names more consistent. Since other regions only species lat/long
#take start lat/start long and name it to just be lat/long
names(maritimenewfoundland)[15] <- paste("longitude")
names(maritimenewfoundland)[16] <- paste("latitude")
names(maritimenewfoundland)[9] <- paste("set")
names(maritimenewfoundland)[3] <- paste("strat")
names(maritimenewfoundland)[7] <- paste("region")
names(maritimenewfoundland)[19] <- paste("depth")

View(gulfquebec)

allregions <- rbind.all.columns(gulfquebec,maritimenewfoundland)
head(allregions)
names(allregions)
allregions$X <- NULL

##fixing mistakes
allregions$species <- gsub("Spirontocarus spinus","Spirontocaris spinus",allregions$species)
allregions$species <- gsub("Raja fyllae","Rajella fyllae",allregions$species)
allregions$species <- gsub("Ulcina olrikii","Aspidophoroides olrikii",allregions$species)
allregions$genus <- gsub("Ulcina","Aspidophoroides",allregions$genus)
allregions$newsciname <- gsub("ulcina olrikii","aspidophoroides olrikii",allregions$newsciname)
allregions$class <- gsub("Teleostei","Actinopterygii",allregions$class)
# allregions$species <- gsub("Gadus ogac","Gadus macrocephalus",allregions$species)
# allregions$newsciname <- gsub("gadus ogac","gadus macrocephalus",allregions$newsciname)
View(allregions)

#creating a new database for functional traits only of unique species. I thought this was
#useful, but I don't think I need it.
# functionaltraits <-allregions[,c(17:24,52:134)] 
# unique(functionaltraits$species) 
# names(functionaltraits)

#Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
#This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
#trawl surveys catching fewer fish (I think?)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Create dataframe. Fill it with nothing
#Make for loop to filter by region and year and calculate frequency (percentage)
freqinfo <- NULL #empty data
for (i in unique(allregions$region)[!is.na(unique(allregions$region))]){
  for (y in unique(allregions[!is.na(allregions$region) & allregions$region==i,"year_final"])){
    
    
    temp <- dplyr::filter(allregions,region==i,year_final==y)
    
    freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
    colnames(freq_obs) <- c("species","frequency")
    
    freq_obs$region=i
    freq_obs$year=y
    
    freq_obs$freq_stand <- range01(freq_obs$frequency)
    
    freqinfo <- rbind(freqinfo,freq_obs)
    
    
  } #end of y 'year_final' loop
  
} #end of i 'region' loop

freqinfo <- freqinfo[order(freqinfo$region,freqinfo$year,freqinfo$freq_stand),]
View(freqinfo)

###Determining which species were captured more than ___% of the time in 
###trawl sets for each region

##MARITIME##
#combining region, date, lat, long into a tag
allregions$tag <- paste(allregions$region,paste(allregions$year_final,allregions$month_final,allregions$day_final,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(allregions,year_final>2005,region=="MARITIME")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_maritime <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_maritime <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_maritime

#sensitivity analysis

x <- NULL
for (i in seq(0.01,0.75,0.01)){
  cutoff <- floor(length(unique(subdata$tag))*i)
  x <- c(x,length(names(which(table(subdata$species)>cutoff))))
}

xplotdata <- data.frame(cutoff=seq(0.01,0.75,0.01),numspecies=x)

ggplot2::ggplot(xplotdata,aes(x=cutoff,y=numspecies))+geom_point()+theme_bw()

##NEWFOUNDLAND##
#combining region, date, lat, long into a tag
allregions$tag <- paste(allregions$region,paste(allregions$year_final,allregions$month_final,allregions$day_final,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Newfoundland region
subdata <- filter(allregions,year_final>2005,region=="NEWFOUNDLAND")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_newfoundland <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_newfoundland <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_newfoundland


##GULF##
allregions$tag <- paste(allregions$region,paste(allregions$year_final,allregions$month_final,allregions$day_final,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- filter(allregions,year_final>2005,region=="GULF")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_gulf <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_gulf <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_gulf


##QUEBEC##
allregions$tag <- paste(allregions$region,paste(allregions$year_final,allregions$month_final,allregions$day_final,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Quebec Region
subdata <- filter(allregions,year_final>2005,region=="QUEBEC")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_quebec <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_quebec <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_quebec


##Finding differences between regions
#There are 184 unique species in each region more than 1% of the time (this does not include animals 
#captured that only have taxonomic information higher than species level)
known <- unique(c(goodspecies_newfoundland, goodspecies_maritime))
unknown <- unique(c(goodspecies_gulf, goodspecies_quebec))
diff <- setdiff(known, unknown)
diff


View(allregions)
complete <- data.frame(unique(c(goodspecies_maritime,goodspecies_newfoundland,goodspecies_gulf,goodspecies_quebec)))
names(complete) <- paste("species")
complete


#merge complete with functional traits by species
df <- merge(complete,functionaltraits, by="species")
head(df)
head(unique(df$species))
dflong <- unique(df[df$species %in% complete$species,])
head(dflong)
dflong$region <- NULL
functionaldatabase <- dflong[!duplicated(dflong$species), ]
##read in functionaldatabase 
functionaldatabase<-read.csv("C:/Users/StevensLy/Documents/Database/Data/functionaldatabase_271117.csv",stringsAsFactors = F)
head(functionaldatabase)
##fixing mistakes
functionaldatabase$species <- gsub("Spirontocarus spinus","Spirontocaris spinus",functionaldatabase$species)
functionaldatabase$species <- gsub("Raja fyllae","Rajella fyllae",functionaldatabase$species)
functionaldatabase$species <- gsub("Ulcina olrikii","Aspidophoroides olrikii",functionaldatabase$species)
functionaldatabase$genus <- gsub("Ulcina","Aspidophoroides",functionaldatabase$genus)
functionaldatabase$class <- gsub("Teleostei","Actinopterygii",functionaldatabase$class)

#functionaldatabase$species <- gsub("Gadus ogac","Gadus macrocephalus",functionaldatabase$species)
#The functional database didn't have all of the information in it so I did some work in excel to complete
#missing information. For example, most invertebrate species were excluded. 
#Re-writing the functionaldatabase will remove any of the work that was completed in excel!!!


##Making a table with functional group info
functional_groups <- data.frame(functionaldatabase$species, functionaldatabase$spec_code, functionaldatabase$resilience.category, 
           functionaldatabase$salinity.tolerance,functionaldatabase$depth.category, functionaldatabase$size.category, 
           functionaldatabase$habitat.category, functionaldatabase$trophic_level, functionaldatabase$fecundity.min, 
           functionaldatabase$fecundity.max, functionaldatabase$feeding.guild.category)

head(functional_groups)
write.csv(functional_groups, file='C:/Users/StevensLy/Documents/Database/Data/functional_groups.csv')



##Get mean length for all unique species in functionaldatabase

alllength <- NULL
for (i in unique(functionaldatabase$species)){
  alllength <- rbind(alllength, data.frame(species=i, 
  meanlength=mean(functionaldatabase[functionaldatabase$species==i,"length_cm"],na.rm=T)))
}
 
alllength
##This becomes the small, medium and large groups for size
alllength[alllength$meanlength<31,]
alllength[alllength$meanlength[31:80],]
alllength[alllength$meanlength>80,]

##make a dataset that just includes trawl information from all four regions
trawldata <- allregions[, c(1:48)]
names(trawldata)
names(trawldata)[2] <- paste("year")
names(trawldata)[7] <- paste("month")
names(trawldata)[8] <- paste("day")
names(trawldata)[29] <- paste("weight_kg")
names(trawldata)[26] <- paste("location")
names(trawldata)[27] <- paste("taxon")
names(trawldata)[28] <- paste("individual_count")
names(trawldata)[30] <- paste("presence")
names(trawldata)[31] <- paste("sampling_method")
names(trawldata)[32] <- paste("collection")
names(trawldata)[34] <- paste("strat_type")
names(trawldata)[35] <- paste("mission")
names(trawldata)[36] <- paste("name")
names(trawldata)[37] <- paste("season")
names(trawldata)[38] <- paste("date_time")
names(trawldata)[39] <- paste("distance")
names(trawldata)[40] <- paste("surf_temp")
names(trawldata)[41] <- paste("bott_temp")
names(trawldata)[42] <- paste("bott_sal")
names(trawldata)[45] <- paste("dmin")
names(trawldata)[46] <- paste("dmax")
names(trawldata)[47] <- paste("totno")
names(trawldata)[48] <- paste("totwgt")
names(trawldata)

#remove unecessary columns
trawldata$Date <- NULL
trawldata$ELAT <- NULL
trawldata$ELONG <- NULL
trawldata$Date <- NULL
trawldata$newsciname <- NULL
trawldata$fishbase <- NULL
trawldata$collection <- NULL
trawldata$genus <- NULL
trawldata$family <- NULL
trawldata$order <- NULL
trawldata$class <- NULL
trawldata$subphylum <- NULL
trawldata$phylum <- NULL
names(trawldata)
head(trawldata)

#this script contains only trawl data from all four regions
#write.csv(trawldata, file='C:/Users/StevensLy/Documents/Database/Data/trawldata.csv')
trawldata<-read.csv("C:/Users/StevensLy/Documents/Database/Data/trawldata.csv",stringsAsFactors = F)

#next step is to merge the two databases together (trawldata and functionaldatabase)
names(functionaldatabase)
names(trawldata)
trawldata$X <- NULL
alldata <- merge(trawldata,functionaldatabase,by="species")
alldata[alldata==""] <- NA
head(alldata)
View(alldata)

write.csv(alldata, file='C:/Users/StevensLy/Documents/Database/Data/fourregions_merged.csv')
fourregions_merged<-read.csv("C:/Users/StevensLy/Documents/Database/Data/fourregions_merged.csv",stringsAsFactors = F)
head(fourregions_merged)
names(fourregions_merged)
fourregions_merged$X <- NULL
View(fourregions_merged)

###Range of lat and long for plotting coordinates##
names(fourregions_merged)
min(fourregions_merged$latitude)
max(fourregions_merged$latitude)
min(fourregions_merged$longitude)
max(fourregions_merged$longitude)
##Starting to break down data and determine percentage of species captured in each region##
##frequency of species captured in each region##
##some observations from quebec may have to be removed because they are in the data
##but recorded as absent
frequency_of_species <- NULL #empty data
for (i in unique(fourregions_merged$region)){
  for (y in unique(fourregions_merged[fourregions_merged$region==i,"year"])){
    
    temp <- dplyr::filter(fourregions_merged,region==i,year==y)
    
    freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
    
    colnames(freq_obs) <- c("Species","frequency")
     
     freq_obs$region=i
     freq_obs$year=y
     
     freq_obs$freq_stand <- range01(freq_obs$frequency)
     
     frequency_of_species <- rbind(frequency_of_species,freq_obs)
     
     
   } #end of y 'year_final' loop
   
 } #end of i 'REGION' loop
 
frequency_of_species <- frequency_of_species[order(frequency_of_species$year),]
View(frequency_of_species)


##frequency of species from all regions 
##different coloured dots for each region would be cool
plotfreq1<-ggplot(frequency_of_species)+
geom_point(aes(x=Species,y=freq_stand))+
ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of species captured from all four regions")+
coord_flip()+
theme_bw();plotfreq1


##grouping species by temperature##
##this info can be added to the functional trait table##

##Mean temperature of gulf
meantemp_g <- trawldata%>%group_by(species, region, year)%>%summarise(meantemp=mean(temperature,na.rm=T))%>%ungroup()%>%data.frame
meantemp_speciesg <- meantemp_g%>%group_by(species)%>%summarise(meantemp=mean(meantemp,na.rm=T))%>%ungroup()%>%data.frame



##I took the mean bottom temperatures of species in maritime and newfoundland region and took the mean temperatures of those regions##
##This have me one mean temperature for each
meanbottemp_nm <- trawldata%>%group_by(species, region, year)%>%summarise(meantemp=mean(bott_temp,na.rm=T))%>%ungroup()%>%data.frame
meantemp_speciesnm <- meanbottemp_nm%>%group_by(species)%>%summarise(meantemp=mean(meantemp,na.rm=T))%>%ungroup()%>%data.frame


##plot temperatures for gulf region
gulf_temperature <- meantemp_speciesg[!is.na(meantemp_speciesg$meantemp),]
ggplot(gulf_temperature, aes(x=meantemp, y=species))+
  geom_point()+theme_bw()

##plot temperatures for maritime and newfoundland region
mn_temperature <- meantemp_speciesnm[!is.na(meantemp_speciesnm$meantemp),]
ggplot(mn_temperature, aes(x=meantemp, y=species))+
  geom_point()+theme_bw()