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
quebec<-read.csv("C:/Users/StevensLy/Documents/Database/Data/Quebec Region RV DFO Survey_SLGO.csv",stringsAsFactors = F)
gulf<-read.csv("C:/Users/StevensLy/Documents/Database/Data/Gulf Region RV DFO Survey.csv",stringsAsFactors = F)

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



#Cleaning Dates
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

#making column names more consistent. Since other regions only species lat/long
#take start lat/start long and name it to just be lat/long
names(maritimenewfoundland)[15] <- paste("longitude")
names(maritimenewfoundland)[16] <- paste("latitude")
names(maritimenewfoundland)[9] <- paste("set")
names(maritimenewfoundland)[3] <- paste("strat")
names(maritimenewfoundland)[7] <- paste("region")
names(maritimenewfoundland)[19] <- paste("depth")


allregions <- rbind.all.columns(gulfquebec,maritimenewfoundland)
head(allregions)
names(allregions)
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
names(trawldata)

#this script contains only trawl data from all four regions
write.csv(trawldata, file='C:/Users/StevensLy/Documents/Database/Data/trawldata.csv')




