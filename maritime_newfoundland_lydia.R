library(reshape2)
library(dplyr)
library(rfishbase)
library(taxize)
source("C:/Users/StevensLy/Documents/Database/Code/MPA database/ClassifyFunction.R")


MNData <- read.csv("C:/Users/StevensLy/Documents/Database/Data/FINALMN_Lydia.csv",stringsAsFactors = F)

#Create new colum to put species into
MNData$newsciname2 = MNData$newsciname

#Function to fix extra space at the end of some species names. 
SpaceFix <- function(x){
  if(!is.na(x)){
    if(substr(x,start = nchar(x),stop = nchar(x))==" "){x=substr(x,start = 1,stop=nchar(x)-1)}
    if(substr(x,start = 1,stop = 1)==" "){x=substr(x,start = 2,stop=nchar(x))}
    }
return(x)}

#remove all taxonomic groups at the end of scientific names
MNData$newsciname2 <- gsub(" P\\.","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" F\\.","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" O\\.","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" C\\.","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" SP\\.","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" S\\.P","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" S\\.C","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" S\\.P","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" S\\.O","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" SF","",MNData$newsciname2)
MNData$newsciname2 <- gsub(" SP","",MNData$newsciname2)

MNData$newsciname2 <- gsub("\\.","",MNData$newsciname2)



#Change names to be consistent  
MNData[which(MNData$newsciname2=="ALEPISAURUS BREVIROSTIS"),"newsciname2"] <- "ALEPISAURUS BREVIROSTRIS"
MNData[which(MNData$newsciname2=="ANGUILLIFORMES APODES ORDER"),"newsciname2"] <- "ANGUILLIFORMES"
MNData[which(MNData$newsciname2=="BARNACLES"),"newsciname2"] <- "MAXILLOPODA"
MNData[which(MNData$newsciname2=="BARRACUDINA"),"newsciname2"] <- "	PARALEPIDIDAE"
MNData[which(MNData$newsciname2=="BLENIIDAE,LIPOPHRYS,PHOLIS SP."),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="BRISTLE WORMS"),"newsciname2"] <- "ANNELIDA"
MNData[which(MNData$newsciname2=="BRITTLE STAR"),"newsciname2"] <- "OPHIUROIDEA"
MNData[which(MNData$newsciname2=="CHIMAERIFORMES HOLOCEPHALI ORDER"),"newsciname2"] <- "CHIMAERIFORMES"
MNData[which(MNData$newsciname2=="cHLOROPHTHALMUS AGASSIZI"),"newsciname2"] <- "CHLOROPHTHALMUS AGASSIZI"
MNData[which(MNData$newsciname2=="CYCLOPTERUS SP. AND GASTROPODA C."),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="DAISY"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="EEL (UNIDENTIFIED)"),"newsciname2"] <- "ANGUILLIFORMES"
MNData[which(MNData$newsciname2=="EGGS (UNIDENTIFIED)"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="FOREIGN ARTICLES,GARBAGE"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="FOURTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="GASTEROSTEIFORMES ORDER"),"newsciname2"] <- "GASTEROSTEIFORMES"
MNData[which(MNData$newsciname2=="HYAS ARANEUS AND/OR HYAS COARCTATUS"),"newsciname2"] <- "HYAS (UNIDENTIFIED)"
MNData[which(MNData$newsciname2=="LIBINIA EMARGINATA,"),"newsciname2"] <- "LIBINIA EMARGINATA"
MNData[which(MNData$newsciname2=="NATANTIA"),"newsciname2"] <- "DECAPODA"
MNData[which(MNData$newsciname2=="newsciname"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="NEZUMIA BAIRDI"),"newsciname2"] <- "NEZUMIA BAIRDII"
MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="PARASUDIS TRUNCULENTUS"),"newsciname2"] <- "PARASUDIS TRUNCULENTA"
MNData[which(MNData$newsciname2=="PLACOPECTEN MAGELLANICUS SHELLS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="PLUERONECTIFORMES HETEROSOMATA ORDER"),"newsciname2"] <- "PLEURONECTIFORMES"
MNData[which(MNData$newsciname2=="PSEUDOPLUERONECTES AMERICANUS"),"newsciname2"] <- "PSEUDOPLEURONECTES AMERICANUS"
MNData[which(MNData$newsciname2=="SAND DOLLARS, URCHINS"),"newsciname2"] <- "ECHINOIDEA"
MNData[which(MNData$newsciname2=="SEA SLUGS"),"newsciname2"] <- "GASTROPODA"
MNData[which(MNData$newsciname2=="SEAWEED,ALGAE,KELP"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SECOND UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="FIRST UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="THIRD UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="FIFTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SIXTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SEVENTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="EIGHTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SERRIVOMER BEANI"),"newsciname2"] <- "SERRIVOMER BEANII"
MNData[which(MNData$newsciname2=="SKATE EGGS (UNIDENTIFIED)"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SKATES AND RAYS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SNAIL AND SLUG EGGS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SNAILS AND SLUGS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SOFT CORAL (UNIDENTIFIED)"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="STICKLEBACK (UNIDENTIFIED)"),"newsciname2"] <- "GASTEROSTEIDAE"
MNData[which(MNData$newsciname2=="STONES AND ROCKS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="UNIDENTIFIED FISH"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="UNIDENTIFIED SP"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="BARRACUDINA (UNIDENTIFIED)"),"newsciname2"] <- "PARALEPIDIDAE"
MNData[which(MNData$newsciname2=="CHIONOECETES (UNIDENTIFIED)"),"newsciname2"] <- "CHIONOECETES"
MNData[which(MNData$newsciname2=="FLOUNDER (UNIDENTIFIED)"),"newsciname2"] <- "PARALICHTHYIDAE"
MNData[which(MNData$newsciname2=="HYAS (UNIDENTIFIED)"),"newsciname2"] <- "HYAS"
MNData[which(MNData$newsciname2=="NATICIDAE (UNIDENTIFIED)"),"newsciname2"] <- "NATICIDAE"
MNData[which(MNData$newsciname2=="SEASNAIL (UNIDENTIFIED)"),"newsciname2"] <- "GASTROPODA"
MNData[which(MNData$newsciname2=="SYNGNATHINAE (UNIDENTIFIED)"),"newsciname2"] <- "SYNGNATHIDAE"
MNData[which(MNData$newsciname2=="STOMIATOID (UNIDENTIFIED)"),"newsciname2"] <- "STOMIATOID"
MNData[which(MNData$newsciname2=="STOMIAS (UNIDENTIFIED)"),"newsciname2"] <- "STOMIAS"
MNData[which(MNData$newsciname2=="EGGS (UNIDENTIDIED)"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="UNIDENTIFIED"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SHARK,SAND"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="GAPPER"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="PARASITES,ROUND WORMS"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="SCULPIN EGGS UNID."),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="UNIDENTIFIED.FISH"),"newsciname2"] <- NA
MNData[which(MNData$newsciname2=="TRIGLOPS OMMATISTIUS"),"newsciname2"] <- "TRIGLOPS MURRAYI"
MNData[which(MNData$newsciname2=="LITHODES MAIA"),"newsciname2"] <- "LITHODES MAJA"
MNData[which(MNData$fishbase=="FOETOREPUS AGASSUSZII"),"fishbase"] <- "FOETOREPUS AGASSIZII"
MNData[which(MNData$fishbase=="HERMIT CRABS"),"fishbase"] <- "DECAPODA"
MNData[which(MNData$fishbase=="COTTIDEA"),"fishbase"] <- "COTTIDAE"
MNData[which(MNData$fishbase=="VAZELLA POURTALESI"),"fishbase"] <- "VAZELLA POURTALESII"
MNData[which(MNData$fishbase=="	CTENOPHORA"),"fishbase"] <- "CTENOPHORA"
MNData[which(MNData$fishbase=="	PORANIA PULVILIS"),"fishbase"] <- "PORANIA PULVILLUS"
MNData[which(MNData$fishbase=="	CEREMASTER GRANULARIS"),"fishbase"] <- "CERAMASTER GRANULARIS"
MNData[which(MNData$fishbase=="		PANTOPODA"),"fishbase"] <- "PANTOPODA"




#755 unique species/classifications
length(unique(MNData$newsciname2))


#Create new colum for fishbase species only
MNData$fishbase = MNData$newsciname2 

#Make changes to species
MNData[which(MNData$fishbase=="REINHARDTIUS HIPPOGLOSSOIDES"),"fishbase"] <- "Reinhardtius hippoglossoides"
MNData[which(MNData$fishbase=="LITHODES MAJA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="LITHODES MAIA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="LEPOHIDIUM PROFUNDORUM"),"fishbase"] <- "Lepophidium profundorum"
MNData[which(MNData$fishbase=="FOETOREPUS AGASSUSZII"),"fishbase"] <- "Foetorepus agassizii"
MNData[which(MNData$fishbase=="DORYTEUTHIS PEALEII"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="HERMIT CRABS"),"fishbase"] <- "DECAPODA"
MNData[which(MNData$fishbase=="COTTIDEA"),"fishbase"] <- "COTTIDAE"
MNData[which(MNData$fishbase=="CYCLOPTERUS AND GASTROPODA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="PARALIPARUS COPEI"),"fishbase"] <- "PARALIPARIS COPEI"
MNData[which(MNData$fishbase=="PARAKICHTHYS DENTATUS"),"fishbase"] <- "PARALICHTHYS DENTATUS"
MNData[which(MNData$fishbase=="STOMIATOID"),"fishbase"] <- "STOMIIFORMES"
MNData[which(MNData$fishbase=="CLAMS"),"fishbase"] <- "BIVALVIA"
MNData[which(MNData$fishbase=="WHELKS"),"fishbase"] <- "BUCCINIDAE"
MNData[which(MNData$fishbase=="RADICIPES GRACILIS"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="SAND DOLLARS"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="PAGUROIDEA SF"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="SEA ANEMONE"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="MYSID SHRIMP"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="VAZELLA POURTALESII"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="VAZELLA POURTALESI"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="AEGA PSORA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="HENRICA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="	CTENOPHORA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="PORANIA PULVILIS"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="CEREMASTER GRANULARIS"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="	PANTOPODA"),"fishbase"] <- NA
MNData[which(MNData$fishbase=="APRISTURUS SP"),"fishbase"] <- "APRISTURUS"


#Fix species extra spaces in fishbase column
MNData$fishbase <- unlist(lapply(MNData$fishbase,FUN=SpaceFix))
MNData$fishbase <- unlist(lapply(MNData$fishbase,FUN=SpaceFix))

View(MNData)

Classify("AGONIDAE")
Classify("NA")

#apply function  to unique species in column fishbase
#the function pulls taxonimc classification information from FishBase

##### The Classify function is not working. It is picking up NA. Need to fix!
outlist <- lapply(unique(MNData$fishbase),FUN=Classify)
taxInfo <- do.call("rbind", outlist)

test=unique(MNData$fishbase)
test[which(test=='NA')+1]









cbind(MNData,taxInfo)

write.csv(MNData, file='Database/Data/FINALMN_Lydia.csv')


outlist <- lapply(unique(MNData$newsciname),FUN=Classify)
taxInfo <- do.call("rbind", outlist)

cbind(MNData,taxInfo)
