#test1

library(reshape2)
library(dplyr)

source("Database/Code/ClassifyFunction.R")


MNData <- read.csv("Database/Data/FINALMN_Lydia.csv",stringsAsFactors = F)

#Create new colum to put species into
MNData$newsciname2 = MNData$newsciname

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

#Change names to be consistent  
MNData[which(MNData$newsciname2=="ALEPISAURUS BREVIROSTIS"),"newsciname2"] <- "ALEPISAURUS BREVIROSTRIS"
MNData[which(MNData$newsciname2=="ANGUILLIFORMES APODES ORDER"),"newsciname2"] <- "ANGUILLIFORMES"
MNData[which(MNData$newsciname2=="BARNACLES"),"newsciname2"] <- "MAXILLOPODA"
MNData[which(MNData$newsciname2=="BARRACUDINA"),"newsciname2"] <- "	PARALEPIDIDAE"
MNData[which(MNData$newsciname2=="BLENIIDAE,LIPOPHRYS,PHOLIS SP."),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="BRISTLE WORMS"),"newsciname2"] <- "ANNELIDA"
MNData[which(MNData$newsciname2=="BRITTLE STAR"),"newsciname2"] <- "OPHIUROIDEA"
MNData[which(MNData$newsciname2=="CHIMAERIFORMES HOLOCEPHALI ORDER"),"newsciname2"] <- "CHIMAERIFORMES"
MNData[which(MNData$newsciname2=="cHLOROPHTHALMUS AGASSIZI"),"newsciname2"] <- "CHLOROPHTHALMUS AGASSIZI"
MNData[which(MNData$newsciname2=="CYCLOPTERUS SP. AND GASTROPODA C."),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="DAISY"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="EEL (UNIDENTIFIED)"),"newsciname2"] <- "ANGUILLIFORMES"
MNData[which(MNData$newsciname2=="EGGS (UNIDENTIFIED)"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="FOREIGN ARTICLES,GARBAGE"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="FOURTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="GASTEROSTEIFORMES ORDER"),"newsciname2"] <- "GASTEROSTEIFORMES"
MNData[which(MNData$newsciname2=="HYAS ARANEUS AND/OR HYAS COARCTATUS"),"newsciname2"] <- "HYAS (UNIDENTIFIED)"
MNData[which(MNData$newsciname2=="LIBINIA EMARGINATA,"),"newsciname2"] <- "LIBINIA EMARGINATA"
MNData[which(MNData$newsciname2=="NATANTIA"),"newsciname2"] <- "DECAPODA"
MNData[which(MNData$newsciname2=="newsciname"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="NEZUMIA BAIRDI"),"newsciname2"] <- "NEZUMIA BAIRDII"
MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="PARASUDIS TRUNCULENTUS"),"newsciname2"] <- "PARASUDIS TRUNCULENTA"
MNData[which(MNData$newsciname2=="PLACOPECTEN MAGELLANICUS SHELLS"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="PLUERONECTIFORMES HETEROSOMATA ORDER"),"newsciname2"] <- "PLEURONECTIFORMES"
MNData[which(MNData$newsciname2=="PSEUDOPLUERONECTES AMERICANUS"),"newsciname2"] <- "PSEUDOPLEURONECTES AMERICANUS"
MNData[which(MNData$newsciname2=="SAND DOLLARS, URCHINS"),"newsciname2"] <- "ECHINOIDEA"
MNData[which(MNData$newsciname2=="SEA SLUGS"),"newsciname2"] <- "GASTROPODA"
MNData[which(MNData$newsciname2=="SEAWEED,ALGAE,KELP"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SECOND UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="FIRST UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="THIRD UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="FIFTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SIXTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SEVENTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="EIGHTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SERRIVOMER BEANI"),"newsciname2"] <- "SERRIVOMER BEANII"
MNData[which(MNData$newsciname2=="SKATE EGGS (UNIDENTIFIED)"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SKATES AND RAYS"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SNAIL AND SLUG EGGS"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SNAILS AND SLUGS"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="SOFT CORAL (UNIDENTIFIED)"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="STICKLEBACK (UNIDENTIFIED)"),"newsciname2"] <- "GASTEROSTEIDAE"
MNData[which(MNData$newsciname2=="STONES AND ROCKS"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="UNIDENTIFIED FISH"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="UNIDENTIFIED SP"),"newsciname2"] <- "NA"
MNData[which(MNData$newsciname2=="BARRACUDINA (UNIDENTIFIED)"),"newsciname2"] <- "PARALEPIDIDAE"
MNData[which(MNData$newsciname2=="CHIONOECETES (UNIDENTIFIED)"),"newsciname2"] <- "CHIONOECETES"
MNData[which(MNData$newsciname2=="FLOUNDER UNIDENTIFIED"),"newsciname2"] <- "PARALICHTHYIDAE"
MNData[which(MNData$newsciname2=="HYAS (UNIDENTIFIED"),"newsciname2"] <- "HYAS"
MNData[which(MNData$newsciname2=="NATICIDAE (UNIDENTIFIED"),"newsciname2"] <- "NATICIDAE"
MNData[which(MNData$newsciname2=="sEASNAIL (UNIDENTIFIED)"),"newsciname2"] <- "GASTROPODA"
MNData[which(MNData$newsciname2=="SYNGNATHINAE (UNIDENTIFIED)"),"newsciname2"] <- "SYNGNATHIDAE"
MNData[which(MNData$newsciname2=="STOMIATOID (UNIDENTIFIED)"),"newsciname2"] <- "STOMIATOID"
MNData[which(MNData$newsciname2=="UNIDENTIFIED"),"newsciname2"] <- "NA"




View(MNData)




Classify("agonidae")

write.csv(MNData, file='Database/Data/FINALMN_Lydia.csv')


outlist <- lapply(unique(MNData$newsciname),FUN=Classify)
taxInfo <- do.call("rbind", outlist)

cbind(MNData,taxInfo)