library(dplyr)
library(rfishbase)
library(lubridate)
library(tidyr)

newdata <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata240217_Lydia.csv",stringsAsFactors = F)
head(newdata)

#remove unessary columns
head(newdata)
newdata$X <- NULL
newdata$X.1 <- NULL
newdata$X.2 <- NULL
newdata$X.3 <- NULL
newdata$X.4 <- NULL
newdata$sciname <- NULL
newdata$newsciname <- NULL
newdata$newsciname2 <- NULL

#date formatting using the package lubridate to format Maritime Dates
head(newdata$DATETIME)
newdata$NEWDATETIME <- dmy_hm(paste(newdata$DATETIME, sep="", tz=""))
head(newdata$NEWDATETIME)
head(newdata)


#created new columns because when I separated into the existing day month year columns, newfoundland data disappeared
newdata$YEAR_M <- NA
newdata$MONTH_M <- NA
newdata$DAY_M <- NA

newdata$YEAR_M <- format(newdata$NEWDATETIME, '%Y')
newdata$MONTH_M <- format(newdata$NEWDATETIME, '%m')
newdata$DAY_M <- format(newdata$NEWDATETIME, '%d')
newdata$TIME <- format(newdata$NEWDATETIME, '%H:%M')
head(newdata)

#combine new year, month, day, time columns with original
#create new column for day
newdata$day_final <- ifelse(is.na(newdata$DAY) ==T, newdata$DAY_M, newdata$DAY)
newdata$month_final <- ifelse(is.na(newdata$MONTH)==T, newdata$MONTH_M, newdata$MONTH)
newdata$year_final <- ifelse(is.na(newdata$YEAR)==T, newdata$YEAR_M, newdata$YEAR)
head(newdata)

#Check to see if worked for newfoundland
newdata[which(newdata$REGION=="NEWFOUNDLAND"),]

#remove old date columns to clean up dataframe 
newdata$DAY <- NULL
newdata$DAY_M <- NULL
newdata$MONTH <- NULL
newdata$MONTH_M <- NULL
newdata$YEAR <- NULL
newdata$YEAR_M <- NULL
newdata$TIME <- NULL
head(newdata)
View(newdata)

#change lowercase to uppercase in colunmn "SEASONS" and 'STRAT_TYPE"
newdata$SEASON <- toupper(newdata$SEASON)
newdata$STRAT_TYPE <- toupper(newdata$STRAT_TYPE)

#fix species column before merge
newdata[which(newdata$species=="Acipenser oxyrhynchus"),"species"] <- "Acipenser oxyrinchus"
newdata[which(newdata$species=="Aspidophoroides olriki"),"species"] <- "Aspidophoroides olrikii"
newdata[which(newdata$species=="Ceratias holboelli"),"species"] <- "Ceratias holboellii"
newdata[which(newdata$species=="Cryptopsaras couesi"),"species"] <- "Cryptopsaras couesii"
newdata[which(newdata$species=="Pandalus propinquus"),"species"] <- "Pandalus propinqvus"
newdata[which(newdata$species=="Scomberesox saurus"),"species"] <- "Scomberesox saurus saurus"
newdata[which(newdata$species=="Stichaeus punctatus"),"species"] <- "Stichaeus punctatus punctatus"
newdata[which(newdata$species=="Stomias boa"),"species"] <- "Stomias boa boa"
newdata[which(newdata$fishbase=="STOMIAS BOA FEROX"),"species"] <- "Stomias boa ferox"
newdata[which(newdata$species=="Synaphobranchus kaupi"),"species"] <- "Synaphobranchus kaupii"
newdata[which(newdata$species=="Gymnelis viridis"),"species"] <- "Gymnelus viridis"
newdata[which(newdata$species=="Osmerus mordax"),"species"] <- "Osmerus mordax mordax"
newdata[which(newdata$species=="Cottunculus thompsoni"),"species"] <- "Cottunculus thomsonii"
newdata[which(newdata$species=="Simenchelys parasiticus"),"species"] <- "Simenchelys parasitica"
newdata[which(newdata$species=="Lycodes esmarki"),"species"] <- "Lycodes esmarkii"
newdata[which(newdata$species=="Lumpenus lumpretaeformis"),"species"] <- "Lumpenus lampretaeformis"
newdata[which(newdata$species=="Paraliparis copei"),"species"] <- "Paraliparis copei copei"

View(newdata)

#merge existing trait data with newdata
traitdata <- read.csv("C:/Users/StevensLy/Desktop/MPA Framework/traitinformation.csv",stringsAsFactors = F)
newdata2 <- merge(newdata, traitdata, by="species", all=T)
View(newdata2)

#sort species based on how often they were captured
table(newdata$species)
table(newdata$species)[which(table(newdata$species)>9000)]
table(newdata$species)[which(table(newdata$species)<1288)] #Less than 5% of the most captured species
table(newdata$species)[which(table(newdata$species)>1288)] 



########trying to figure out rfishbase########

options(FISHBASE_API = "http://fishbase.ropensci.org")
fish <- newdata2$species
fish
species(fish[fish=="Agonus decagonus"], fields=c("SpecCode", "PriceCateg", "Vulnerability"))
species(fish, fields=c("SpecCode", "PriceCateg", "Vulnerability"))
species(fish[fish == "Gadus morhua"], fields = c("SpecCode"), server=getOption("http://fishbase.ropensci.org"))

#could try installing directly from github
#library(devtools)
#devtools::install_github("ropensci/rfishbase@sckott-endpoint-fix")



