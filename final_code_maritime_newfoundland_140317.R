library(reshape2)
library(dplyr)
library(rfishbase)
library(taxize)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)

# NewfData <- read.csv("Database/Data/newfoundland_temp_Lydia.csv")
# str(NewfData)                            
# head(NewfData)
# nrow(NewfData)
# ncol(NewfData)
# 
# # species and Weights are now collapsed into two 
# # columns
# Newf1<-melt(NewfData, id=1:13)
# View(Newf1)
# 
# # removed duplicated rows that had no total weight
# Newf2 <- Newf1[is.na(Newf1$value) == FALSE,]
# View(Newf2)
# 
# # Once Newfoundland data were "melted" it was combined with Maritime data in excel
#
# combined <- read.csv("Database/Data/updatedcombined022017.csv")
# 
# # Create a new column. Take common name from one column and replace with scientific name
# 
# combined$sciname = NA
# combined[combined$COMM == "PANDALUS SP.",]$sciname ="PANDALUS SP."
# combined[combined$COMM == "AMERICAN PLAICE",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "MAILED SCULPIN",]$sciname ="TRIGLOPS OMMATISTIUS"
# combined[combined$COMM == "WINTER FLOUNDER",]$sciname ="PSEUDOPLEURONECTES AMERICANUS"
# combined[combined$COMM == "THORNY SKATE",]$sciname ="AMBLYRAJA RADIATA"
# combined[combined$COMM == "SHORT-FIN SQUID",]$sciname ="ILLEX ILLECEBROSUS"
# combined[combined$COMM == "COD(ATLANTIC)",]$sciname ="GADUS MORHUA"
# combined[combined$COMM == "WITCH FLOUNDER",]$sciname ="GLYPTOCEPHALUS CYNOGLOSSUS"
# combined[combined$COMM == "YELLOWTAIL FLOUNDER",]$sciname ="LIMANDA FERRUGINEA"
# combined[combined$COMM == "REDFISH UNSEPARATED",]$sciname ="SEBASTES MENTELLA"
# combined[combined$COMM == "SPIDER/(QUEEN,SNOW)UNID",]$sciname ="CHIONOECETES (UNIDENTIFIED)"
# combined[combined$COMM == "LITTLE SKATE",]$sciname ="LEUCORAJA ERINACEA"
# combined[combined$COMM == "LONGHORN SCULPIN",]$sciname ="MYOXOCEPHALUS OCTODECEMSPINOSUS"
# combined[combined$COMM == "EELPOUTS(NS)",]$sciname ="LYCODES TERRAENOVAE"
# combined[combined$COMM == "ARGENTINE(ATLANTIC)",]$sciname ="ARGENTINA SILUS"
# combined[combined$COMM == "LONGFIN HAKE",]$sciname ="PHYCIS CHESTERI"
# combined[combined$COMM == "NORTHERN HAGFISH",]$sciname ="MYXINE GLUTINOSA"
# combined[combined$COMM == "MARLIN-SPIKE GRENADIER",]$sciname ="NEZUMIA BAIRDII"
# combined[combined$COMM == "HADDOCK",]$sciname ="MELANOGRAMMUS AEGLEFINUS"
# combined[combined$COMM == "WHITE HAKE",]$sciname ="UROPHYCIS TENUIS"
# combined[combined$COMM == "SILVER HAKE",]$sciname ="MERLUCCIUS BILINEARIS"
# combined[combined$COMM == "STRIPED ATLANTIC WOLFFISH",]$sciname ="ANARHICHAS LUPUS"
# combined[combined$COMM == "SMOOTH SKATE",]$sciname ="MALACORAJA SENTA"
# combined[combined$COMM == "MONKFISH,GOOSEFISH,ANGLER",]$sciname ="LOPHIUS AMERICANUS"
# combined[combined$COMM == "TURBOT,GREENLAND HALIBUT",]$sciname ="REINHARDTIUS HIPPOGLOSSOIDES"
# combined[combined$COMM == "POLLOCK",]$sciname ="POLLACHIUS VIRENS"
# combined[combined$COMM == "SEA RAVEN",]$sciname ="HEMITRIPTERUS AMERICANUS"
# combined[combined$COMM == "CUSK",]$sciname ="BROSME BROSME"
# combined[combined$COMM == "CAPELIN",]$sciname ="MALLOTUS VILLOSUS"
# combined[combined$COMM == "WINTER SKATE",]$sciname ="LEUCORAJA OCELLATA"
# combined[combined$COMM == "LUMPFISH",]$sciname ="CYCLOPTERUS LUMPUS"
# combined[combined$COMM == "HALIBUT(ATLANTIC)",]$sciname ="HIPPOGLOSSUS HIPPOGLOSSUS"
# combined[combined$COMM == "HERRING(ATLANTIC)",]$sciname ="CLUPEA HARENGUS"
# combined[combined$COMM == "ALEWIFE",]$sciname ="ALOSA PSEUDOHARENGUS"
# combined[combined$COMM == "SNAKE BLENNY",]$sciname ="LUMPENUS LAMPRETAEFORMIS"
# combined[combined$COMM == "FOURBEARD ROCKLING",]$sciname ="ENCHELYOPUS CIMBRIUS"
# combined[combined$COMM == "AMERICAN LOBSTER",]$sciname ="HOMARUS AMERICANUS"
# combined[combined$COMM == "LANTERNFISH (NS)",]$sciname ="BENTHOSEMA GLACIALE"
# combined[combined$COMM == "SPINY DOGFISH",]$sciname ="SQUALUS ACANTHIAS"
# combined[combined$COMM == "OCEAN POUT",]$sciname ="ZOARCES AMERICANUS"
# combined[combined$COMM == "ATLANTIC SPINY LUMPSUCKER",]$sciname ="EUMICROTREMUS SPINOSUS"
# combined[combined$COMM == "SCALLOPS",]$sciname ="PLACOPECTEN MAGELLANICUS"
# combined[combined$COMM == "MACKEREL(ATLANTIC)",]$sciname ="SCOMBER SCOMBRUS"
# combined[combined$COMM == "SHAD AMERICAN",]$sciname ="ALOSA SAPIDISSIMA"
# combined[combined$COMM == "THREEBEARD ROCKLING",]$sciname ="GAIDROPSARUS VULGARIS"
# combined[combined$COMM == "SHORTTAILED EELPOUT(VAHL)",]$sciname ="LYCODES VAHLII"
# combined[combined$COMM == "SPOTTED WOLFFISH",]$sciname ="ANARHICHAS MINOR"
# combined[combined$COMM == "OFF-SHORE HAKE",]$sciname ="MERLUCCIUS ALBIDUS"
# combined[combined$COMM == "RADIATED SHANNY",]$sciname ="ULVARIA SUBBIFURCATA"
# combined[combined$COMM == "LAVAL'S EELPOUT",]$sciname ="LYCODES TERRAENOVAE"
# combined[combined$COMM == "ARCTIC EELPOUT",]$sciname ="LYCODES RETICULATUS"
# combined[combined$COMM == "ALLIGATOR FISH (NS)",]$sciname ="ASPIDOPHOROIDES MONOPTERYGIUS"
# combined[combined$COMM == "ALLIGATORFISH",]$sciname ="ASPIDOPHOROIDES OLRIKII"
# combined[combined$COMM == "JONAH CRAB",]$sciname ="CANCER BOREALIS"
# combined[combined$COMM == "AMERICAN PLAICE",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "DAUBED SHANNY",]$sciname ="LEPTOCLINUS MACULATUS"
# combined[combined$COMM == "NORTHERN STONE CRAB",]$sciname ="LITHODES MAJA"
# combined[combined$COMM == "BRILL/WINDOWPANE",]$sciname ="SCOPHTHALMUS AQUOSUS"
# combined[combined$COMM == "GULF STREAM FLOUNDER",]$sciname ="CITHARICHTHYS ARCTIFRONS"
# combined[combined$COMM == "ROSEFISH(BLACK BELLY)",]$sciname ="HELICOLENUS DACTYLOPTERUS"
# combined[combined$COMM == "FISH DOCTOR",]$sciname ="GYMNELUS VIRIDIS"
# combined[combined$COMM == "NORTHERN SAND LANCE",]$sciname ="AMMODYTES DUBIUS"
# combined[combined$COMM == "BUTTERFISH",]$sciname ="PEPRILUS TRIACANTHUS"
# combined[combined$COMM == "ATLANTIC ROCK CRAB",]$sciname ="CANCER IRRORATUS"
# combined[combined$COMM == "WRYMOUTH",]$sciname ="CRYPTACANTHODES MACULATUS"
# combined[combined$COMM == "TOAD CRAB,UNIDENT.",]$sciname ="HYAS (UNIDENTIFIED)"
# combined[combined$COMM == "FOURSPOT FLOUNDER",]$sciname ="HIPPOGLOSSINA OBLONGA"
# combined[combined$COMM == "AMERICAN JOHN DORY",]$sciname =" ZENOPSIS OCELLATA"
# combined[combined$COMM == "NORTHERN WOLFFISH",]$sciname ="ANARHICHAS DENTICULATUS"
# combined[combined$COMM == "ROUNDNOSE GRENADIER",]$sciname ="CORYPHAENOIDES RUPESTRIS"
# combined[combined$COMM == "SPINY CRAB",]$sciname =" LITHODES MAIA"
# combined[combined$COMM == "SNOWFLAKE HOOKEAR SCULPIN",]$sciname ="ARTEDIELLUS UNCINATUS"
# combined[combined$COMM == "EELPOUT,NEWFOUNDLAND",]$sciname ="LYCODES LAVALAEI"
# combined[combined$COMM == "AMERICAN ATLANT STURGEON",]$sciname ="ACIPENSER OXYRINCHUS"
# combined[combined$COMM == "WOLF EELPOUT",]$sciname ="LYCENCHELYS VERRILLII"
# combined[combined$COMM == "PALLID SCULPIN",]$sciname ="COTTUNCULUS THOMSONII"
# combined[combined$COMM == "SHORT-NOSE GREENEYE",]$sciname ="cHLOROPHTHALMUS AGASSIZI"
# combined[combined$COMM == "ROUND SKATE",]$sciname ="RAJELLA FYLLAE"
# combined[combined$COMM == "BLACK DOGFISH",]$sciname ="CENTROSCYLLIUM FABRICII"
# combined[combined$COMM == "SNOW CRAB (QUEEN)",]$sciname ="CHIONOECETES OPILIO"
# combined[combined$COMM == "ATLANTIC BATFISH",]$sciname ="DIBRANCHUS ATLANTICUS"
# combined[combined$COMM == "WHITE BARRACUDINA",]$sciname ="ARCTOZENUS RISSO"
# combined[combined$COMM == "CUSK EELS (NS)",]$sciname ="BENTHOCOMETES ROBUSTUS"
# #combined[combined$COMM == "NORTHERN,COMMON SEAROBIN",]$sciname ="PRIONOTUS CAROLINUS"
# combined[combined$COMM == "LOLIGO SP.",]$sciname ="LOLIGO SP."
# combined[combined$COMM == "CUNNER",]$sciname ="TAUTOGOLABRUS ADSPERSUS"
# combined[combined$COMM == "HATCHETFISH",]$sciname ="ARGYROPELECUS SP."
# combined[combined$COMM == "SQUIRREL OR RED HAKE",]$sciname ="UROPHYCIS CHUSS"
# combined[combined$COMM == "ATLANTIC SILVER HATCHFISH",]$sciname ="ARGYROPELECUS ACULEATUS"
# combined[combined$COMM == "GRAY'S CUTTHROAT EEL",]$sciname ="SYNAPHOBRANCHUS KAUPII"
# combined[combined$COMM == "FAWN CUSK EEL",]$sciname =" LEPOHIDIUM PROFUNDORUM"
# combined[combined$COMM == "PERCOIDEI F.",]$sciname ="PERCOIDEI F."
# combined[combined$COMM == "SIXTH UNIDENTIFIED PER SET",]$sciname ="SIXTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "RAINBOW SMELT",]$sciname ="OSMERUS MORDAX MORDAX"
# combined[combined$COMM == "SPOTFIN DRAGONET",]$sciname ="FOETOREPUS AGASSUSZII"
# combined[combined$COMM == "LONGFIN SQUID, LONGFIN INSHORE SQUID",]$sciname ="DORYTEUTHIS PEALEII"
# combined[combined$COMM == "SEASNAIL UNIDENTIFIED",]$sciname ="SEASNAIL (UNIDENTIFIED)"
# combined[combined$COMM == "ATLANTIC SEA POACHER",]$sciname ="LEPTAGONUS DECAGONUS"
# combined[combined$COMM == "SMELTS,CAPELIN (NS)",]$sciname ="MALLOTUS VILLOSUS"
# combined[combined$COMM == "ATLANTIC SEASNAIL",]$sciname ="BUCCINUM UNDATUM"
# combined[combined$COMM == "PANDALUS BOREALIS",]$sciname ="PANDALUS BOREALIS"
# combined[combined$COMM == "SEA SCALLOP",]$sciname ="PLACOPECTEN MAGELLANICUS"
# combined[combined$COMM == "ICELAND SCALLOP",]$sciname ="CHLAMYS ISLANDICA"
# combined[combined$COMM == "POLAR SCULPIN",]$sciname ="COTTUNCULUS MICROPS"
# combined[combined$COMM == "ECHINARACHNIUS PARMA",]$sciname ="ECHINARACHNIUS PARMA"
# combined[combined$COMM == "HERMIT CRABS",]$sciname ="HERMIT CRABS"
# combined[combined$COMM == "ASTEROIDEA S.C.",]$sciname ="ASTEROIDEA S.C"
# combined[combined$COMM == "SEA URCHIN (GREEN)",]$sciname ="STRONGYLOCENTROTUS DROEBACHIENSIS"
# combined[combined$COMM == "PANDALUS MONTAGUI",]$sciname ="PANDALUS MONTAGUI"
# combined[combined$COMM == "LONGNOSE GREENEYE",]$sciname ="PARASUDIS TRUCULENTA"
# combined[combined$COMM == "HOOKEAR SCULPIN,ATL.",]$sciname ="ARTEDIELLUS ATLANTICUS"
# combined[combined$COMM == "ROUGHNOSE GRENADIER",]$sciname ="TRACHYRINCUS MURRAYI"
# combined[combined$COMM == "PALE EELPOUT",]$sciname ="LYCODES PALLIDUS"
# combined[combined$COMM == "BEARDFISH",]$sciname ="POLYMIXIA LOWEI"
# combined[combined$COMM == "BARRACUDINA,UNIDENTIFIED",]$sciname ="BARRACUDINA (UNIDENTIFIED)"
# combined[combined$COMM == "ROCK GUNNEL(EEL)",]$sciname ="PHOLIS GUNNELLUS"
# combined[combined$COMM == "BLUEBACK HERRING",]$sciname ="ALOSA AESTIVALIS"
# combined[combined$COMM == "SNIPE EEL",]$sciname ="NEMICHTHYS SCOLOPACEUS"
# combined[combined$COMM == "GRUBBY OR LITTLE SCULPIN",]$sciname ="MYOXOCEPHALUS AENAEUS"
# combined[combined$COMM == "SNUBNOSE EEL, SLIME EEL",]$sciname ="NOTACANTHUS CHEMNITZII"
# combined[combined$COMM == "STICKLEBACK UNIDENTIFIED",]$sciname ="STICKLEBACK (UNIDENTIFIED)"
# combined[combined$COMM == "SCULPINS",]$sciname ="COTTIDEA F."
# combined[combined$COMM == "LUMPFISHES,SEASNAILS (NS)",]$sciname ="CYCLOPTERUS SP. AND GASTROPODA C."
# combined[combined$COMM == "BLACKSNOUT SEASNAIL",]$sciname ="PARALIPARUS COPEI"
# combined[combined$COMM == "NORTHERN PIPEFISH",]$sciname ="SYNGNATHUS FUSCUS"
# combined[combined$COMM == "SUMMER FLOUNDER",]$sciname ="PARAKICHTHYS DENTATUS"
# combined[combined$COMM == "HOLOTHUROIDEA C.",]$sciname ="HOLOTHUROIDEA C."
# combined[combined$COMM == "EEL-UNIDENTIFIED",]$sciname ="EEL (UNIDENTIFIED)"
# combined[combined$COMM == "SEA LAMPREY",]$sciname ="PETROMYZON MARINUS"
# combined[combined$COMM == "HYAS COARCTATUS",]$sciname ="HYAS COARCTATUS"
# combined[combined$COMM == "ATLANTIC SAURY,NEEDLEFISH",]$sciname ="SCOMBERESOX SAURUS SAURUS"
# combined[combined$COMM == "OCTOPUS",]$sciname ="OCTOPODIDAE F."
# combined[combined$COMM == "SHRIMP",]$sciname ="PANDALUS SP."
# combined[combined$COMM == "SPIDER CRAB (NS)",]$sciname ="LIBINIA EMARGINATA,"
# combined[combined$COMM == "NINTH UNIDENTIFIED PER SET",]$sciname ="NINTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "SEA URCHINS",]$sciname ="ARBACIIDAE F."
# combined[combined$COMM == "CRUSTACEA C.",]$sciname ="CRUSTACEA C."
# combined[combined$COMM == "OCTOPODIDAE F.",]$sciname ="OCTOPODIDAE F."
# combined[combined$COMM == "TWOHORN SCULPIN",]$sciname ="ICELUS BICORNIS"
# combined[combined$COMM == "STOMIATOID UNIDENTIFIED",]$sciname ="STOMIATOID (UNIDENTIFIED)"
# combined[combined$COMM == "SAND LANCE (NS)",]$sciname ="AMMODYTES AMERICANUS"
# combined[combined$COMM == "GREENEYES (NS)",]$sciname ="CHLOROPHTHALMIDAE SP."
# combined[combined$COMM == "UNIDENTIFIED SPECIES",]$sciname ="UNIDENTIFIED SP."
# combined[combined$COMM == "TOAD CRAB",]$sciname ="HYAS ARANEUS AND/OR HYAS COARCTATUS"
# combined[combined$COMM == "PURPLE-SPINED SEA URCHIN",]$sciname ="ARBACIA PUNCTULAT"
# combined[combined$COMM == "HOOKEAR SCULPIN (NS)",]$sciname ="ARTEDIELLUS ATLANTICUS"
# combined[combined$COMM == "CLAMS (NS)",]$sciname ="CLAMS"
# combined[combined$COMM == "OMMASTREPHES SP.",]$sciname ="OMMASTREPHES SP."
# combined[combined$COMM == "BOA DRAGONFISH",]$sciname ="STOMIAS BOA BOA"
# combined[combined$COMM == "VIPERFISH",]$sciname ="CHAULIODUS SLOANI"
# combined[combined$COMM == "CRANGON SP.",]$sciname ="CRANGON SP."
# combined[combined$COMM == "SPONGES",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "CALLIONYMUS SP.",]$sciname ="CALLIONYMUS SP."
# combined[combined$COMM == "ATLANTIC SOFT POUT",]$sciname ="MELANOSTIGMA ATLANTICUM"
# combined[combined$COMM == "BRISTLE WORMS",]$sciname ="BRISTLE WORMS"
# combined[combined$COMM == "ARCTIC SCULPIN",]$sciname ="MYOXOCEPHALUS SCORPIOIDES"
# combined[combined$COMM == "SHORTHORN SCULPIN",]$sciname ="MYOXOCEPHALUS SCORPIUS"
# combined[combined$COMM == "MULLER'S PEARLSIDES",]$sciname ="MAUROLICUS MUELLERI"
# combined[combined$COMM == "CEPHALOPODA C.",]$sciname ="CEPHALOPODA C."
# combined[combined$COMM == "SEASNAIL,GELATINOUS",]$sciname ="LIPARIS FABRICII"
# combined[combined$COMM == "RED DEEPSEA CRAB",]$sciname ="CHACEON QUINQUEDENS"
# combined[combined$COMM == "POLYIPNUS ASTEROIDES",]$sciname ="POLYIPNUS ASTEROIDES"
# combined[combined$COMM == "MUNIDA IRIS",]$sciname ="MUNIDA IRIS"
# combined[combined$COMM == "AMERICAN PLAICE",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "WHELKS",]$sciname ="WHELKS"
# combined[combined$COMM == "RADICIPES GRACILIS",]$sciname ="RADICIPES GRACILIS"
# combined[combined$COMM == "MUNIDOPSIS CURVIROSTRA",]$sciname ="MUNIDOPSIS CURVIROSTRA"
# combined[combined$COMM == "SAND DOLLARS",]$sciname ="SAND DOLLARS"
# combined[combined$COMM == "BOBTAIL SQUID",]$sciname ="SEMIROSSIA TENERA"
# combined[combined$COMM == "APHRODITA SP.",]$sciname ="APHRODITA SP."
# combined[combined$COMM == "PTERASTER MILITARIS",]$sciname ="PTERASTER MILITARIS"
# combined[combined$COMM == "HIPPASTERIA PHRYGIANA",]$sciname ="HIPPASTERIA PHRYGIANA"
# combined[combined$COMM == "SEA POTATO",]$sciname ="BOLTENIA SP."
# combined[combined$COMM == "UNID REMAINS,DIGESTED",]$sciname ="NA"
# combined[combined$COMM == "PAGUROIDEA S.F.",]$sciname ="PAGUROIDEA S.F."
# combined[combined$COMM == "THREESPINE STICKLEBACK",]$sciname ="GASTEROSTEUS ACULEATUS ACULEATUS"
# combined[combined$COMM == "DRAGONETS",]$sciname ="CALLIONYMUS LYRA"
# combined[combined$COMM == "BARNACLES",]$sciname ="BARNACLES"
# combined[combined$COMM == "BRITTLE STAR",]$sciname ="BRITTLE STAR"
# combined[combined$COMM == "GOITRE BLACKSMELT",]$sciname ="BATHYLAGUS EURYOPS"
# combined[combined$COMM == "SEA SQUIRTS",]$sciname ="ASCIDIACEA C."
# combined[combined$COMM == "PANDALIDAE F.",]$sciname ="PANDALUS SP."
# combined[combined$COMM == "SEA ANEMONE",]$sciname ="SEA ANEMONE"
# combined[combined$COMM == "ARGIS DENTATA",]$sciname ="ARGIS DENTATA"
# combined[combined$COMM == "SEA CORN",]$sciname ="WHELKS"
# combined[combined$COMM == "MYSID SHRIMP",]$sciname ="MYSID SHRIMP"
# combined[combined$COMM == "SPIRONTOCARIS LILJEBORGII",]$sciname ="SPIRONTOCARIS LILJEBORGII"
# combined[combined$COMM == "TUNICATA S.P.",]$sciname ="TUNICATA S.P."
# combined[combined$COMM == "SEASNAIL,DUSKY",]$sciname ="LIPARIS GIBBUS"
# combined[combined$COMM == "LEBBEUS GROENLANDICUS",]$sciname ="LEBBEUS GROENLANDICUS"
# combined[combined$COMM == "FLOUNDER UNIDENTIFIED",]$sciname ="FLOUNDER (UNIDENTIFIED)"
# combined[combined$COMM == "SNAILS AND SLUGS",]$sciname ="SNAILS AND SLUGS"
# combined[combined$COMM == "RUSSIAN HATS",]$sciname ="VAZELLA POURTALESI"
# combined[combined$COMM == "AMPHIPODA O.",]$sciname ="AMPHIPODA O."
# combined[combined$COMM == "Northern Krill",]$sciname ="MEGANYCTIPHANES NORVEGICA"
# combined[combined$COMM == "CRANGON SEPTEMSPINOSA",]$sciname ="CRANGON SEPTEMSPINOSA"
# combined[combined$COMM == "HEART URCHIN",]$sciname ="SPATANGOIDA O."
# combined[combined$COMM == "BASKET STARS",]$sciname ="EURYALINA S.O."
# combined[combined$COMM == "SPIRONTOCARIS SPINUS",]$sciname ="SPIRONTOCARIS SPINUS"
# combined[combined$COMM == "LONGSPINE SNIPEFISH",]$sciname ="MACRORAMPHOSUS SCOLOPAX"
# combined[combined$COMM == "KRILL SHRIMP",]$sciname ="EUPHAUSIACEA O."
# combined[combined$COMM == "SOFT CORAL UNIDENTIFIED",]$sciname ="SOFT CORAL (UNIDENTIFIED)"
# combined[combined$COMM == "SKATE UNID. EGGS",]$sciname ="SKATE EGGS (UNIDENTIFIED)"
# combined[combined$COMM == "SEA CAULIFLOWER, STRAWBERRIES",]$sciname ="GERSEMIA RUBIFORMIS"
# combined[combined$COMM == "SYSCENUS INFELIX",]$sciname ="SYSCENUS INFELIX"
# combined[combined$COMM == "MUD STAR",]$sciname ="CTENODISCUS CRISPATUS"
# combined[combined$COMM == "SPINY SUNSTAR",]$sciname ="CROSSASTER PAPPOSUS"
# combined[combined$COMM == "PSEUDARCHASTER PARELII",]$sciname ="PSEUDARCHASTER PARELII"
# combined[combined$COMM == "EUALUS GAIMARDII",]$sciname ="EUALUS GAIMARDII"
# combined[combined$COMM == "DIPLOPTERASTER MULTIPES",]$sciname ="DIPLOPTERASTER MULTIPES"
# combined[combined$COMM == "NEMATODA C.",]$sciname ="NEMATODA C."
# combined[combined$COMM == "FIFTH UNIDENTIFIED PER SET",]$sciname ="FIFTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "SQUID (NS)",]$sciname ="TEUTHIDA O."
# combined[combined$COMM == "MUSSELS (NS)",]$sciname ="BIVALVIA C."
# combined[combined$COMM == "ASTERIAS SP.",]$sciname ="ASTERIAS SP."
# combined[combined$COMM == "NORTHERN MOONSNAIL",]$sciname ="EUSPIRA HEROS"
# combined[combined$COMM == "JELLYFISHES",]$sciname ="MEDUSOZOA S.P."
# combined[combined$COMM == "PURPLE SUNSTAR",]$sciname ="SOLASTER ENDECA"
# combined[combined$COMM == "AEGA PSORA",]$sciname ="AEGA PSORA"
# combined[combined$COMM == "PASIPHAEIDAE F.",]$sciname ="PASIPHAEIDAE F."
# combined[combined$COMM == "JELLYFISH",]$sciname ="MEDUSOZOA S.P."
# combined[combined$COMM == "ASTERIAS RUBENS",]$sciname ="ASTERIAS RUBENS"
# combined[combined$COMM == "BLOOD STAR",]$sciname ="HENRICIA LEVIUSCULA"
# combined[combined$COMM == "PSILASTER ANDROMEDA",]$sciname ="PSILASTER ANDROMEDA"
# combined[combined$COMM == "RED ISOPOD",]$sciname ="ISOPODA O."
# combined[combined$COMM == "MOLLUSCA P.",]$sciname ="MOLLUSCA P."
# combined[combined$COMM == "BIVALVIA C.",]$sciname ="BIVALVIA C."
# combined[combined$COMM == "FLABELLUM ALABASTRUM",]$sciname ="FLABELLUM ALABASTRUM"
# combined[combined$COMM == "LONGTOOTH ANGLEMOUTH",]$sciname ="SIGMOPS ELONGATUS"
# combined[combined$COMM == "LAMPANYCTUS MACDONALDI",]$sciname ="LAMPANYCTUS MACDONALDI"
# combined[combined$COMM == "SHORT BARRACUDINA",]$sciname ="MACROPARALEPIS BREVIS"
# combined[combined$COMM == "EIGHTH UNIDENTIFIED PER SET",]$sciname ="EIGHTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "ACANTHEPHYRA PELAGICA",]$sciname ="ACANTHEPHYRA PELAGICA"
# combined[combined$COMM == "PORTUGUESE SHARK",]$sciname ="CENTROSCYMNUS COELOLEPIS"
# combined[combined$COMM == "SCOPELOSAURUS LEPIDUS",]$sciname ="SCOPELOSAURUS LEPIDUS"
# combined[combined$COMM == "SHRIMP",]$sciname ="PANDALUS SP."
# combined[combined$COMM == "FOURTH UNIDENTIFIED PER SET",]$sciname ="FOURTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "LONGNOSE CHIMERA",]$sciname ="RHINOCHIMAERIDAE F."
# combined[combined$COMM == "OGREFISH",]$sciname ="ANOPLOGASTER CORNUTA"
# combined[combined$COMM == "SERGESTES ARCTICUS",]$sciname ="SERGESTES ARCTICUS"
# combined[combined$COMM == "PORANIOMORPHA HISPIDA",]$sciname ="PORANIOMORPHA HISPIDA"
# combined[combined$COMM == "STOUT SAWPALATE",]$sciname ="SERRIVOMER BEANII"
# combined[combined$COMM == "HENRICA SP",]$sciname ="HENRICA SP."
# combined[combined$COMM == "PIPEFISH UNIDEN.",]$sciname ="SYNGNATHINAE F. (UNIDENTIFIED)"
# combined[combined$COMM == "4-LINE SNAKE BLENNY",]$sciname ="EUMESOGRAMMUS PRAECISUS"
# combined[combined$COMM == "LEPTASTERIAS POLARIS",]$sciname ="LEPTASTERIAS POLARIS"
# combined[combined$COMM == "EUALUS FABRICII",]$sciname ="EUALUS FABRICII"
# combined[combined$COMM == "SEA CUCUMBER (COMMON)",]$sciname ="HOLOTHUROIDEA C."
# combined[combined$COMM == "HYPERIIDAE F.",]$sciname ="HYPERIIDAE F."
# combined[combined$COMM == "SEA SLUGS",]$sciname ="SEA SLUGS"
# combined[combined$COMM == "LEBBEUS POLARIS",]$sciname ="LEBBEUS POLARIS"
# combined[combined$COMM == "COMB JELLIES",]$sciname ="	CTENOPHORA P."
# combined[combined$COMM == "PORANIA PULVILIS",]$sciname ="PORANIA PULVILIS"
# combined[combined$COMM == "GOLD-BANDED/BAMBOO CORAL",]$sciname ="ISIDIDAE F."
# combined[combined$COMM == "CEREMASTER GRANULARIS",]$sciname ="CEREMASTER GRANULARIS"
# combined[combined$COMM == "EGGS UNID.",]$sciname ="EGGS (UNIDENTIDIED)"
# combined[combined$COMM == "BLOOD STAR (GENUS)",]$sciname ="HENRICIA LEVIUSCULA"
# combined[combined$COMM == "OPHIURA SARSI",]$sciname ="OPHIURA SARSI"
# combined[combined$COMM == "SPINDLE SHELL",]$sciname ="FASCIOLARIIDA F."
# combined[combined$COMM == "MOON SNAIL UNSP",]$sciname ="NATICIDAE F. (UNIDENTIFIED)"
# combined[combined$COMM == "NEW ENGLAND NEPTUNE",]$sciname ="NEPTUNEA LYRATA"
# combined[combined$COMM == "CUP CORAL",]$sciname ="DENDROPHYLLIIDAE F."
# combined[combined$COMM == "PASIPHAEA MULTIDENTATA",]$sciname ="PASIPHAEA MULTIDENTATA"
# combined[combined$COMM == "TONGUE FISH",]$sciname ="CYNOGLOSSIDAE F."
# combined[combined$COMM == "SEA SPIDER",]$sciname ="	PANTOPODA O."
# combined[combined$COMM == "LEBBEUS SP.",]$sciname ="LEBBEUS SP."
# combined[combined$COMM == "HYALINOECIA CF TUBICOLA",]$sciname ="HYALINOECIA TUBICOLA"
# combined[combined$COMM == "DEAD MANS FINGERS",]$sciname ="ALCYONIUM DIGITATUM"
# combined[combined$COMM == "BLUE ANTIMORA/HAKE",]$sciname ="ANTIMORA ROSTRATA"
# combined[combined$COMM == "COMMON MUSSELS",]$sciname ="MYTILUS EDULIS"
# combined[combined$COMM == "DUCKBILL BARRACUDINA",]$sciname ="MAGNISUDIS ATLANTICA"
# combined[combined$COMM == "BANK CLAM",]$sciname ="CYRTODARIA SILIQUA"
# combined[combined$COMM == "PURSE WINTER SKATE",]$sciname ="LEUCORAJA OCELLATA"
# combined[combined$COMM == "FIRST UNIDENTIFIED PER SET",]$sciname ="FIRST UNIDENTIFIED PER SET"
# combined[combined$COMM == "FOREIGN ARTICLES,GARBAGE",]$sciname ="FOREIGN ARTICLES,GARBAGE"
# combined[combined$COMM == "NYMPHON SP.",]$sciname ="NYMPHON SP."
# combined[combined$COMM == "BAR,SURF CLAM",]$sciname ="SPISULA SOLIDISSIMA"
# combined[combined$COMM == "DUCKBILL OCEANIC EEL",]$sciname ="NESSORHAMPHUS INGOLFIANUS"
# combined[combined$COMM == "SERGIA SP.",]$sciname ="SERGIA SP."
# combined[combined$COMM == "GRENADIERS (NS)",]$sciname ="MACROURIDAE F."
# combined[combined$COMM == "POROMITRA CRASSICEPS",]$sciname ="POROMITRA CRASSICEPS"
# combined[combined$COMM == "DAISY",]$sciname ="DAISY"
# combined[combined$COMM == "SMOOTHHEAD,AGASSIZ'S",]$sciname ="LEPTOCHILICHTHYS AGASSIZII"
# combined[combined$COMM == "APRISTURUS SP.",]$sciname ="APRISTURUS SP"
# combined[combined$COMM == "KNIFENOSE CHIMERA",]$sciname ="RHINOCHIMAERA ATLANTICA"
# combined[combined$COMM == "TOMCOD(ATLANTIC)",]$sciname ="MICROGADUS TOMCOD"
# combined[combined$COMM == "SEA MOUSE",]$sciname ="APHRODITA HASTATA"
# combined[combined$COMM == "PORANIOMORPHA BOREALIS",]$sciname ="PORANIOMORPHA BOREALIS"
# combined[combined$COMM == "LAMPSHELLS",]$sciname ="ARTICULATA"
# combined[combined$COMM == "WHELK EGGS (NS)",]$sciname ="WHELK EGGS"
# combined[combined$COMM == "HARMOTHOE SP.",]$sciname ="HARMOTHOE SP."
# combined[combined$COMM == "RIBBED SCULPIN",]$sciname ="TRIGLOPS PINGELII"
# combined[combined$COMM == "GEODIA SPP.",]$sciname ="GEODIA SP."
# combined[combined$COMM == "BUBBLE GUM CORAL",]$sciname ="PARAGORGIA ARBOREA"
# combined[combined$COMM == "SEA PEN",]$sciname ="PENNATULACEA"
# combined[combined$COMM == "SABINEA SP.",]$sciname ="SABINEA SP."
# combined[combined$COMM == "YOLDIA SAPOTILLA",]$sciname ="YOLDIA SAPOTILLA"
# combined[combined$COMM == "WAVE WHELK,COMMON EDIBLE",]$sciname ="BUCCINUM UNDATUM"
# combined[combined$COMM == "SABINEA SARSI",]$sciname ="SABINEA SARSI"
# combined[combined$COMM == "GLACIER LANTERNFISH",]$sciname ="BENTHOSEMA GLACIALE"
# combined[combined$COMM == "MOLPADIA SP.",]$sciname ="MOLPADIA SP."
# combined[combined$COMM == "APRISTURUS LAURUSSONI",]$sciname ="APRISTURUS LAURUSSONI"
# combined[combined$COMM == "OCEAN QUAHAUG",]$sciname ="ARCTICA ISLANDICA"
# combined[combined$COMM == "MARGARITES COSTALIS",]$sciname ="MARGARITES COSTALIS"
# combined[combined$COMM == "ATLANTOPANDALUS PROPINQUUS",]$sciname ="ATLANTOPANDALUS PROPINQVUS"
# combined[combined$COMM == "HYDROZOA",]$sciname ="HYDROZOA C."
# combined[combined$COMM == "LANTERNFISH,HORNED",]$sciname ="CENTROPHRYNE SPINULOSA"
# combined[combined$COMM == "LEPTASTERIAS SP.",]$sciname ="LEPTASTERIAS SP."
# combined[combined$COMM == "SCARLETT PSOLUS",]$sciname ="SCARLETT PSOLUS"
# combined[combined$COMM == "PELICAN GULPER",]$sciname ="EURYPHARYNX PELECANOIDES"
# combined[combined$COMM == "ARISTOSTOMIAS SP.",]$sciname ="ARISTOSTOMIAS SP."
# combined[combined$COMM == "ROUGH SAGRE",]$sciname ="ETMOPTERUS PRINCEPS"
# combined[combined$COMM == "MASTIGOTEUTHIS SP.",]$sciname ="MASTIGOTEUTHIS SP."
# combined[combined$COMM == "LIMPET (NS)",]$sciname ="PATELLIDAR F."
# combined[combined$COMM == "PURSE SMOOTH SKATE",]$sciname ="MALACORAJA SENTA"
# combined[combined$COMM == "ISOPOD",]$sciname = "ISOPODA O."
# combined[combined$COMM == "EUPHAUSIIDAE F.",]$sciname ="EUPHAUSIIDAE F."
# combined[combined$COMM == "SCLEROCRANGON BOREAS",]$sciname ="SCLEROCRANGON BOREAS"
# #combined[combined$COMM == "TEALIA FELINA",]$sciname ="URTICINA FELINA"
# combined[combined$COMM == "METRIDIUM SENILE",]$sciname ="METRIDIUM SENILE"
# combined[combined$COMM == "PONTOPHILUS NORVEGICUS",]$sciname ="PONTOPHILUS NORVEGICUS"
# combined[combined$COMM == "PSEUDARCHASTER SP",]$sciname ="PSEUDARCHASTER SP."
# combined[combined$COMM == "CORAL (NS)",]$sciname ="ANTHOZOA C."
# combined[combined$COMM == "ALDROVANDIA SP",]$sciname ="ALDROVANDIA SP."
# combined[combined$COMM == "STRAIGHTLINE DRAGONFISH",]$sciname ="BOROSTOMIAS ANTARCTICUS"
# combined[combined$COMM == "BATHYPTEROIS LONGIPES",]$sciname ="BATHYPTEROIS LONGIPES"
# combined[combined$COMM == "SOFT SHELL CLAM",]$sciname =" MYA ARENARIA"
# combined[combined$COMM == "SPOTTED LANTERNFISH",]$sciname ="MYCTOPHUM PUNCTATUM"
# combined[combined$COMM == "HORSE MUSSELS",]$sciname ="MODIOLUS MODIOLUS"
# combined[combined$COMM == "SPINY SKINNED ANIMALS",]$sciname ="ECHINODERMATA P."
# combined[combined$COMM == "BANDED GUNNEL",]$sciname ="PHOLIS FASCIATA"
# combined[combined$COMM == "LOPHASTER FURCIFER",]$sciname ="LOPHASTER FURCIFER"
# combined[combined$COMM == "Skate;LITTLE OR WINTER; Unspecified",]$sciname ="RAJIDAE F."
# combined[combined$COMM == "TONGUEFISH",]$sciname ="CYNOGLOSSIDAE F."
# combined[combined$COMM == "BATHYSAURUS FEROX",]$sciname ="BATHYSAURUS FEROX"
# combined[combined$COMM == "SLICKHEAD",]$sciname ="ALEPOCEPHALIDAE F."
# combined[combined$COMM == "THIRD UNIDENTIFIED PER SET",]$sciname ="THIRD UNIDENTIFIED PER SET"
# combined[combined$COMM == "BEANS BLUEBACK",]$sciname ="ALOSA AESTIVALIS"
# combined[combined$COMM == "SEVENTH UNIDENTIFIED PER SET",]$sciname ="SEVENTH UNIDENTIFIED PER SET"
# combined[combined$COMM == "PYCNOGONIDAE O.",]$sciname ="PYCNOGONIDAE O."
# combined[combined$COMM == "DAINTY MORA",]$sciname ="HALARGYREUS JOHNSONII"
# combined[combined$COMM == "ALDROVANDIA PHALACRA",]$sciname ="ALDROVANDIA PHALACRA"
# combined[combined$COMM == "LEPIDONOTUS SQUAMATUS",]$sciname ="LEPIDONOTUS SQUAMATUS"
# combined[combined$COMM == "ANOMIA SP.",]$sciname ="ANOMIA SP."
# combined[combined$COMM == "NEZUMIA SP.",]$sciname ="NEZUMIA SP."
# combined[combined$COMM == "ASTERIAS FORBESI",]$sciname ="ASTERIAS FORBESI"
# combined[combined$COMM == "SEGMENTED WORMS",]$sciname ="ANNELIDA P."
# combined[combined$COMM == "OPHIACANTHA ABYSSICOLA",]$sciname ="OPHIACANTHA ABYSSICOLA"
# combined[combined$COMM == "STOUT EELBLENNY",]$sciname ="ANISARCHUS MEDIUS"
# combined[combined$COMM == "ACANTHOGORGIA ARMATA",]$sciname ="ACANTHOGORGIA ARMATA"
# combined[combined$COMM == "REDFISH",]$sciname ="SEBASTES MENTELLA"
# combined[combined$COMM == "SPOTTED HAKE",]$sciname ="UROPHYCIS REGIUS"
# combined[combined$COMM == "SMALL POLYCHAETE )3MM DIA",]$sciname ="NA"
# combined[combined$COMM == "BATHYRAJA SP",]$sciname ="	BATHYRAJA SP."
# combined[combined$COMM == "URASTERIAS LINCKI",]$sciname ="URASTERIAS LINCKI"
# combined[combined$COMM == "BROWN ROCKWEED",]$sciname ="ASCOPHYLLUM NODOSUM"
# combined[combined$COMM == "DERICHTHYS SERPENTINUS",]$sciname ="DERICHTHYS SERPENTINUS"
# combined[combined$COMM == "ARMORED SEA ROBIN",]$sciname ="PERISTEDION MINIATUM"
# combined[combined$COMM == "SKATES AND RAYS (NS)",]$sciname ="SKATES AND RAYS"
# combined[combined$COMM == "SEA BASSES",]$sciname ="CENTROPRISTIS STRIATA"
# combined[combined$COMM == "GREENLAND COD",]$sciname ="GADUS OGAC"
# combined[combined$COMM == "HYDROZOA C.",]$sciname ="HYDROZOA C."
# combined[combined$COMM == "MORRHUA VENUSNA",]$sciname ="PITAR MORRHUANA"
# combined[combined$COMM == "EPITONIUM",]$sciname ="EPITONIUM SP."
# combined[combined$COMM == "BLENNIES,SHANNIES,GUNNELS",]$sciname ="BLENNIIDAE,LIPOPHRYS,PHOLIS SP."
# combined[combined$COMM == "COMMON WOLF EEL",]$sciname ="LYCENCHELYS PAXILLUS"
# combined[combined$COMM == "SEA GRAPES",]$sciname ="MOLGULA MANHATTENSIS"
# combined[combined$COMM == "ANGLEMOUTH",]$sciname ="CYCLOTHONE SP."
# combined[combined$COMM == "ARCTIC STAGHORN SCULPIN",]$sciname ="GYMNOCANTHUS TRICUSPIS"
# combined[combined$COMM == "LANTERNFISH PATCHWORK",]$sciname ="NOTOSCOPELUS RESPLENDENS"
# combined[combined$COMM == "COPEPODA S.C.",]$sciname ="COPEPODA S.C."
# combined[combined$COMM == "AMERICAN STRAPTAIL GRENADIER",]$sciname ="MALACOCEPHALUS OCCIDENTALIS"
# combined[combined$COMM == "ARISTAEPSIS EDWARDSINANA",]$sciname ="ARISTAEOPSIS EDWARDSIANA"
# combined[combined$COMM == "LAEMONEMA BARBATULUM",]$sciname ="LAEMONEMA BARBATULUM"
# combined[combined$COMM == "MYCTOPHIFORMES",]$sciname ="MYCTOPHIFORMES O."
# combined[combined$COMM == "BLOOD WORMS",]$sciname ="GLYCERA SP."
# combined[combined$COMM == "LEMONWEED",]$sciname ="LITHOSPERMUM RUDERALE"
# combined[combined$COMM == "ANGLEMOUTH (NS)",]$sciname ="CYCLOTHONE SP."
# combined[combined$COMM == "HAKE (NS)",]$sciname ="UROPHYCSIS SP."
# combined[combined$COMM == "HALIPTERUS (BALTICINA) SP.",]$sciname ="HELIPTERUS SP."
# combined[combined$COMM == "SQUID EGGS",]$sciname ="ILLEX SP. EGGS"
# combined[combined$COMM == "SEAROBINS",]$sciname ="PERISTEDION MINIATUM"
# combined[combined$COMM == "SPIRONTOCARIS PHIPPSII",]$sciname ="SPIRONTOCARIS PHIPPSII"
# combined[combined$COMM == "SKATES (NS)",]$sciname ="RAJIDAE F."
# combined[combined$COMM == "STRIPED SEASNAIL",]$sciname ="LIPARIS LIPARIS"
# combined[combined$COMM == "PORCUPINE CRAB",]$sciname ="NEOLITHODES GRIMALDII"
# combined[combined$COMM == "ARCTIC ALLIGATORFISH",]$sciname ="ASPIDOPHOROIDES OLRIKII"
# combined[combined$COMM == "AMERICAN EEL",]$sciname ="ANGUILLA ROSTRATA"
# combined[combined$COMM == "UNID. FISH",]$sciname ="UNIDENTIFIED FISH"
# combined[combined$COMM == "SPATULATE SCULPIN",]$sciname ="ICELUS SPATULA"
# combined[combined$COMM == "VACHON'S EELPOUT",]$sciname ="LYCODES ESMARKII"
# combined[combined$COMM == "ROUGHHEAD GRENADIER",]$sciname ="MACROURUS BERGLAX"
# combined[combined$COMM == "SEA TADPOLE",]$sciname ="CAREPROCTUS REINHARDTI"
# combined[combined$COMM == "PAGURUS SP.",]$sciname ="PAGURUS SP."
# combined[combined$COMM == "SWORDFISH",]$sciname ="XIPHIAS GLADIUS"
# combined[combined$COMM == "PLANEHEAD FILEFISH",]$sciname ="STEPHANOLEPIS HISPIDUS"
# #combined[combined$COMM == "CRANGONIDAE F.",]$sciname ="CRANGONIDAE F."
# combined[combined$COMM == "AXIUS SERRATUS",]$sciname ="AXIUS SERRATUS"
# combined[combined$COMM == "TILE FISH",]$sciname ="MALACANTHIDAE F."
# combined[combined$COMM == "ARGENTINES (NS)",]$sciname ="ARGENTINA SP."
# combined[combined$COMM == "SPIRONTOCARIS",]$sciname ="SPIRONTOCARIS SP."
# combined[combined$COMM == "LANTERNFISH",]$sciname ="DIAPHUS DUMERILII"
# combined[combined$COMM == "CHRYSOGORGIA AGASSIZII",]$sciname ="CHRYSOGORGIA AGASSIZII"
# combined[combined$COMM == "LARGESCALE LANTERNFISH",]$sciname ="SYMBOLOPHORUS VERANYI"
# combined[combined$COMM == "SHORTSPINE TAPIRFISH",]$sciname ="POLYACANTHONOTUS RISSOANUS"
# combined[combined$COMM == "DICROLENE INTRONIGER",]$sciname ="DICROLENE INTRONIGER"
# combined[combined$COMM == "HALOSAUROPSIS MACROCHIR",]$sciname ="HALOSAUROPSIS MACROCHIR"
# combined[combined$COMM == "AMERICAN BARRELFISH",]$sciname ="HYPEROGLYPHE PERCIFORMIS"
# combined[combined$COMM == "OPHIURA SP.",]$sciname ="OPHIURA SP."
# combined[combined$COMM == "LEPTYCHASTER ARCTICUS",]$sciname ="LEPTYCHASTER ARCTICUS"
# combined[combined$COMM == "RONDELETIA LORICATA",]$sciname ="RONDELETIA LORICATA"
# combined[combined$COMM == "CANCER CRAB (NS)",]$sciname ="CANCER SP."
# combined[combined$COMM == "TRANSPARENT HATCHETFISH",]$sciname ="STERNOPTYX DIAPHANA"
# combined[combined$COMM == "SEA PEACH",]$sciname ="HALOCYNTHIA PYRIFORMIS"
# combined[combined$COMM == "SEALS (NS)",]$sciname ="PHOCIDAE F."
# combined[combined$COMM == "EUALUS MACILENTUS",]$sciname ="EUALUS MACILENTUS"
# combined[combined$COMM == "AMAUROPSIS ISLANDICA",]$sciname ="AMAUROPSIS ISLANDICA"
# combined[combined$COMM == "SOFT SHELL OR LONG NECK CLAM",]$sciname ="MYA ARENARIA"
# combined[combined$COMM == "SABELLIDAE F.",]$sciname ="SABELLIDAE F."
# combined[combined$COMM == "ATLANTIC GYMNAST",]$sciname ="XENODERMICHTHYS COPEI"
# combined[combined$COMM == "MIRROR LANTERNFISH",]$sciname ="LAMPADENA SPECULIGERA"
# combined[combined$COMM == "PURSE LITTLE SKATE",]$sciname ="LEUCORAJA ERINACEA"
# combined[combined$COMM == "PSOLUS PHANTAPUS",]$sciname ="PSOLUS PHANTAPUS"
# combined[combined$COMM == "SECOND UNIDENTIFIED PER SET",]$sciname ="SECOND UNIDENTIFIED PER SET"
# combined[combined$COMM == "BLACK SWALLOWER",]$sciname ="CHIASMODON NIGER"
# combined[combined$COMM == "BAIRDS SMOOTHEAD",]$sciname ="ALEPOCEPHALUS BAIRDII"
# combined[combined$COMM == "ALDROVANDIA AFFINIS",]$sciname ="ALDROVANDIA AFFINIS"
# combined[combined$COMM == "EELS, CUTTHROAT (NS)",]$sciname ="SYNAPHOBRANCHUS KAUPII"
# combined[combined$COMM == "NORTHERN PUFFER",]$sciname ="SPHOEROIDES MACULATU"
# combined[combined$COMM == "EELPOUTS (NS)",]$sciname ="LYCODES SP."
# combined[combined$COMM == "RAZOR CLAM",]$sciname ="ENSIS ARCUATUS"
# combined[combined$COMM == "NOTOSCOPELUS BOLINI",]$sciname ="NOTOSCOPELUS BOLINI"
# combined[combined$COMM == "BRACHIURAN CRABS",]$sciname ="CANCER SP."
# combined[combined$COMM == "CERATOSCOPELUS SP.",]$sciname ="CERATOSCOPELUS SP."
# combined[combined$COMM == "GREENLAND COCKLE",]$sciname ="SERRIPES GROENLANDICUS"
# combined[combined$COMM == "ASTARTE UNDATA",]$sciname ="ASTARTE UNDATA"
# combined[combined$COMM == "ILLEX SP.",]$sciname ="ILLEX SP."
# combined[combined$COMM == "PUFFER",]$sciname ="SPHOEROIDES SP."
# combined[combined$COMM == "DRAGONFISH,BIGHEAD (NS)",]$sciname ="STOMIAS BOA FEROX"
# combined[combined$COMM == "HOLTBYRNIA ANOMALA",]$sciname ="HOLTBYRNIA ANOMALA"
# combined[combined$COMM == "ABYSSAL SKATE",]$sciname ="RAJELLA BATHYPHILA"
# combined[combined$COMM == "FUNICULINA QUADRANGULARIS",]$sciname ="FUNICULINA QUADRANGULARIS"
# combined[combined$COMM == "DUCK OR PELICAN FOOT",]$sciname ="NA"
# combined[combined$COMM == "BRYOZOA P.",]$sciname ="BRYOZOA P."
# combined[combined$COMM == "INQUILINE SEASNAIL",]$sciname ="LIPARIS INQUILINUS"
# combined[combined$COMM == "SCALLOP SHELLS",]$sciname ="PLACOPECTEN MAGELLANICUS SHELLS"
# combined[combined$COMM == "NEPHTYIDFAE F.",]$sciname ="NEPHTYIDAE F."
# combined[combined$COMM == "LARGE POLYCHAETE, 3MM DIA",]$sciname ="POLYCHAETA C."
# combined[combined$COMM == "NUCELLA LAPILLUS",]$sciname ="NUCELLA LAPILLUS"
# combined[combined$COMM == "SEAWEED,(ALGAE),KELP",]$sciname ="SEAWEED,ALGAE,KELP"
# combined[combined$COMM == "SALPIDAE F.",]$sciname ="SALPIDAE F."
# combined[combined$COMM == "DEEPBODY BOARFISH",]$sciname ="ANTIGONIA CAPROS"
# combined[combined$COMM == "CRAB",]$sciname ="DECAPODA O."
# combined[combined$COMM == "BRIER SKATE",]$sciname ="RAJA EGLANTERIA"
# combined[combined$COMM == "BLUE WHITING",]$sciname ="MICROMESISTIUS POUTASSOU"
# combined[combined$COMM == "GULF SEA SNAIL",]$sciname ="UROSALPINX CINEREA"
# combined[combined$COMM == "RED DORY",]$sciname ="CYTTUS ROSEUS"
# combined[combined$COMM == "SNAKE EEL",]$sciname ="OPHICHUTHUS SP."
# combined[combined$COMM == "SEAWEED BLENNY",]$sciname ="PARABLENNIUS MARMOREUS"
# combined[combined$COMM == "GREENLAND SEASNAIL",]$sciname ="MARGARITES GROENLANDICUS"
# combined[combined$COMM == "CONGER EEL",]$sciname ="CONGER OCEANICUS"
# combined[combined$COMM == "OCTOPOTEUTHIS SP.",]$sciname ="OCTOPOTEUTHIS SP."
# combined[combined$COMM == "NOTCH FEELERFISH",]$sciname ="BATHYPTEROIS DUBIUS"
# combined[combined$COMM == "ANTHOMASTUS GRANDIFLORUS",]$sciname ="ANTHOMASTUS GRANDIFLORUS"
# combined[combined$COMM == "MYCTOPHUM SP.",]$sciname ="MYCTOPHUM SP."
# combined[combined$COMM == "MUNIDA VALIDA",]$sciname ="MUNIDA VALIDA"
# combined[combined$COMM == "STONES AND ROCKS",]$sciname ="STONES AND ROCKS"
# combined[combined$COMM == "SEA CORALS (NS)",]$sciname ="ANTHOZOA C."
# combined[combined$COMM == "DRAGONFISH UNIDENTIFIED",]$sciname ="STOMIAS SP. (UNIDENTIFIED)"
# combined[combined$COMM == "MOLGULIDAE F.",]$sciname ="MOLGULIDAE F."
# combined[combined$COMM == "GONOSTOMA BATHYPHILUM",]$sciname ="GONOSTOMA BATHYPHILUM"
# combined[combined$COMM == "GNATHOPHAUSIA SP.",]$sciname ="GNATHOPHAUSIA SP."
# combined[combined$COMM == "LOOSEJAW",]$sciname ="MALACOSTEUS NIGER"
# combined[combined$COMM == "PARAPASIPHAEA SULCATIFRONS",]$sciname ="PARAPASIPHAEA SULCATIFRO"
# combined[combined$COMM == "COCKLES",]$sciname ="CARDIIDAE F."
# combined[combined$COMM == "STAUROTEUTHIDAE F.",]$sciname ="STAUROTEUTHIDAE F"
# combined[combined$COMM == "GORGONOCEPHALUS SP.",]$sciname ="GORGONOCEPHALUS SP."
# combined[combined$COMM == "GIANT CANOE BUBBLE",]$sciname ="SCAPHANDER PUNCTOSTRIATUS"
# combined[combined$COMM == "NYBELIN S SCULPIN",]$sciname ="TRIGLOPS NYBELINI"
# combined[combined$COMM == "PERIWINKLES",]$sciname ="LITTORINIDAE F."
# combined[combined$COMM == "GONATUS SP.",]$sciname ="GONATUS SP."
# combined[combined$COMM == "SHORTTAIL SKATE",]$sciname ="AMBLYRAJA JENSENI"
# combined[combined$COMM == "COPEPOD",]$sciname ="COPEPOD S.C."
# combined[combined$COMM == "STAGHORN BRYOZOA",]$sciname ="BRYOZOA P."
# combined[combined$COMM == "LANTERNFISH KROYER'S",]$sciname ="NOTOSCOPELUS ELONGATUS"
# combined[combined$COMM == "BARRACUDINA (NS)",]$sciname ="BARRACUDINA"
# combined[combined$COMM == "PHOTOSTOMIAS GUERNEI",]$sciname ="PHOTOSTOMIAS GUERNEI"
# combined[combined$COMM == "SABINEA SEPTEMCARINATA",]$sciname ="SABINEA SEPTEMCARINATA"
# combined[combined$COMM == "SOUTHERN SEA BASS",]$sciname ="CENTROPRISTIS STRIATA"
# combined[combined$COMM == "SILKY BUCCINUM",]$sciname ="BUCCINUM SP."
# combined[combined$COMM == "RAZOR SHELL CLAM",]$sciname ="ENSIS ARCUATUS"
# combined[combined$COMM == "PORBEAGLE,MACKEREL SHARK",]$sciname ="LAMNA NASUS"
# combined[combined$COMM == "EUPHAUSIA SP.",]$sciname ="EUPHAUSIA SP."
# combined[combined$COMM == "CANCER SP.",]$sciname ="CANCER SP."
# combined[combined$COMM == "HERRING/CAPELIN LIKE",]$sciname ="ACTINOPTERYGII C."
# combined[combined$COMM == "BLACK CORALS THORNY CORALS",]$sciname ="ANTIPATHARIA O."
# combined[combined$COMM == "ARCTIC SHANNY",]$sciname ="STICHAEUS PUNCTATUS PUNCTATUS"
# combined[combined$COMM == "CHITONS",]$sciname ="POLYPLACOPHORA C."
# combined[combined$COMM == "ECHINUS SP.",]$sciname ="ECHINUS SP."
# combined[combined$COMM == "NUT SHELLS",]$sciname ="NUT SHELLS"
# combined[combined$COMM == "CUSK-EELS INCLUDES BROTULIDAE F.",]$sciname ="OPHIDIIDAE F."
# combined[combined$COMM == "ALEPOCEPHALUS SP.",]$sciname ="ALEPOCEPHALUS SP."
# combined[combined$COMM == "SCULPIN (NS)",]$sciname ="COTTIDEA F."
# combined[combined$COMM == "SLENDER ARMED SEA STAR",]$sciname ="LUIDIA CLATHRATA"
# combined[combined$COMM == "BUCCINIDAE F.",]$sciname ="BUCCINIDAE F."
# combined[combined$COMM == "BALANIDAE F.",]$sciname ="BALANIDAE F."
# combined[combined$COMM == "FINFISHES (NS)",]$sciname ="FINFISHES (NS)"
# combined[combined$COMM == "DOGFISHES (NS)",]$sciname ="SQUALIDAE F."
# combined[combined$COMM == "LADY CRAB",]$sciname ="OVALIPES OCELLATUS"
# combined[combined$COMM == "FILAMENT TUBE WORM",]$sciname ="POLYCHAETA C."
# combined[combined$COMM == "ORGANIC DEBRIS",]$sciname ="ORGANIC DEBRIS"
# combined[combined$COMM == "SPINY EEL",]$sciname ="NOTACANTHUS CHEMNITZII"
# combined[combined$COMM == "GRAY TRIGGERFISH",]$sciname ="BALISTES CAPRISCUS"
# combined[combined$COMM == "PRICKLEBACKS",]$sciname ="STICHAEIDAE F."
# combined[combined$COMM == "APHRODITA ACULEATA",]$sciname ="APHRODITA ACULEATA"
# combined[combined$COMM == "LEFTEYE FLOUNDER",]$sciname =" BOTHIDAE F."
# combined[combined$COMM == "ATLANTIC TORPEDO",]$sciname ="TORPEDO NOBILIANA"
# combined[combined$COMM == "SIMONYI'S FROSTFISH",]$sciname ="BENTHODESMUS ELONGATUS SIMONYI"
# combined[combined$COMM == "ACANTHEPHYRA EXEMIA",]$sciname ="ACANTHEPHYRA EXIMIA"
# combined[combined$COMM == "CHIASMODON SP.",]$sciname ="CHIASMODON SP."
# combined[combined$COMM == "AMPHIPOD",]$sciname ="AMPHIPODA O."
# combined[combined$COMM == "HARD CLAM",]$sciname ="MERCENARIA MERCINARIA"
# combined[combined$COMM == "ROCKLING UNIDENTIFIED",]$sciname ="LOTIDAE F."
# combined[combined$COMM == "AMPHIPODS",]$sciname ="AMPHIPODA O."
# combined[combined$COMM == "CRUSTACEA LARVAE",]$sciname ="CRUSTACEA S.P. LARVAE"
# combined[combined$COMM == "SNAIL/SLUG EGGS",]$sciname ="SNAIL AND SLUG EGGS"
# combined[combined$COMM == "STIMPSON'S SURF CLAM, ARCTIC SURF CLAM",]$sciname =" MACTROMERIS POLYNYMA"
# combined[combined$COMM == "NEALOTUS TRIPES",]$sciname ="NEALOTUS TRIPES"
# combined[combined$COMM == "SCUP",]$sciname ="STENOTOMUS CHRYSOPS"
# combined[combined$COMM == "CUBICEPS PAUCIRADIATUS",]$sciname ="CUBICEPS PAUCIRADIATUS"
# combined[combined$COMM == "ECTREPOSEBASTES IMUS",]$sciname ="ECTREPOSEBASTES IMUS"
# combined[combined$COMM == "ASTARTE SP.",]$sciname ="ASTARTE SP."
# combined[combined$COMM == "SPONGE",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "PURSE THORNY SKATE",]$sciname ="RHIZAXINELLA SP."
# combined[combined$COMM == "SPEARFISH REMORA",]$sciname ="REMORA BRACHYPTERA"
# combined[combined$COMM == "CTENOPHORES,COELENTERATES,PORIFERA",]$sciname ="CTENOPHORA P., CNIDARIA P., PORIFERA P."
# combined[combined$COMM == "DOFLEINS LANTERNFISH",]$sciname ="LOBIANCHIA DOFLEINI"
# combined[combined$COMM == "BIGEYE SMOOTH-HEAD",]$sciname ="BAJACALIFORNIA MEGALOPS"
# combined[combined$COMM == "SCOPELOBERYX ROBUSTUS",]$sciname ="SCOPELOBERYX ROBUSTUS"
# combined[combined$COMM == "ANTHOZOA SEA ANEMONES",]$sciname ="ANTHOZOA C."
# combined[combined$COMM == "PENNATULA BOREALIS",]$sciname ="PENNATULA BOREALIS"
# combined[combined$COMM == "GAMMARIDAE F.",]$sciname ="GAMMARIDAE F."
# combined[combined$COMM == "POLYIPNUS SP.",]$sciname ="POLYIPNUS SP."
# combined[combined$COMM == "SMOOTH GRENADIER",]$sciname ="NEZUMIA AEQUALIS"
# combined[combined$COMM == "LIZARDFISH,LARGESCALE",]$sciname ="SAURIDA BRASILIENSIS"
# combined[combined$COMM == "SHORTNOSE LANCETFISH",]$sciname ="ALEPISAURUS BREVIROSTRIS"
# combined[combined$COMM == "TORPEDO DRAGONFISH",]$sciname ="GRAMMATOSTOMIAS DENTATUS"
# combined[combined$COMM == "LEPIDOPHANES GUENTHERI",]$sciname ="LEPIDOPHANES GUENTHERI"
# combined[combined$COMM == "ATLANTIC SILVERSIDE",]$sciname ="MENIDIA MENIDIA"
# combined[combined$COMM == "SEASNAIL",]$sciname ="GASTROPODA C."
# combined[combined$COMM == "PHRONIMA SP.",]$sciname ="PHRONIMA SP."
# combined[combined$COMM == "LAMPANYCTUS SP.",]$sciname ="LAMPANYCTUS SP."
# combined[combined$COMM == "BARNDOOR SKATE",]$sciname ="DIPTURUS LAEVIS"
# combined[combined$COMM == "FLATFISH",]$sciname ="PLEURONECIFORMES O."
# combined[combined$COMM == "DAGGERTOOTH",]$sciname ="ANOTOPTERUS PHARAO"
# combined[combined$COMM == "BLACK SCABBARDFISH",]$sciname ="APHANOPUS CARBO"
# combined[combined$COMM == "FAN WORM",]$sciname ="SABELLIDA O."
# combined[combined$COMM == "EPIGONUS DENTICULATUS",]$sciname ="EPIGONUS DENTICULATUS"
# combined[combined$COMM == "SERGIA ROBUSTA",]$sciname ="SERGIA ROBUSTA"
# combined[combined$COMM == "SPIRONTOCARIS FABRICI",]$sciname ="SPIRONTOCARIS FABRICI"
# combined[combined$COMM == "MELAMPHAEIDAE",]$sciname ="MELAMPHAEIDAE F."
# combined[combined$COMM == "SEA SNAILS,SEA BUTTERFLIES, PTEROPODA",]$sciname ="NA"
# combined[combined$COMM == "LONGFIN SEASNAIL",]$sciname ="LIPARIDAE F."
# combined[combined$COMM == "BLENNIES (NS)",]$sciname ="BLENNIIDAE F."
# combined[combined$COMM == "SPIDER HAZARDS CORAL",]$sciname ="LOPHELIA PERTUSA"
# combined[combined$COMM == "SERRIVOMER SP.",]$sciname ="SERRIVOMER SP."
# combined[combined$COMM == "CARIDEA SO.",]$sciname ="CARIDEA S.O."
# combined[combined$COMM == "DEEPSEA CAT SHARK",]$sciname ="APRISTURUS PROFUNDORUM"
# combined[combined$COMM == "CHONE SP.",]$sciname ="CHONE SP."
# combined[combined$COMM == "SILVER ROCKLING",]$sciname ="GAIDROPSARUS ARGENTATUS"
# combined[combined$COMM == "BACKFIN TAPIRFISH",]$sciname ="LIPOGENYS GILLII"
# combined[combined$COMM == "FISH EGGS-UNIDENTIFIED",]$sciname ="NA"
# combined[combined$COMM == "GONATUS STEENSTRUPII",]$sciname ="GONATUS STEENSTRUPII"
# combined[combined$COMM == "BLACKMOUTH BASS",]$sciname ="SYNAGROPS BELLUS"
# combined[combined$COMM == "MUSCULUS NIGER",]$sciname ="MUSCULUS NIGER"
# combined[combined$COMM == "POLAR EELPOUT",]$sciname ="LYCODES POLARIS"
# combined[combined$COMM == "SAND LANCES (NS)",]$sciname ="AMMODYTES AMERICANUS"
# combined[combined$COMM == "LONGNOSE GRENADIER",]$sciname ="COELORINCHUS CARMINATUS"
# combined[combined$COMM == "SEA TADPOLE(NS)",]$sciname ="CAREPROCTUS REINHARDTI "
# combined[combined$COMM == "ATLANTIC MOONFISH",]$sciname ="SELENE SETAPINNIS"
# combined[combined$COMM == "SALMON(ATLANTIC)",]$sciname ="SALMO SALAR"
# combined[combined$COMM == "BRYOZOANS ECTOPROCTA",]$sciname ="BRYOZOAN ECTOPROCTOA"
# combined[combined$COMM == "STOUT BEARD FISH",]$sciname ="POLYMIXIA NOBILIS"
# combined[combined$COMM == "HARMOTHOE EXTENUATA",]$sciname ="HARMOTHOE EXTENUATA"
# combined[combined$COMM == "HYGOPHUM HYGOMI",]$sciname ="HYGOPHUM HYGOMI"
# combined[combined$COMM == "POLYMASTIA SP.",]$sciname ="POLYMASTIA SP."
# combined[combined$COMM == "PANDORA GOULDIANA",]$sciname ="PANDORA GOULDIANA"
# combined[combined$COMM == "BATFISHES",]$sciname ="OGCOCEPHALIDAE F."
# combined[combined$COMM == "OMMASTREPHIDAE F.",]$sciname ="OMMASTREPHIDAE F."
# combined[combined$COMM == "ACANELLA ARBUSCULA",]$sciname ="ACANELLA ARBUSCULA"
# combined[combined$COMM == "SLENDER EELBLENNY",]$sciname ="LUMPENUS FABRICII"
# combined[combined$COMM == "SPINY EELS (NS)",]$sciname ="NOTACANTHUS CHEMNITZII"
# combined[combined$COMM == "DEEPWATER CHIMAERA",]$sciname ="HYDROLAGUS AFFINIS"
# combined[combined$COMM == "CORYPHAENOIDES CARAPINUS",]$sciname ="CORYPHAENOIDES CARAPINU"
# combined[combined$COMM == "CATAETYX LATICEPS",]$sciname ="CATAETYX LATICEPS"
# combined[combined$COMM == "SCULPIN UNIDENTIFIED",]$sciname ="COTTIDEA F."
# combined[combined$COMM == "THORNY TINSELFISH",]$sciname ="GRAMMICOLEPIS BRACHIUSCULUS"
# combined[combined$COMM == "QUAHAUG",]$sciname ="MERCINARIA MERCINARIA"
# combined[combined$COMM == "LOOSEJAWS (NS)",]$sciname ="MALACOSTEUS NIGER"
# combined[combined$COMM == "ROULEINA ATTRITA",]$sciname ="ROULEINA ATTRITA"
# combined[combined$COMM == "BATHYPTEROIS QUADRIFILIS",]$sciname ="BATHYPTEROIS QUADRIFILIS"
# combined[combined$COMM == "XENOLEPIDICHTHYS DALGLEISHI",]$sciname ="XENOLEPIDICHTHYS DALGLEIS"
# combined[combined$COMM == "GONATIDAE F.",]$sciname ="GONATIDAE F."
# combined[combined$COMM == "POLYMASTIA MAMMILARIS",]$sciname ="POLYMASTIA MAMMILARIS"
# combined[combined$COMM == "SMALLMOUTH FLOUNDER",]$sciname ="ETROPUS MICROSTOMUS"
# combined[combined$COMM == "HEART SHELL",]$sciname =" CARDIIDAE F."
# combined[combined$COMM == "POLYNOIDAE F.",]$sciname ="POLYNOIDAE F."
# combined[combined$COMM == "SEMIROSSIA TENERA",]$sciname = "SEMIROSSIA TENERA"
# combined[combined$COMM == "ANOMIIDAE F.",]$sciname ="ANOMIIDAE F."
# combined[combined$COMM == "PURSE BARNDOOR SKATE",]$sciname ="DIPTURUS LAEVIS"
# combined[combined$COMM == "PORTUNIDAE F.",]$sciname ="PORTUNIDAE F."
# combined[combined$COMM == "PARALEPIS ELONGATA",]$sciname ="PARALEPIS ELONGATA"
# combined[combined$COMM == "SCORPIONFISHES",]$sciname ="SCORPAENIDAE F."
# combined[combined$COMM == "PECTINARIA SP.",]$sciname ="PECTINARIA SP."
# combined[combined$COMM == "ANTHOPTILUM GRANDIFLORUM",]$sciname ="ANTHOPTILUM GRANDIFLORUM"
# combined[combined$COMM == "CARDINAL FISHES",]$sciname ="APOGONIDAE F."
# combined[combined$COMM == "PAGURUS POLITUS",]$sciname ="PAGURUS POLITUS"
# combined[combined$COMM == "SEA CUCUMBER",]$sciname ="HOLOTHUROIDEA C."
# combined[combined$COMM == "SIPHONOPHORA O.",]$sciname ="SIPHONOPHORA O."
# combined[combined$COMM == "LAMPADENA SP.",]$sciname ="LAMPADENA SP."
# combined[combined$COMM == "ANEMONE SEA SPIDER",]$sciname ="NA"
# combined[combined$COMM == "SKATE,ROSETTE",]$sciname ="LEUCORAJA GARMANI"
# combined[combined$COMM == "CALAPPA MEGALOPS",]$sciname ="CALAPPA MEGALOPS"
# combined[combined$COMM == "VINCIGUERRIA NIMBARIA",]$sciname ="VINCIGUERRIA NIMBARIA"
# combined[combined$COMM == "BASSOGIGAS GILLI",]$sciname ="BASSOGIGAS GILLI"
# combined[combined$COMM == "DICHELOPANDALUS LEPTOCERUS",]$sciname ="DICHELOPANDALUS LEPTOCERUS"
# combined[combined$COMM == "LUGWORM",]$sciname ="ARENICOLA MARINA"
# combined[combined$COMM == "PINK JELLYFISH",]$sciname ="AURELIA SP."
# combined[combined$COMM == "GADIFORMES",]$sciname ="GADIFORMES O."
# combined[combined$COMM == "MENHADEN(ATLANTIC)",]$sciname ="BREVOORTIA TYRANNUS"
# combined[combined$COMM == "HISTIOTEUTHIS SP.",]$sciname ="HISTIOTEUTHIS SP."
# combined[combined$COMM == "GHOST CATSHARK",]$sciname ="APRISTURUS MANIS"
# combined[combined$COMM == "HALOSAURUS GUENTHERI",]$sciname ="HALOSAURUS GUENTHERI"
# combined[combined$COMM == "STEREOMASTIS SCULPTA",]$sciname ="STEREOMASTIS SCULPTA"
# combined[combined$COMM == "UNID FISH AND INVERTEBRATES",]$sciname ="NA"
# combined[combined$COMM == "TREMASTER MIRABILIS",]$sciname ="TREMASTER MIRABILIS"
# combined[combined$COMM == "SPINYTAIL SKATE",]$sciname ="BATHYRAJA SPINICAUDA"
# combined[combined$COMM == "DEEPSEA ANGLER",]$sciname ="CERATIAS HOLBOELLI"
# combined[combined$COMM == "DEEPWATER FLOUNDER",]$sciname ="MONOLENE SESSILICAUDA"
# combined[combined$COMM == "TAANINGICHTHYS MINIMUS",]$sciname ="TAANINGICHTHYS MINIMUS"
# combined[combined$COMM == "ILYOPHIS BRUNNEUS",]$sciname ="ILYOPHIS BRUNNEUS"
# combined[combined$COMM == "LAMPANYCTUS PHOTONOTUS",]$sciname ="LAMPANYCTUS PHOTONOTUS"
# combined[combined$COMM == "LONGNOSE LANCETFISH",]$sciname ="ALEPISAURUS FEROX"
# combined[combined$COMM == "NOTOSCOPELUS CAUDISPINOSUS",]$sciname ="NOTOSCOPELUS CAUDISPINOSUS"
# combined[combined$COMM == "OWENIIDAE F.",]$sciname ="OWENIIDAE F."
# combined[combined$COMM == "MOLLUSCA SHELLS EMPTY",]$sciname ="NA"
# combined[combined$COMM == "BENTHOSEMA SP.",]$sciname ="BENTHOSEMA SP."
# combined[combined$COMM == "OMOSUDIS LOWEI",]$sciname ="OMOSUDIS LOWEI"
# combined[combined$COMM == "BATHYPTEROIS PHENAX",]$sciname ="BATHYPTEROIS PHENAX"
# combined[combined$COMM == "EVERMANELLA INDICA",]$sciname ="EVERMANELLA INDICA"
# combined[combined$COMM == "DENDRODOA SP.",]$sciname ="DENDRODOA SP."
# combined[combined$COMM == "BLOOD WORM",]$sciname ="GLYCERA SP."
# combined[combined$COMM == "SOLEMYA BOREALIS",]$sciname ="SOLEMYA BOREALIS"
# combined[combined$COMM == "SHARK, SAND",]$sciname ="NA"
# combined[combined$COMM == "TEREBRATULINA SP.",]$sciname ="TEREBRATULINA SP."
# combined[combined$COMM == "GALATHEIDAE F.",]$sciname ="GALATHEIDAE F."
# combined[combined$COMM == "OITHONA SPINIROSTRIS",]$sciname ="OITHONA SPINIROSTRIS"
# combined[combined$COMM == "MACROSTOMIAS LONGIBARBATUS",]$sciname ="MACROSTOMIAS LONGIBARBATUS"
# combined[combined$COMM == "SEA LILIES",]$sciname ="CRINOIDAE C."
# combined[combined$COMM == "SEA RAVEN, EGGS",]$sciname ="HEMITRIPTERUS AMERICANUS EGGS"
# combined[combined$COMM == "GREATER SHEARWATER",]$sciname ="ARDENNA GRAVIS"
# combined[combined$COMM == "DOG WHELK",]$sciname ="NUCELLA LAPILLUS"
# combined[combined$COMM == "CHIRIDOTA LAEVIS",]$sciname ="CHIRIDOTA LAEVIS"
# combined[combined$COMM == "ARGYROPELECUS SP.",]$sciname ="ARGYROPELECUS SP."
# combined[combined$COMM == "ARGYROPELECUS GIGAS",]$sciname ="ARGYROPELECUS GIGAS"
# combined[combined$COMM == "CORYPHAENOIDES SP",]$sciname ="CORYPHAENOIDES SP."
# combined[combined$COMM == "DIPLOPHOS MADERENSIS",]$sciname ="DIPLOPHOS MADERENSIS"
# combined[combined$COMM == "DEEP SEA FINNED OCTOPUS",]$sciname ="GRIMOTEUTHIS SP."
# combined[combined$COMM == "EUSIRUS CUSPIDATUS",]$sciname ="EUSIRUS CUSPIDATUS"
# combined[combined$COMM == "ROULEINA SP.",]$sciname ="ROULEINA SP."
# combined[combined$COMM == "BOARFISH",]$sciname ="CAPROS APER"
# combined[combined$COMM == "YELLOWFIN BASS",]$sciname ="ANTHIAS NICHOLSI"
# combined[combined$COMM == "SCLEROCRANGON SP.",]$sciname ="SCLEROCRANGON SP."
# combined[combined$COMM == "TURBELLARIA C.",]$sciname ="TURBELLARIA C."
# combined[combined$COMM == "KNOTTED WRACK",]$sciname ="ASCOPHYLLUM NODOSUM"
# combined[combined$COMM == "HIPPOLYTID EGGS",]$sciname ="HIPPOLYTIDAE F. EGGS"
# combined[combined$COMM == "EPIGONUS PANDIONIS",]$sciname ="EPIGONUS PANDIONIS"
# combined[combined$COMM == "ANOMIA SIMPLEX",]$sciname ="ANOMIA SIMPLEX"
# combined[combined$COMM == "GAPPER",]$sciname ="NA"
# combined[combined$COMM == "MARGARITES GROENLANDICA",]$sciname ="MARGARITES GROENLANDICA"
# combined[combined$COMM == "THYSANOESSA RASCHII",]$sciname ="THYSANOESSA RASCHII"
# combined[combined$COMM == "SEA WHIP KELP",]$sciname ="LAMINARIALES O."
# combined[combined$COMM == "PORTUNID CRABS",]$sciname ="PORTUNIDAE F."
# combined[combined$COMM == "ALFONSIN A CASTA LARGA",]$sciname ="BERYX DECADACTYLUS"
# combined[combined$COMM == "GONATUS FABRICII",]$sciname ="GONATUS FABRICII"
# combined[combined$COMM == "NOTOSCOPELUS SP.",]$sciname ="NOTOSCOPELUS SP."
# combined[combined$COMM == "FIG SPONGE",]$sciname ="	PORIFERA P."
# combined[combined$COMM == "AXIIDAE F.",]$sciname ="AXIIDAE F."
# combined[combined$COMM == "ONEIRODES SP.",]$sciname ="ONEIRODES SP."
# combined[combined$COMM == "TANAIDAE F.",]$sciname ="TANAIDAE F."
# combined[combined$COMM == "MELANOSTOMIAS SP.",]$sciname ="MELANOSTOMIAS SP."
# combined[combined$COMM == "PTERYGIOTEUTHIS SP.",]$sciname ="PTERYGIOTEUTHIS SP."
# combined[combined$COMM == "SMOOTHHEAD",]$sciname ="LEPTOCHILICHTHYS AGASSIZII"
# combined[combined$COMM == "BATHYPTEROIS GRALLATOR",]$sciname ="BATHYPTEROIS GRALLATOR"
# combined[combined$COMM == "ACANTHEPHYRA SP.",]$sciname ="ACANTHEPHYRA SP."
# combined[combined$COMM == "MORAS",]$sciname ="MORIDAE F."
# combined[combined$COMM == "BIGEYE",]$sciname ="PRIACANTHIDAE F."
# combined[combined$COMM == "SIPUNCULUS SP.",]$sciname ="SIPUNCULUS SP."
# combined[combined$COMM == "BOTHUS SP.",]$sciname ="BOTHUS SP."
# combined[combined$COMM == "PARASITES,ROUND WORMS",]$sciname ="NA"
# combined[combined$COMM == "STOMPHIA COCCINEA",]$sciname ="STOMPHIA COCCINEA"
# combined[combined$COMM == "GARLAND HYDROIDS",]$sciname ="GARLAND HYDROIDS"
# combined[combined$COMM == "CONGER SP.",]$sciname ="CONGER SP."
# combined[combined$COMM == "EELPOUT (NS)",]$sciname ="LYCODES TERRAENOVAE"
# combined[combined$COMM == "SAND DOLLARS,URCHINS (NS)",]$sciname ="SAND DOLLARS, URCHINS"
# combined[combined$COMM == "BLIND LOBSTER",]$sciname ="ACANTHACARIS CAECA"
# combined[combined$COMM == "ASTROTECTEN DUPLICATUS",]$sciname ="ASTROTECTEN DUPLICATUS"
# combined[combined$COMM == "EPIGONUS SP.",]$sciname ="EPIGONUS SP."
# combined[combined$COMM == "BATFISH,SPINY",]$sciname ="HALIEUTICHTHYS BISPINOSUS"
# combined[combined$COMM == "GORGONOCEPHALUS LAMARCKI",]$sciname ="GORGONOCEPHALUS LAMARCKI"
# combined[combined$COMM == "ARGIS SP.",]$sciname ="ARGIS SP."
# combined[combined$COMM == "SCULPIN EGGS UNID.",]$sciname ="NA"
# combined[combined$COMM == "LAMPANYCTUS PUSILLUS",]$sciname ="LAMPANYCTUS PUSILLUS"
# combined[combined$COMM == "LAMPRIFORMES",]$sciname ="LAMPRIFORMES O."
# combined[combined$COMM == "LAEMONEMA SP.",]$sciname ="LAEMONEMA SP."
# combined[combined$COMM == "CUBICEPS GRACILIS",]$sciname ="CUBICEPS GRACILIS"
# combined[combined$COMM == "PILOTFISH",]$sciname ="NAUCRATES DUCTOR"
# combined[combined$COMM == "LEBBEUS ZEBRA",]$sciname ="LEBBEUS ZEBRA"
# combined[combined$COMM == "GREATER AMBERJACK",]$sciname ="SERIOLA DUMERILI"
# combined[combined$COMM == "SMOOTH DOGFISH",]$sciname ="MUSTELUS CANIS"
# combined[combined$COMM == "VENEFICA PROCERA",]$sciname ="VENEFICA PROCERA"
# combined[combined$COMM == "BERYCOID FISH",]$sciname ="TRACHICHTHYIDAE F."
# combined[combined$COMM == "SMOOTH FLOUNDER",]$sciname ="PLEURONECTES PUTNAMI"
# combined[combined$COMM == "BIGEYE SCAD",]$sciname ="SECLAR CRUMENOPHTHALMUS"
# combined[combined$COMM == "HISTIOTEUTHIDAE F.",]$sciname ="HISTIOTEUTHIDAE F."
# combined[combined$COMM == "BLADDER WRACK",]$sciname ="FUCUS VESICULOSUS"
# combined[combined$COMM == "OCEAN SUNFISH",]$sciname ="MOLA MOLA"
# combined[combined$COMM == "SPIONIDA F.",]$sciname ="SPIONIDA F."
# combined[combined$COMM == "CARDINALFISH",]$sciname ="APOGONIDAE F."
# combined[combined$COMM == "DOG WHELKS",]$sciname ="NUCELLA LAPILLUS"
# combined[combined$COMM == "Clavularia spp",]$sciname ="CLAVULARIA SP."
# combined[combined$COMM == "BATHYPTEROIS SP.",]$sciname ="BATHYPTEROIS SP.."
# combined[combined$COMM == "PYROTEUTHIS SP.",]$sciname ="PYROTEUTHIS SP."
# combined[combined$COMM == "MELAMPHAES MICROPS",]$sciname ="MELAMPHAES MICROPS"
# combined[combined$COMM == "ANONYX SP.",]$sciname ="ANONYX SP."
# combined[combined$COMM == "IDOTEA SP.",]$sciname ="IDOTEA SP."
# combined[combined$COMM == "EUALUS SP.",]$sciname ="EUALUS SP."
# combined[combined$COMM == "OPHELIA SP.",]$sciname ="OPHELIA SP."
# combined[combined$COMM == "MELAMPHAES SUBORBITALIS",]$sciname ="MELAMPHAES SUBORBITALIS"
# combined[combined$COMM == "ILYOPHIS SP",]$sciname ="ILYOPHIS SP."
# combined[combined$COMM == "SILVER-RAG",]$sciname ="ARIOMMA BONDI"
# combined[combined$COMM == "CAUDINA ARENATA",]$sciname ="CAUDINA ARENATA."
# combined[combined$COMM == "MARINE INVERTEBRATES (NS)",]$sciname ="INVERTEBRATES"
# combined[combined$COMM == "SHARK (NS)",]$sciname ="CHONDRCHTHYES C."
# combined[combined$COMM == "BLACK OREO",]$sciname ="ALLOCYTTUS NIGER"
# combined[combined$COMM == "SWALLOWERS",]$sciname ="CHIASMODON NIGER"
# combined[combined$COMM == "LEPTAGONUS.DECAGONUS",]$sciname ="LEPTAGONUS DECAGONUS"
# combined[combined$COMM == "ALEPISAURUS.FEROX",]$sciname ="ALEPISAURUS FEROX"
# combined[combined$COMM == "ALEPOCEPHALIDAE",]$sciname ="ALEPOCEPHALIDAE F."
# combined[combined$COMM == "ALOSA.PSEUDOHARENGUS",]$sciname ="ALOSA PSEUDOHARENGUS"
# combined[combined$COMM == "AMMODYTES.DUBIUS",]$sciname ="AMMODYTES DUBIUS"
# combined[combined$COMM == "ANARHICHAS.DENTICULATUS",]$sciname ="ANARHICHAS DENTICULATUS"
# combined[combined$COMM == "ANARHICHAS.LUPUS",]$sciname ="ANARHICHAS LUPUS"
# combined[combined$COMM == "ANARHICHAS.MINOR",]$sciname ="ANARHICHAS MINOR"
# combined[combined$COMM == "ANOPLOGASTER.CORNUTA",]$sciname ="ANOPLOGASTER CORNUTA"
# combined[combined$COMM == "ANOTOPTERUS.PHARAO",]$sciname ="ANOTOPTERUS.PHARAO"
# combined[combined$COMM == "ANTIMORA.ROSTRATA",]$sciname ="ANTIMORA ROSTRATA"
# combined[combined$COMM == "APRISTURUS.PROFUNDORUM",]$sciname ="APRISTURUS PROFUNDORUM"
# combined[combined$COMM == "ARGENTINA.SILUS",]$sciname ="ARGENTINA SILUS"
# combined[combined$COMM == "ARTEDIELLUS..SP_",]$sciname ="ARTEDIELLUS SP."
# combined[combined$COMM == "ASPIDOPHOROIDES.MONOPTERYGIUS",]$sciname ="ASPIDOPHOROIDES MONOPTERYGIUS"
# combined[combined$COMM == "ASPIDOPHOROIDES.OLRIKII",]$sciname ="ASPIDOPHOROIDES OLRIKII"
# combined[combined$COMM == "BATHYLAGUS.EURYOPS",]$sciname ="BATHYLAGUS EURYOPS"
# combined[combined$COMM == "BATHYPTEROIS.DUBIUS",]$sciname ="BATHYPTEROIS DUBIUS"
# combined[combined$COMM == "BATHYTROCTES..SP_",]$sciname ="BATHYTROCTES SP."
# combined[combined$COMM == "BOREOGADUS.SAIDA",]$sciname ="BOREOGADUS SAIDA"
# combined[combined$COMM == "BROSME.BROSME",]$sciname ="BROSME BROSME"
# combined[combined$COMM == "CAREPROCTUS..SP_",]$sciname ="CAREPROCTUS SP."
# combined[combined$COMM == "CARISTIUS.FASCIATUS",]$sciname ="CARISTIUS FASCIATUS"
# combined[combined$COMM == "CENTROSCYLLIUM.FABRICII",]$sciname ="CENTROSCYLLIUM FABRICII"
# combined[combined$COMM == "CERATIAS.HOLBOELLI",]$sciname ="CERATIAS HOLBOELLI"
# combined[combined$COMM == "CHAULIODUS.SLOANI",]$sciname ="CHAULIODUS SLOANI"
# combined[combined$COMM == "CHIASMODON.NIGER",]$sciname ="CHIASMODON NIGER"
# combined[combined$COMM == "CHIONOECETES.OPILIO",]$sciname ="CHIONOECETES OPILIO"
# combined[combined$COMM == "CLUPEA.HARENGUS",]$sciname ="CLUPEA HARENGUS"
# combined[combined$COMM == "CORYPHAENOIDES.RUPESTRIS",]$sciname ="CORYPHAENOIDES RUPESTRIS"
# combined[combined$COMM == "COTTIDAE",]$sciname ="COTTIDAE F."
# combined[combined$COMM == "COTTUNCULUS.MICROPS",]$sciname ="COTTUNCULUS MICROPS"
# combined[combined$COMM == "CRYPTACANTHODES.MACULATUS",]$sciname ="CRYPTACANTHODES MACULATUS"
# combined[combined$COMM == "CRYPTOPSARAS.COUESII",]$sciname ="CRYPTOPSARAS COUESII"
# combined[combined$COMM == "CYCLOPTERUS.LUMPUS",]$sciname ="CYCLOPTERUS LUMPUS"
# combined[combined$COMM == "CYCLOTHONE..SP_",]$sciname ="CYCLOTHONE SP."
# combined[combined$COMM == "DIBRANCHUS.ATLANTICUS",]$sciname ="DIBRANCHUS ATLANTICUS"
# combined[combined$COMM == "DIRETMUS.ARGENTEUS",]$sciname ="DIRETMUS ARGENTEUS"
# combined[combined$COMM == "ENCHELYOPUS.CIMBRIUS",]$sciname ="ENCHELYOPUS CIMBRIUS"
# combined[combined$COMM == "EUMESOGRAMMUS.PRAECISUS",]$sciname ="EUMESOGRAMMUS PRAECISUS"
# combined[combined$COMM == "EUMICROTREMUS..SP_",]$sciname ="EUMICROTREMUS SP."
# combined[combined$COMM == "EURYPHARYNX.PELECANOIDES",]$sciname ="EUYPHARYNX PELECANOIDES"
# combined[combined$COMM == "GADUS.MORHUA",]$sciname ="GADUS MORHUA"
# combined[combined$COMM == "GADUS.OGAC",]$sciname ="GADUS OGAC"
# combined[combined$COMM == "GAIDROPSARUS..SP_",]$sciname ="GAIDROPSARUS SP."
# combined[combined$COMM == "GASTEROSTEIFORMES..ORDER.",]$sciname ="GASTEROSTEIFORMES O."
# combined[combined$COMM == "GASTEROSTEUS.ACULEATUS.ACULEATUS",]$sciname ="GASTEROSTEUS ACULEATUS ACULEATUS"
# combined[combined$COMM == "GLYPTOCEPHALUS.CYNOGLOSSUS",]$sciname ="GLYPTOCEPHALUS CYNOGLOSSUS"
# combined[combined$COMM == "GONOSTOMA..SP_",]$sciname ="GONOSTOMA SP."
# combined[combined$COMM == "GONOSTOMATIDAE",]$sciname ="GONOSTOMATIDAE F."
# combined[combined$COMM == "GYMNELUS.VIRIDIS",]$sciname ="GYMNELUS VIRIDIS"
# combined[combined$COMM == "GYMNOCANTHUS.TRICUSPIS",]$sciname ="GYMNOCANTHUS TRICUSPIS"
# combined[combined$COMM == "HALARGYREUS.JOHNSONII.1",]$sciname ="HALARGYREUS JOHNSONII"
# combined[combined$COMM == "HEMITRIPTERUS.AMERICANUS",]$sciname ="HEMITRIPTERUS AMERICANUS"
# combined[combined$COMM == "HIPPOGLOSSOIDES.PLATESSOIDES",]$sciname ="HIPPOGLOSSOIDES PLATESSOIDES"
# combined[combined$COMM == "HIPPOGLOSSUS.HIPPOGLOSSUS",]$sciname ="HIPPOGLOSSUS HIPPOGLOSSUS"
# combined[combined$COMM == "HOPLOSTETHUS..SP_",]$sciname ="HOPLOSTETHUS SP."
# combined[combined$COMM == "HYDROLAGUS.AFFINIS",]$sciname ="HYDROLAGUS AFFINIS"
# combined[combined$COMM == "ICELUS..SP_",]$sciname ="ICELUS SP."
# combined[combined$COMM == "ICELUS.SPATULA",]$sciname ="ICELUS SPATULA"
# combined[combined$COMM == "LEPIDION.EQUES",]$sciname ="LEPIDION EQUES"
# combined[combined$COMM == "LIMANDA.FERRUGINEA",]$sciname ="LIMANDA FERRUGINEA"
# combined[combined$COMM == "LIPARIDAE",]$sciname ="LIPARIDAE F."
# combined[combined$COMM == "LIPOGENYS.GILLII",]$sciname ="LIPOGENYS GILLII"
# combined[combined$COMM == "LOPHIUS.AMERICANUS",]$sciname ="LOPHIUS AMERICANUS"
# combined[combined$COMM == "LUMPENUS.LAMPRETAEFORMIS",]$sciname ="LUMPENUS LAMPRETAEFORMIS"
# combined[combined$COMM == "LEPTOCLINUS.MACULATUS",]$sciname ="LEPTOCLINUS MACULATUS"
# combined[combined$COMM == "LYCENCHELYS..SP_",]$sciname ="LYCENCHELYS SP."
# combined[combined$COMM == "LYCODES..SP_",]$sciname ="LYCODES SP."
# combined[combined$COMM == "LYCODES.ESMARKII",]$sciname ="LYCODES ESMARKII"
# combined[combined$COMM == "LYCODES.RETICULATUS",]$sciname ="LYCODES RETICULATUS"
# combined[combined$COMM == "LYCODES.VAHLII",]$sciname ="LYCODES VAHLII"
# combined[combined$COMM == "POLYACANTHONOTUS.RISSOANUS",]$sciname ="POLYACANTHONOTUS RISSOANUS"
# combined[combined$COMM == "MACROURIDAE",]$sciname ="MACROURIDAE F."
# combined[combined$COMM == "MACROURUS.BERGLAX",]$sciname ="MACROURUS BERGLAX"
# combined[combined$COMM == "ZOARCES.AMERICANUS",]$sciname ="ZOARCES AMERICANUS"
# combined[combined$COMM == "MALACOSTEUS.NIGER",]$sciname ="MALACOSTEUS NIGER"
# combined[combined$COMM == "MALLOTUS.VILLOSUS",]$sciname ="MALLOTUS VILLOSUS"
# combined[combined$COMM == "MELAMPHAIDAE",]$sciname ="MELAMPHAIDAE F."
# combined[combined$COMM == "MELANOGRAMMUS.AEGLEFINUS",]$sciname ="MELANOGRAMMUS AEGLEFINUS"
# combined[combined$COMM == "MELANOSTIGMA.ATLANTICUM",]$sciname ="MELANOSTIGMA ATLANTICUM"
# combined[combined$COMM == "MERLUCCIUS.ALBIDUS",]$sciname ="MERLUCCIUS ALBIDUS"
# combined[combined$COMM == "MERLUCCIUS.BILINEARIS",]$sciname ="MERLUCCIUS BILINEARIS"
# combined[combined$COMM == "MICROMESISTIUS.POUTASSOU",]$sciname ="MICROMESISTIUS POUTASSOU"
# combined[combined$COMM == "MYCTOPHIDAE",]$sciname ="MYCTOPHIDAE F."
# combined[combined$COMM == "MYOXOCEPHALUS.OCTODECEMSPINOSUS",]$sciname ="MYOXOCEPHALUS OCTODECEMSPINOSUS"
# combined[combined$COMM == "MYOXOCEPHALUS.SCORPIOIDES",]$sciname ="MYOXOCEPHALUS SCORPIOIDES"
# combined[combined$COMM == "MYOXOCEPHALUS.SCORPIUS",]$sciname ="MYOXOCEPHALUS SCORPIUS"
# combined[combined$COMM == "MYXINE.GLUTINOSA",]$sciname ="MYXINE GLUTINOSA"
# combined[combined$COMM == "NEMICHTHYS.SCOLOPACEUS",]$sciname ="NEMICHTHYS SCOLOPACEUS"
# combined[combined$COMM == "NESSORHAMPHUS.INGOLFIANUS",]$sciname ="NESSORHAMPHUS INGOLFIANUS"
# combined[combined$COMM == "NEZUMIA.BAIRDII",]$sciname ="NEXUMIA BAIRDII"
# combined[combined$COMM == "NOTACANTHUS.CHEMNITZII",]$sciname ="NOTACANTHUS CHEMNITZII"
# combined[combined$COMM == "PANDALUS.BOREALIS",]$sciname ="PANDALUS BOREALIS"
# combined[combined$COMM == "PANDALUS.MONTAGUI",]$sciname ="PANDALUS MONTAGUI"
# combined[combined$COMM == "PANDALUS.PROPINQVUS",]$sciname ="PANDALUS PROPINQVUS"
# combined[combined$COMM == "PARALEPIDIDAE",]$sciname ="PARALEPIDIDAE F."
# combined[combined$COMM == "PARASUDIS.TRUCULENTA",]$sciname ="PARASUDIS TRUNCULENTA"
# combined[combined$COMM == "PETROMYZON.MARINUS",]$sciname ="PETROMYZON MARINUS"
# combined[combined$COMM == "PHOLIDAE",]$sciname ="PHOLIDAE F."
# combined[combined$COMM == "POLLACHIUS.VIRENS",]$sciname ="POLLACHIUS VIRENS"
# combined[combined$COMM == "PSEUDOPLEURONECTES.AMERICANUS",]$sciname ="PSEUDOPLEURONECTES AMERICANUS"
# combined[combined$COMM == "RAJA..SP_",]$sciname ="RAJA SP."
# combined[combined$COMM == "BATHYRAJA.SPINICAUDA",]$sciname ="BATHYRAJA SPINICAUDA"
# combined[combined$COMM == "RAJELLA.FYLLAE",]$sciname ="RAJELLA FYLLAE"
# combined[combined$COMM == "AMBLYRAJA.JENSENI",]$sciname ="AMBLYRAJA JENSENI"
# combined[combined$COMM == "DIPTURUS.LINTEUS",]$sciname ="DIPTURUS LINTEUS"
# combined[combined$COMM == "AMBLYRAJA.RADIATA",]$sciname ="AMBLYRAJA RADIATA"
# combined[combined$COMM == "MALACORAJA.SENTA",]$sciname ="MALACORAJA SENTA"
# combined[combined$COMM == "REINHARDTIUS.HIPPOGLOSSOIDES",]$sciname ="REINHARDTIUS HIPPOGLOSSOIDES"
# combined[combined$COMM == "SCOMBERESOX.SAURUS.SAURUS",]$sciname ="SCOMBERESOX SAURUS SAURUS"
# combined[combined$COMM == "NOTOSUDIDAE",]$sciname ="NOTOSUDIDAE F."
# combined[combined$COMM == "SCORPAENIDAE",]$sciname ="SCORPAENIDAE F."
# combined[combined$COMM == "SEBASTES.NORVEGICUS",]$sciname ="SEBASTES NORVEGICUS"
# combined[combined$COMM == "SEBASTES.MENTELLA",]$sciname ="SEBASTES MENTELLA"
# combined[combined$COMM == "SERRIVOMER.BEANII",]$sciname ="SERRIVOMER BEANII"
# combined[combined$COMM == "SIMENCHELYS.PARASITICUS",]$sciname ="SIMENCHELYS PARASITICUS"
# combined[combined$COMM == "SQUALUS.ACANTHIAS",]$sciname ="SQUALUS ACANTHIAS"
# combined[combined$COMM == "STERNOPTYCHIDAE",]$sciname ="STERNOPTYCHIDAE F."
# combined[combined$COMM == "STOMIAS.BOA.FEROX",]$sciname ="STOMIAS BOA FEROX"
# combined[combined$COMM == "STOMIIDAE",]$sciname ="STOMIIDAE F."
# combined[combined$COMM == "SYNAPHOBRANCHUS.KAUPII",]$sciname ="SYNAPHOBRANCHUS KAUPII"
# combined[combined$COMM == "TRACHYRINCUS.MURRAYI",]$sciname ="TRACHYRINCUS MURRAYI"
# combined[combined$COMM == "TRIGLOPS..SP_",]$sciname ="TRIGLOPS SP."
# combined[combined$COMM == "UNIDENTIFIED.FISH",]$sciname ="NA"
# combined[combined$COMM == "PHYCIS.CHESTERI",]$sciname ="PHYCIS CHESTERI"
# combined[combined$COMM == "UROPHYCIS.TENUIS",]$sciname ="UROPHYCIS TENUIS"
# combined[combined$COMM == "XENODERMICHITHYS..ALEPOSOMUS..COPEI",]$sciname ="XENODERMICHITHYS ALEPOSOMUS COPEI"
# 
# 
# 
# source("C:/Users/StevensLy/Documents/Database/Code/MPA database/ClassifyFunction.R")
# 
# 
# MNData <- read.csv("C:/Users/StevensLy/Documents/Database/Data/FINALMN_Lydia.csv",stringsAsFactors = F)
# 
# #Create new colum to put species into
# MNData$newsciname2 = MNData$newsciname
# 
# #Function to fix extra space at the end of some species names. 
# SpaceFix <- function(x){
#   if(!is.na(x)){
#     if(substr(x,start = nchar(x),stop = nchar(x))==" "){x=substr(x,start = 1,stop=nchar(x)-1)}
#     if(substr(x,start = 1,stop = 1)==" "){x=substr(x,start = 2,stop=nchar(x))}
#   }
#   return(x)}
# 
# #remove all taxonomic groups at the end of scientific names
# MNData$newsciname2 <- gsub(" P\\.","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" F\\.","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" O\\.","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" C\\.","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" SP\\.","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" S\\.P","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" S\\.C","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" S\\.P","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" S\\.O","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" SF","",MNData$newsciname2)
# MNData$newsciname2 <- gsub(" SP","",MNData$newsciname2)
# MNData$newsciname2 <- gsub("\\.","",MNData$newsciname2)
# 
# 
# 
# #Change names to be consistent  
# MNData[which(MNData$newsciname2=="ALEPISAURUS BREVIROSTIS"),"newsciname2"] <- "ALEPISAURUS BREVIROSTRIS"
# MNData[which(MNData$newsciname2=="ANGUILLIFORMES APODES ORDER"),"newsciname2"] <- "ANGUILLIFORMES"
# MNData[which(MNData$newsciname2=="BARNACLES"),"newsciname2"] <- "MAXILLOPODA"
# MNData[which(MNData$newsciname2=="BARRACUDINA"),"newsciname2"] <- "	PARALEPIDIDAE"
# MNData[which(MNData$newsciname2=="BLENIIDAE,LIPOPHRYS,PHOLIS SP."),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="BRISTLE WORMS"),"newsciname2"] <- "ANNELIDA"
# MNData[which(MNData$newsciname2=="BRITTLE STAR"),"newsciname2"] <- "OPHIUROIDEA"
# MNData[which(MNData$newsciname2=="CHIMAERIFORMES HOLOCEPHALI ORDER"),"newsciname2"] <- "CHIMAERIFORMES"
# MNData[which(MNData$newsciname2=="cHLOROPHTHALMUS AGASSIZI"),"newsciname2"] <- "CHLOROPHTHALMUS AGASSIZI"
# MNData[which(MNData$newsciname2=="CYCLOPTERUS SP. AND GASTROPODA C."),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="DAISY"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="EEL (UNIDENTIFIED)"),"newsciname2"] <- "ANGUILLIFORMES"
# MNData[which(MNData$newsciname2=="EGGS (UNIDENTIFIED)"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="FOREIGN ARTICLES,GARBAGE"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="FOURTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="GASTEROSTEIFORMES ORDER"),"newsciname2"] <- "GASTEROSTEIFORMES"
# MNData[which(MNData$newsciname2=="HYAS ARANEUS AND/OR HYAS COARCTATUS"),"newsciname2"] <- "HYAS (UNIDENTIFIED)"
# MNData[which(MNData$newsciname2=="LIBINIA EMARGINATA,"),"newsciname2"] <- "LIBINIA EMARGINATA"
# MNData[which(MNData$newsciname2=="NATANTIA"),"newsciname2"] <- "DECAPODA"
# MNData[which(MNData$newsciname2=="newsciname"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="NEZUMIA BAIRDI"),"newsciname2"] <- "NEZUMIA BAIRDII"
# MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="PARASUDIS TRUNCULENTUS"),"newsciname2"] <- "PARASUDIS TRUNCULENTA"
# MNData[which(MNData$newsciname2=="PLACOPECTEN MAGELLANICUS SHELLS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="PLUERONECTIFORMES HETEROSOMATA ORDER"),"newsciname2"] <- "PLEURONECTIFORMES"
# MNData[which(MNData$newsciname2=="PSEUDOPLUERONECTES AMERICANUS"),"newsciname2"] <- "PSEUDOPLEURONECTES AMERICANUS"
# MNData[which(MNData$newsciname2=="SAND DOLLARS, URCHINS"),"newsciname2"] <- "ECHINOIDEA"
# MNData[which(MNData$newsciname2=="SEA SLUGS"),"newsciname2"] <- "GASTROPODA"
# MNData[which(MNData$newsciname2=="SEAWEED,ALGAE,KELP"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SECOND UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="FIRST UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="THIRD UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="FIFTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SIXTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SEVENTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="EIGHTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="NINTH UNIDENTIFIED PER SET"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SERRIVOMER BEANI"),"newsciname2"] <- "SERRIVOMER BEANII"
# MNData[which(MNData$newsciname2=="SKATE EGGS (UNIDENTIFIED)"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SKATES AND RAYS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SNAIL AND SLUG EGGS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SNAILS AND SLUGS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SOFT CORAL (UNIDENTIFIED)"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="STICKLEBACK (UNIDENTIFIED)"),"newsciname2"] <- "GASTEROSTEIDAE"
# MNData[which(MNData$newsciname2=="STONES AND ROCKS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="UNIDENTIFIED FISH"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="UNIDENTIFIED SP"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="BARRACUDINA (UNIDENTIFIED)"),"newsciname2"] <- "PARALEPIDIDAE"
# MNData[which(MNData$newsciname2=="CHIONOECETES (UNIDENTIFIED)"),"newsciname2"] <- "CHIONOECETES"
# MNData[which(MNData$newsciname2=="FLOUNDER (UNIDENTIFIED)"),"newsciname2"] <- "PARALICHTHYIDAE"
# MNData[which(MNData$newsciname2=="HYAS (UNIDENTIFIED)"),"newsciname2"] <- "HYAS"
# MNData[which(MNData$newsciname2=="NATICIDAE (UNIDENTIFIED)"),"newsciname2"] <- "NATICIDAE"
# MNData[which(MNData$newsciname2=="SEASNAIL (UNIDENTIFIED)"),"newsciname2"] <- "GASTROPODA"
# MNData[which(MNData$newsciname2=="SYNGNATHINAE (UNIDENTIFIED)"),"newsciname2"] <- "SYNGNATHIDAE"
# MNData[which(MNData$newsciname2=="STOMIATOID (UNIDENTIFIED)"),"newsciname2"] <- "STOMIATOID"
# MNData[which(MNData$newsciname2=="STOMIAS (UNIDENTIFIED)"),"newsciname2"] <- "STOMIAS"
# MNData[which(MNData$newsciname2=="EGGS (UNIDENTIDIED)"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="UNIDENTIFIED"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SHARK,SAND"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="GAPPER"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="PARASITES,ROUND WORMS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="SCULPIN EGGS UNID."),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="UNIDENTIFIED.FISH"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="TRIGLOPS OMMATISTIUS"),"newsciname2"] <- "TRIGLOPS MURRAYI"
# MNData[which(MNData$newsciname2=="FOETOREPUS AGASSUSZII"),"newsciname2"] <- "FOETOREPUS AGASSIZII"
# MNData[which(MNData$newsciname2=="HERMIT CRABS"),"newsciname2"] <- "DECAPODA"
# MNData[which(MNData$newsciname2=="COTTIDEA"),"newsciname2"] <- "COTTIDAE"
# MNData[which(MNData$newsciname2=="VAZELLA POURTALESI"),"newsciname2"] <- "VAZELLA POURTALESII"
# MNData[which(MNData$newsciname2=="	CTENOPHORA"),"newsciname2"] <- "CTENOPHORA"
# MNData[which(MNData$newsciname2=="	PORANIA PULVILIS"),"newsciname2"] <- "PORANIA PULVILLUS"
# MNData[which(MNData$newsciname2=="	CEREMASTER GRANULARIS"),"newsciname2"] <- "CERAMASTER GRANULARIS"
# MNData[which(MNData$newsciname2=="EUMICROTREMUSINOSUS"),"newsciname2"] <- "EUMICROTREMUS"
# MNData[which(MNData$newsciname2=="PAGUROIDEA SF"),"newsciname2"] <- "PAGUROIDEA"
# MNData[which(MNData$newsciname2=="SPIRONTOCARISINUS"),"newsciname2"] <- "SPIRONTOCARIS"
# MNData[which(MNData$COMM=="SPINY CRAB"),"newsciname2"] <- "LITHODES"
# MNData[which(MNData$COMM=="NORTHERN STONE CRAB"), "newsciname2"] <- "LITHODES MAJA"
# MNData[which(MNData$newsciname2=="PORANIOMORPHA BOREALIS"), "newsciname2"] <- "PORANIOMORPHA"
# MNData[which(MNData$newsciname2=="WHELK EGGS"), "newsciname2"] <- NA
# MNData[which(MNData$COMM=="FAWN CUSK EEL"), "newsciname2"] <- "LEPOPHIDIUM PROFUNDORUM"
# MNData[which(MNData$newsciname2=="CENTROPHRYNEINULOSA"), "newsciname2"] <- "CENTROPHRYNE SPINULOSA"
# MNData[which(MNData$newsciname2=="SCARLETT PSOLUS"), "newsciname2"] <- "PSOLUS FABRICII"
# MNData[which(MNData$newsciname2=="PATELLIDAR"), "newsciname2"] <- "PATELLIDAE"
# MNData[which(MNData$newsciname2=="UROPHYCIS REGIUS"), "newsciname2"] <- "UROPHYCIS REGIA"
# MNData[which(MNData$newsciname2=="	BATHYRAJA"), "newsciname2"] <- "BATHYRAJA"
# MNData[which(MNData$newsciname2=="UROPHYCSIS"), "newsciname2"] <- "UROPHYSIS"
# MNData[which(MNData$newsciname2=="HELIPTERUS"), "newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="ILLEX EGGS"), "newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="ICELUSATULA"), "newsciname2"] <- "ICELUS SPATULA"
# MNData[which(MNData$COMM=="MIRROR LANTERNFISH"), "newsciname2"] <- "LAMPADENA SPECULIGERA"
# MNData[which(MNData$COMM=="RED DORY"),"newsciname2"] <- "CYTTOPSIS ROSEA"
# MNData[which(MNData$newsciname2=="OPHICHTUS"),"newsciname2"] <- "OPHICHTHIDAE"
# MNData[which(MNData$newsciname2=="PARAPASIPHAEA SULCATIFRO"),"newsciname2"] <- "PARAPASIPHAE SULCATIFRONS"
# MNData[which(MNData$newsciname2=="STAUROTEUTHIDAE F"),"newsciname2"] <- "STAUROTEUTHIDAE"
# MNData[which(MNData$newsciname2=="	PARALEPIDIDAE"),"newsciname2"] <- "PARALEPIDIDAE"
# MNData[which(MNData$newsciname2=="STICHAEUS PUNCTATUS PUNCTATUS"),"newsciname2"] <- "STICHAEUS PUNCTATUS"
# MNData[which(MNData$newsciname2=="NUT SHELLS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="FINFISHES (NS)"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="ORGANIC DEBRIS"),"newsciname2"] <- NA
# MNData[which(MNData$newsciname2=="BENTHODESMUS ELONGATUS SIMONYI"),"newsciname2"] <- "BENTHODESMUS ELONGATUS"
# MNData[which(MNData$newsciname2=="MERCENARIA MERCINARIA"),"newsciname2"] <- "MERCENARIA MERCENARIA"
# MNData[which(MNData$newsciname2=="PLEURONECIFORMES"),"newsciname2"] <- "PLEURONECTIFORMES"
# MNData[which(MNData$newsciname2=="SPIRONTOCARIS FABRICI"),"newsciname2"] <- "EUALUS FABRICII"
# MNData[which(MNData$newsciname2=="MELAPHAIDAE"),"newsciname2"] <- "MELAMPHAIDAE"
# MNData[which(MNData$newsciname2=="GONATUS STEENSTRUPII"),"newsciname2"] <- "GONATUS STEENSTRUPI"
# MNData[which(MNData$newsciname2=="MERCINARIA MERCINARIA"),"newsciname2"] <- "MERCENARIA MERCENARIA"
# MNData[which(MNData$newsciname2=="POLYMASTIA MAMMILARIS"),"newsciname2"] <- "POLYMASTIA MAMILLARIS"
# MNData[which(MNData$newsciname2=="BATHYRAJAINICAUDA"),"newsciname2"] <- "BATHYRAJA SPINICAUDA"
# MNData[which(MNData$newsciname2=="EVERMANELLA INDICA"),"newsciname2"] <- "EVERMANNELLA INDICA"
# MNData[which(MNData$newsciname2=="OITHONAINIROSTRIS"),"newsciname2"] <- "OITHONA SPINIROSTRIS"
# MNData[which(MNData$newsciname2=="CRINOIDAE"),"newsciname2"] <- "CRINOIDEA"
# MNData[which(MNData$newsciname2=="GRIMOTEUTHIS"),"newsciname2"] <- "GRIMPOTEUTHIS"
# MNData[which(MNData$newsciname2=="	PORIFERA"),"newsciname2"] <- "PORIFERA"
# MNData[which(MNData$newsciname2=="GARLAND HYDROIDS"),"newsciname2"] <- "HYDROZOA"
# MNData[which(MNData$newsciname2=="SECLAR CRUMENOPHTHALMUS"),"newsciname2"] <- "SELAR CRUMENOPHTHALMUS"
# MNData[which(MNData$newsciname2=="CHONDRCHTHYES"),"newsciname2"] <- "CHONDRICHTHYES"
# MNData[which(MNData$newsciname2=="ALEPISAURIDAE  PLAGYODONTIDAE"),"newsciname2"] <- "ALEPISAURIDAE"
# MNData[which(MNData$COMM=="ALEPISAURIDAE..PLAGYODONTIDAE."),"newsciname2"] <- "ALEPISAURIDAE"
# MNData[which(MNData$COMM=="ANGUILLIFORMES..APODES...ORDER."),"newsciname2"] <- "ANGUILLIFORMES"
# MNData[which(MNData$newsciname2=="CERATIUS HOLBOELLI"),"newsciname2"] <- "CERATIAS HOLBOELLI"
# MNData[which(MNData$COMM=="CHIMAERIFORMES..HOLOCEPHALI...ORDER."),"newsciname2"] <- "CHIMAERIDAE"
# MNData[which(MNData$newsciname2=="CRYPTOSARAS COUESI"),"newsciname2"] <- "CRYPTOPSARAS COUESI"
# MNData[which(MNData$COMM=="GADIFORMES..ANACANTHINI...ORDER."),"newsciname2"] <- "GADIFORMES"
# MNData[which(MNData$COMM=="GASTEROSTEIFORMES..ORDER."),"newsciname2"] <- "GASTEROSTEIFORMES"
# MNData[which(MNData$newsciname2=="GASTEROSTEUS ACULATEUS"),"newsciname2"] <- "GASTEROSTEUS ACULEATUS"
# MNData[which(MNData$newsciname2=="GONOSTOMIDAE"),"newsciname2"] <- "GONOSTOMATIDAE"
# MNData[which(MNData$newsciname2=="LEPIDION HALOPORPHYRUS  EQUES"),"newsciname2"] <- "LEPIDION EQUES"
# MNData[which(MNData$COMM=="LOPHIFORMES..PEDICULATI...ORDER."),"newsciname2"] <- "LOPHIIFORMES"
# MNData[which(MNData$newsciname2=="MOLVA BRYKELANGE"),"newsciname2"] <- "MUSTELUS CANIS"
# MNData[which(MNData$newsciname2=="MYOXOCEPHALUS AENEUS"),"newsciname2"] <- "MYOXOCEPHALUS AENAEUS"
# MNData[which(MNData$newsciname2=="PARASUDIS TRUCULENTUS"),"newsciname2"] <- "PARASUDIS TRUCULENTA"
# MNData[which(MNData$COMM=="PLUERONECTIFORMES.HETEROSOMATA..ORDER."),"newsciname2"] <- "PLEURONECTIFORMES"
# MNData[which(MNData$newsciname2=="RAJA  BATHYRAJA INICAUDA"),"newsciname2"] <- "BATHYRAJA SPINICAUDA"
# MNData[which(MNData$newsciname2=="RAJA BATHYPHILA"),"newsciname2"] <- "RAJELLA BATHYPHILA"
# MNData[which(MNData$newsciname2=="RAJA MOLLIS"),"newsciname2"] <- "MALACORAJA SPINACIDERMIS"
# MNData[which(MNData$COMM=="SELACHII..CHONDRICHTHYES...CLASS."),"newsciname2"] <- "CHONDRICHTHYES"
# MNData[which(MNData$newsciname2=="TRACHYRHYNCHUS MURRAYI"),"newsciname2"] <- "TRACHYRINCUS MURRAYI"
# MNData[which(MNData$newsciname2=="WHALEFISHES  REDMOUTH"),"newsciname2"] <- "RONDELETIA LORICATA"
# MNData[which(MNData$COMM=="XENODERMICHTHYS..ALEPOSOMUS..COPEI"),"newsciname2"] <- "XENODERMICHTHYS COPEI"
# MNData[which(MNData$newsciname2=="OPHICHUTHUS"),"newsciname2"] <- "OPHICHTHUS"
# MNData[which(MNData$newsciname2=="MELAMPHAEIDAE"),"newsciname2"] <- "Stephanoberyciformes"
# MNData[which(MNData$newsciname2=="CAREPROCTUS REINHARDTI"),"newsciname2"] <- "CAREPROCTUS REINHARDTI"
# MNData[which(MNData$COMM=="SEA TADPOLE(NS)"),"newsciname2"] <- "CAREPROCTUS REINHARDTI"
# 
# #found more mistakes and fixed a week or two later
# MNData[which(MNData$newsciname2=="Acipenser oxyrhynchus"),"newsciname2"] <- "ACIPENSER OXYRINCHUS"
# MNData[which(MNData$newsciname2=="Aspidophoroides olriki"),"newsciname2"] <- "ASPIDOPHOROIDES OLRIKII"
# MNData[which(MNData$newsciname2=="Ceratias holboelli"),"newsciname2"] <- "CAERATIAS HOLBOELLII"
# MNData[which(MNData$newsciname2=="Cryptopsaras couesi"),"newsciname2"] <- "CRYPTOPSARAS COUESII"
# MNData[which(MNData$newsciname2=="Pandalus propinquus"),"newsciname2"] <- "PANDALUS PROPINQVUS"
# MNData[which(MNData$newsciname2=="Scomberesox saurus"),"newsciname2"] <- "SCOMBERESOX SAURUS SAURUS"
# MNData[which(MNData$newsciname2=="Stichaeus punctatus"),"newsciname2"] <- "STICHAEUS PUNCTATUS PUNCTATUS"
# MNData[which(MNData$newsciname2=="Stomias boa"),"newsciname2"] <- "STOMIAS BOA BOA"
# MNData[which(MNData$fishbase=="STOMIAS BOA FEROX"),"newsciname2"] <- "STOMIAS BOA FEROX"
# MNData[which(MNData$newsciname2=="Synaphobranchus kaupi"),"newsciname2"] <- "SYNAPHOBRANCHUS KAUPII"
# MNData[which(MNData$newsciname2=="Gymnelis viridis"),"newsciname2"] <- "GYMNELUS VIRIDIS"
# MNData[which(MNData$newsciname2=="Osmerus mordax"),"newsciname2"] <- "OSMERUS MORDAX MORDAX"
# MNData[which(MNData$newsciname2=="Cottunculus thompsoni"),"newsciname2"] <- "COTTUNCULUS THOMSONII"
# MNData[which(MNData$newsciname2=="Simenchelys parasiticus"),"newsciname2"] <- "SIMENCHELYS PARASITICA"
# MNData[which(MNData$newsciname2=="Lycodes esmarki"),"newsciname2"] <- "LYCODES ESMARKII"
# MNData[which(MNData$newsciname2=="Lumpenus lumpretaeformis"),"newsciname2"] <- "LUMPENUS LAMPRETAEFORMIS"
# MNData[which(MNData$newsciname2=="Agonus Decagonus"),"newsciname2"] <- "LEPTAGONUS DECAGONUS"
# 
# 
# #number of unique species/classifications
# length(unique(MNData$newsciname2))
# 
# 
# #Create new colum for fishbase species only
# MNData$fishbase = MNData$newsciname2 
# 
# #Make changes to species
# MNData[which(MNData$fishbase=="REINHARDTIUS HIPPOGLOSSOIDES"),"fishbase"] <- "Reinhardtius hippoglossoides"
# MNData[which(MNData$fishbase=="LITHODES"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="LITHODES MAJA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="LOPHIUS AMERICANUS"),"fishbase"] <- "Lophius americanus"
# MNData[which(MNData$fishbase=="LEPOPHIDIUM PROFUNDORUM"),"fishbase"] <- "Lepophidium profundorum"
# MNData[which(MNData$fishbase=="FOETOREPUS AGASSUSZII"),"fishbase"] <- "Foetorepus agassizii"
# MNData[which(MNData$fishbase=="DORYTEUTHIS PEALEII"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="HERMIT CRABS"),"fishbase"] <- "DECAPODA"
# MNData[which(MNData$fishbase=="COTTIDEA"),"fishbase"] <- "COTTIDAE"
# MNData[which(MNData$fishbase=="CYCLOPTERUS AND GASTROPODA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PARALIPARUS COPEI"),"fishbase"] <- "PARALIPARIS COPEI"
# MNData[which(MNData$fishbase=="PARAKICHTHYS DENTATUS"),"fishbase"] <- "PARALICHTHYS DENTATUS"
# MNData[which(MNData$fishbase=="STOMIATOID"),"fishbase"] <- "STOMIIFORMES"
# MNData[which(MNData$fishbase=="CLAMS"),"fishbase"] <- "BIVALVIA"
# MNData[which(MNData$fishbase=="WHELKS"),"fishbase"] <- "BUCCINIDAE"
# MNData[which(MNData$fishbase=="RADICIPES GRACILIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="SAND DOLLARS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PAGUROIDEA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="SEA ANEMONE"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="MYSID SHRIMP"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="VAZELLA POURTALESII"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="VAZELLA POURTALESI"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="AEGA PSORA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="HENRICA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="	CTENOPHORA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CTENOPHORA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PORANIA PULVILIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CEREMASTER GRANULARIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="	PANTOPODA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="SPIRONTOCARIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PORANIOMORPHA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ARTICULATA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="WHELK EGGS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="SABINEA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PSOLUS FABRICII"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PATELLIDAE"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ANTHOZOA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PITAR MORRHUANA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="BLENNIIDAE,LIPOPHRYS,PHOLIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="LYCENCHELYS PAXILLUS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="EPITONIUM"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="GADUS OGAC"),"fishbase"] <- "Gadus ogac"
# MNData[which(MNData$fishbase=="MOLGULA MANHATTENSIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="UROPHYSIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CAREPROCTUS REINHARDTI"),"fishbase"] <- "Careproctus reinhardti"
# MNData[which(MNData$fishbase=="ARGENTINA"),"fishbase"] <- "ARGENTINIDAE"
# MNData[which(MNData$fishbase=="CHRYSOGORGIA AGASSIZII"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="POLYCHAETA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ANTHOMASTUS GRANDIFLORUS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="MYCTOPHUM"),"fishbase"] <- "MYCTOPHIDAE"
# MNData[which(MNData$fishbase=="PARAPASIPHAE SULCATIFRONS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="STAUROTEUTHIDAE"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="COPEPOD"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ECHINUS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CRUSTACEA LARVAE"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CTENOPHORA, CNIDARIA, PORIFERA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="LIPARIDAE"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="COELORHYNCHUS CARMINATUS"),"fishbase"] <- "Macrourinae"
# MNData[which(MNData$fishbase=="COELORINCHUS CARMINATUS"),"fishbase"] <- "Macrourinae"
# MNData[which(MNData$fishbase=="CAREPROCTUS REINHARDTI"),"fishbase"] <- "Careproctus reinhardti"
# MNData[which(MNData$fishbase=="BRYOZOAN ECTOPROCTOA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="LUMPENUS FABRICII"),"fishbase"] <- "Stichaeidae"
# MNData[which(MNData$fishbase=="POLYMASTIA MAMILLARIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="PECTINARIA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="SIPHONOPHORA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CALAPPA MEGALOPS"),"fishbase"] <- "Calappidae"
# MNData[which(MNData$fishbase=="HEMITRIPTERUS AMERICANUS EGGS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ARDENNA GRAVIS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="HIPPOLYTIDAE EGGS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="MARGARITES GROENLANDICA"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="ASTROTECTEN DUPLICATUS"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="HALIEUTICHTHYS BISPINOSUS"),"fishbase"] <- "Ogcocephalidae" #ONLY RECOGNIZE AT FAMILY
# MNData[which(MNData$fishbase=="INVERTEBRATES"),"fishbase"] <- NA
# MNData[which(MNData$fishbase=="CHONDRICHTHYES"),"fishbase"] <- "Elasmobranchii"
# MNData[which(MNData$fishbase=="CHONDRICHTHYES"),"fishbase"] <- "Elasmobranchii"
# 
# 
# 
# #found more mistakes February 28. Fixed and re-ran code
# MNData[which(MNData$COMM=="ACIPENSER.OXYRHYNCHUS"),"fishbase"] <- "ACIPENSER OXYRINCHUS"
# MNData[which(MNData$COMM=="ASPIDOPHOROIDES.OLRIKI"),"fishbase"] <- "ASPIDOPHOROIDES OLRIKII"
# MNData[which(MNData$COMM=="DEEPSEA ANGLER"),"fishbase"] <- "CERATIAS HOLBOELLI"
# MNData[which(MNData$COMM=="CRYPTOSARAS.COUESI"),"fishbase"] <- "CRYPTOPSARAS COUESII"
# MNData[which(MNData$COMM=="PANDALUS.PROPINQUUS"),"fishbase"] <- "PANDALUS PROPINQVUS"
# MNData[which(MNData$sciname=="SCOMBERESOX SAURUS SAURUS"),"fishbase"] <- "SCOMBERESOX SAURUS SAURUS"
# MNData[which(MNData$COMM=="ARCTIC SHANNY"),"newsciname2"] <- "STICHAEUS PUNCTATUS"
# MNData[which(MNData$sciname=="STICHAEUS PUNCTATUS"),"fishbase"] <- "STICHAEUS PUNCTATUS"
# MNData[which(MNData$COMM=="STICHAEUS.PUNCTATUS"),"COMM"] <- "ARCTIC SHANNY"
# MNData[which(MNData$fishbase=="Stomias boa"),"fishbase"] <- "STOMIAS BOA BOA"
# MNData[which(MNData$fishbase=="STOMIAS BOA FEROX"),"fishbase"] <- "STOMIAS BOA FEROX"
# MNData[which(MNData$COMM=="SYNAPHOBRANCHUS.KAUPI"),"fishbase"] <- "SYNAPHOBRANCHUS KAUPII"
# MNData[which(MNData$COMM=="GYMNELIS.VIRIDIS"),"fishbase"] <- "GYMNELUS VIRIDIS"
# MNData[which(MNData$fishbase=="Osmerus mordax"),"fishbase"] <- "OSMERUS MORDAX MORDAX"
# MNData[which(MNData$COMM=="COTTUNCULUS.THOMPSONI"),"fishbase"] <- "COTTUNCULUS THOMSONII"
# MNData[which(MNData$COMM=="SIMENCHELYS.PARASITICUS"),"fishbase"] <- "SIMENCHELYS PARASITICA"
# MNData[which(MNData$COMM=="LYCODES.ESMARKI"),"fishbase"] <- "LYCODES ESMARKII"
# MNData[which(MNData$COMM=="LUMPENUS.LUMPRETAEFORMIS"),"fishbase"] <- "LUMPENUS LAMPRETAEFORMIS"
# MNData[which(MNData$COMM=="AGONUS.DECAGONUS"),"fishbase"] <- "LEPTAGONUS DECAGONUS"
# 
# 
# #Fix species extra spaces in fishbase column
# MNData$fishbase <- unlist(lapply(MNData$fishbase,FUN=SpaceFix))
# MNData$fishbase <- unlist(lapply(MNData$fishbase,FUN=SpaceFix))
# 
# View(MNData)
# 
# #the next species on the list after an error message appears
# #SpeciesList[which(SpeciesList=="Macrourinae")+1]
# 
# SpeciesList <- unique(MNData$fishbase)
# #Run from when there is a species with an error rather than start fro beginning
# #outlist <- lapply(SpeciesList[which(SpeciesList=="STERNOPTYCHIDAE"):length(SpeciesList)],FUN=Classify)
# #run from beginning (this takes a while!!!)
# outlist <- lapply((SpeciesList), FUN=Classify)
# taxInfo <- do.call("rbind", outlist)
# 
# head(taxInfo)
# head(taxInfo)
# 
# newdata <- merge(MNData,taxInfo, id.vars="fishbase")
# View(newdata)
# 
# write.csv(newdata, file='C:/Users/StevensLy/Documents/Database/Data/newdata240217_Lydia.csv')
# 
# newdata <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata240217_Lydia.csv",stringsAsFactors = F)
# head(newdata)
# 
# #remove unessary columns
# head(newdata)
# newdata$X <- NULL
# newdata$X.1 <- NULL
# newdata$X.2 <- NULL
# newdata$X.3 <- NULL
# newdata$X.4 <- NULL
# newdata$sciname <- NULL
# newdata$newsciname <- NULL
# newdata$newsciname2 <- NULL
# 
# #date formatting using the package lubridate to format Maritime Dates
# head(newdata$DATETIME)
# newdata$NEWDATETIME <- dmy_hm(paste(newdata$DATETIME, sep="", tz=""))
# head(newdata$NEWDATETIME)
# head(newdata)
# 
# 
# #created new columns because when I separated into the existing day month year columns, newfoundland data disappeared
# newdata$YEAR_M <- NA
# newdata$MONTH_M <- NA
# newdata$DAY_M <- NA
# 
# newdata$YEAR_M <- format(newdata$NEWDATETIME, '%Y')
# newdata$MONTH_M <- format(newdata$NEWDATETIME, '%m')
# newdata$DAY_M <- format(newdata$NEWDATETIME, '%d')
# newdata$TIME <- format(newdata$NEWDATETIME, '%H:%M')
# head(newdata)
# 
# #combine new year, month, day, time columns with original
# #create new column for day
# newdata$day_final <- ifelse(is.na(newdata$DAY) ==T, newdata$DAY_M, newdata$DAY)
# newdata$month_final <- ifelse(is.na(newdata$MONTH)==T, newdata$MONTH_M, newdata$MONTH)
# newdata$year_final <- ifelse(is.na(newdata$YEAR)==T, newdata$YEAR_M, newdata$YEAR)
# head(newdata)
# 
# #Check to see if worked for newfoundland
# newdata[which(newdata$REGION=="NEWFOUNDLAND"),]
# 
# #remove old date columns to clean up dataframe 
# newdata$DAY <- NULL
# newdata$DAY_M <- NULL
# newdata$MONTH <- NULL
# newdata$MONTH_M <- NULL
# newdata$YEAR <- NULL
# newdata$YEAR_M <- NULL
# newdata$TIME <- NULL
# head(newdata)
# View(newdata)
# 
# #change lowercase to uppercase in colunmn "SEASONS" and 'STRAT_TYPE"
# newdata$SEASON <- toupper(newdata$SEASON)
# newdata$STRAT_TYPE <- toupper(newdata$STRAT_TYPE)
# 
# #merge existing trait data with newdata
# traitdata <- read.csv("C:/Users/StevensLy/Desktop/MPA Framework/traitinformation020317.csv",stringsAsFactors = F)
# newdata2 <- merge(newdata, traitdata, by="species", all=T)
# View(newdata2)
# 
# write.csv(newdata2,file='C:/Users/StevensLy/Documents/Database/Data/newdata2_020317_Lydia.csv')
# 
# 
# #Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
# #This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
# #trawl surveys catching fewer fish (I think?)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# 
# #Create dataframe. Fill it with nothing
# #Make for loop to filter by region and year and calculate frequency (percentage)
# fulldata <- NULL #empty data
# for (i in unique(newdata$REGION)){
#   for (y in unique(newdata[newdata$REGION==i,"year_final"])){
#     
#     temp <- dplyr::filter(newdata,REGION==i,year_final==y)
#     
#     freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
#     colnames(freq_obs) <- c("Species","frequency")
#     
#     freq_obs$Region=i
#     freq_obs$year=y
#     
#     freq_obs$freq_stand <- range01(freq_obs$frequency)
#     
#     fulldata <- rbind(fulldata,freq_obs)
#     
#     
#   } #end of y 'year_final' loop
#   
# } #end of i 'REGION' loop
# 
# fulldata <- fulldata[order(fulldata$Region,fulldata$year),]
# View(fulldata)
# 
# 
# table(fulldata$freq_stand=="1")
# table(fulldata$freq_stand=="0")
# table(fulldata$freq_stand=="0.5")
# 
# 
# #test species
# testplot1<-ggplot(fulldata[fulldata$species=="Hippoglossoides platessoides",])+
#   geom_point(aes(x=fulldata$year,y=fulldata$freq_stand))+
#   ylab("Standardized Frequency")+xlab("Year")+
#   theme_bw()
# 
# 
# 
# #sort species based on how often they were observed
# table(newdata$species)
# table(newdata$species)[which(table(newdata$species)>9000)]
# table(newdata$species)[which(table(newdata$species)<1288)] #Less than 5% of the most captured species
# table(newdata$species)[which(table(newdata$species)>1288)] 
# 
# 
# ########rfishbase practice########
# library(devtools)
# devtools::install_github("ropensci/rfishbase", force=T)
# 
# #cached list of species
# test <- species_list()
# tt <- stocks(test, fields=c("Resilience", "Vulnerability"))
# View(tt)
# 
# #my list of species
# fishinfo <- newdata3$species
# unique(fishinfo)
# stockinfo <- stocks(unique(fishinfo, fields=c("Resilience", "Vulnerability", "SpecCode", "PriceCateg")))
# oxygeninfo <- oxygen(unique(fishinfo))
# ecologyinfo <- ecology(unique(fishinfo))
# reproductioninfo <- reproduction(unique(fishinfo))
# popinfo <- popchar(unique(fishinfo))
# biology <- (unique(fishinfo))
# #common <- sci_to_common(unique(fishinfo))
# #lengthfreq <- length_freq(unique(fishinfo))
# #maturity <- maturity(unique(fishinfo))
# ??rfishbase
# 
# View(stockinfo)
# View(oxygeninfo)
# View(ecologyinfo)
# View(reproductioninfo)
# View(popinfo)
# 
# 
# write.csv(newdata2,file='C:/Users/StevensLy/Documents/Database/Data/newdata3_020317_Lydia.csv')
# write.csv(stockinfo, file='C:/Users/StevensLy/Documents/Database/Data/stockinfo.csv')
# write.csv(oxygeninfo, file='C:/Users/StevensLy/Documents/Database/Data/oxygeninfo.csv')
# write.csv(ecologyinfo, file='C:/Users/StevensLy/Documents/Database/Data/ecologyinfo.csv')
# write.csv(reproductioninfo, file='C:/Users/StevensLy/Documents/Database/Data/reproductioninfo.csv')
# write.csv(popinfo, file='C:/Users/StevensLy/Documents/Database/Data/popinfo.csv')

newdata4 <- read.csv("C:/Users/StevensLy/Documents/Database/Data/newdata170317_Lydia3.csv",stringsAsFactors = F)

head(newdata4)
#View(newdata4)

table(newdata4$Invertebrate==1) #OCCASIONS WITH INVERTEBRATES
table(newdata4$Invertebrate==0) #OCCASIONS WITH VETEBRATES
table(is.na(newdata4$Invertebrate)) #OCCASIONS WITH NA (OCCASIONS WHERE PLANTS WERE FOUND OR SPECIFIC SPECIES COULD NOT BE IDENTIFIED)
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


# #Create a dummy plot related to a specific year
# plotdata1970 <- fulldata[fulldata$year==1970,]
# plot(plotdata1970$freq_stand)
# 
# #Changing species from factor to character (As a factor it ordered the species alphabetically )
# plotdata1970$species <- factor(plotdata1970$species,levels=as.character(plotdata1970$species))
# 
# #1970 Plot Species vs Standardized Freq.
# plot1970<-ggplot(plotdata1970)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+
#   coord_flip()+
#   theme_bw();plot1970
# 
# ##The above comments follow for the remaining years
# 
# plotdata1980 <- fulldata[fulldata$year==1980 ,]
# plot(plotdata1980 $freq_stand)
# plotdata1980 $species <- factor(plotdata1980$species,levels=as.character(plotdata1980$species))
# plot1980<-ggplot(plotdata1980)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+
#   coord_flip()+
#   theme_bw();plot1980
# 
# 
# data1990 <- fulldata[fulldata$year==1990,]
# plot(plotdata1990$freq_stand)
# plotdata1990$species <- factor(plotdata1990$species,levels=as.character(plotdata1990$species))
# plot1990<-ggplot(plotdata1990)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+
#   coord_flip()+
#   theme_bw();plot1990
# 
# ##After 1995 Newfoundland data gets incorportated. It is now sepated by year and region 
# ##This makes the plots more legiable
# 
# plotdata2000 <- fulldata[fulldata$year==2000 & fulldata$Region=="NEWFOUNDLAND",]
# plot(plotdata2000$freq_stand)
# plotdata2000$species <- factor(plotdata2000$species,levels=as.character(plotdata2000$species))
# plot2000N <- ggplot(plotdata2000)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+ggtitle("Newfoundland")+
#   coord_flip()+
#   theme_bw();plot2000N
# 
# plotdata2000 <- fulldata[fulldata$year==2000 & fulldata$Region=="MARITIME",]
# plot(plotdata2000$freq_stand)
# plotdata2000$species <- factor(plotdata2000$species,levels=as.character(plotdata2000$species))
# plot2000M<-ggplot(plotdata2000)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+ggtitle("Maritime")+
#   coord_flip()+
#   theme_bw();plot2000M
# 
# grid.arrange(plot2000M,plot2000N)
# 
# 
# plotdata2010 <- fulldata[fulldata$year==2010 & fulldata$Region=="NEWFOUNDLAND",]
# plot(plotdata2010$freq_stand)
# plotdata2010$species <- factor(plotdata2010$species,levels=as.character(plotdata2010$species))
# plot2010N <- ggplot(plotdata2010)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+ggtitle("Newfoundland")+
#   coord_flip()+
#   theme_bw();plot2010N
# 
# plotdata2010 <- fulldata[fulldata$year==2010 & fulldata$Region=="MARITIME",]
# plot(plotdata2010$freq_stand)
# plotdata2010$species <- factor(plotdata2010$species,levels=as.character(plotdata2010$species))
# plot2010M<-ggplot(plotdata2010)+
#   geom_point(aes(x=species,y=freq_stand))+
#   ylab("Standardized Frequency")+xlab("Species")+ggtitle("Maritime")+
#   coord_flip()+
#   theme_bw();plot2010M
# 
# grid.arrange(plot2010M,plot2010N)


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
goodspecies <- names(which(table(subdata$species)>Precent_1_stations))
pointdata <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("SLONG","SLAT","year_final")]


##NEWFOUNDLAND##
newdata4$tag <- paste(newdata4$REGION,paste(newdata4$year_final,newdata4$month_final,newdata4$day_final,sep="-"),
                      newdata4$SLONG,newdata4$SLAT,sep="_")
#filter dataset by anything after 2005 (last decade) and Newfoundland region
subdata2 <- filter(newdata4,year_final>2005,REGION=="NEWFOUNDLAND")
#what is 1% of the unique species
Precent_1_stations_Newfoundland <- floor(length(unique(subdata2$tag))*0.01)
#which species are found more than 1% of the time
goodspecies_Newfoundland <- names(which(table(subdata2$species)>Precent_1_stations_Newfoundland))
pointdata_Newfoundland <- subdata[!subdata2$species%in%names(which(table(subdata2$species)>Precent_1_stations_Newfoundland)),c("SLONG","SLAT","year_final")]

#list of maritime species
goodspecies
#list of newfoundland species
goodspecies_Newfoundland

##Combining species from each regions
listone2 <- unique(c(goodspecies, goodspecies_Newfoundland))
#looking to see which species were captured in one region, but not the other
setdiff(goodspecies_Newfoundland, goodspecies)
setdiff(goodspecies, goodspecies_Newfoundland)

#sort and list species with the highest number of occasions
occasiontable <- as.data.frame(sort(table(c(subdata$species, subdata2$species))),stringsAsFactors = F)
colnames(occasiontable) <- c("species","count")

occ <- occasiontable[occasiontable$species %in% listone2,]

occasion_plot <- ggplot(occ, aes(x=reorder(species,-count),y=count))+
  geom_bar(stat="identity")+
  ylab("Count")+xlab("Species")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


#plot species by count and region (this is overall count)

#Maritime Region
Msubdata <- as.data.frame(sort(table(subdata$species)),stringsAsFactors = F)
colnames(Msubdata) <- c("species","count")

occasion_plot_M <- ggplot(Msubdata, aes(x=reorder(species, -count), y=count))+
  geom_bar(stat="identity")+
  ylab("Count")+xlab("Species")+ggtitle("Maritime")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#Nefoundland Region
NFsubdata <- as.data.frame(sort(table(subdata2$species)),stringsAsFactors = F)
colnames(NFsubdata) <- c("species","count")

#Newfoundland 
occasion_plot_NL <- ggplot(NFsubdata, aes(x=reorder(species, -count), y=count))+
    geom_bar(stat="identity")+
    ylab("Count")+xlab("Species")+ggtitle("Newfoundland")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))



countdata <- data.frame(species=NULL,count=NULL)
for(i in listone2){
  temp=data.frame(species=i,count=length(which(newdata4$species==i)),stringsAsFactors = F)
  countdata <- rbind(countdata,temp)
  print(i)
}

View(countdata)

newdata4$count=99999
for(i in listone2){
  newdata4[which(newdata4$species==i),"count"]=length(which(newdata4$species==i))
  print(i)
}

newdata4[newdata4$count == 99999,"count"]=NA


##View count data and determine max and min occasions
View(countdata)
max(newdata4$count,na.rm=T)
min(newdata4$count,na.rm=T)
length(countdata)
print(countdata)



