#########################################
##             GBIF DATA             ####
#########################################

## 1. DOWNLOAD GBIF DATA                 ####
## 1.1 LOAD PACKAGES AND ADD CREDENTIALS ####
##---------------------------------------####

# Load packages
library(rgbif)911
library(stringr) # string manipulations (not needed, may also be done by base R)
library(rio)     # data import (not needed, may also be done by base R)
library(dplyr)   # for data-wrangling
library(wicket)  # check WKT strings
library(sp)
library(maptools)
library(raster)
library(rgdal)
library (data.table)
library(dplyr)
library(rgeos)

# Make R ask for you login credentials:
options(gbif_user=rstudioapi::askForPassword("my gbif username"))
options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))

## 1.1 MAKE A DOWNLOAD KEY ####
##-------------------------####

# Create a spatial filter - Trondheim Municipality. Load the shape-file of the municipality border:
# Boundary of Trondheim
Trondheim <- readOGR("ko1601admin_omr_f.shp")
proj4string(Trondheim) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs")   # Define original coordinate reference system

# Make a bbox:
my_wkt <- "POLYGON((10.0012017 63.3061409, 10.0012017 63.5207752, 10.7376525 63.5207752, 10.7376525 63.3061409, 10.0012017 63.3061409))" 
# wicket::validate_wkt(my_wkt)    # Check that it is valid
geom_param <- paste("geometry", "within", my_wkt)

# Make a download key. NB! Maximum of 3 download requests handled simultaneously
download_key <- occ_download(
  'hasCoordinate = TRUE',
  geom_param,
  type = "and"
) %>% 
  occ_download_meta

## 1.2 CREATE THE 'COFFEE BREAK'-FUNCTION TO RETRIEVE THE DATA    ####
##     OBS! This has been done, and should thus not be done again ####
##----------------------------------------------------------------####

# Make the "Coffee Break"-function to retrieve the requested data
# define function
download_GBIF_API <- function(download_key,n_try,Sys.sleep_duration,destfile_name){
  start_time <- Sys.time()
  n_try_count <- 1
  
  download_url <- paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1],sep="")
  
  try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                    quiet=TRUE),silent = TRUE)
  
  while (inherits(try_download, "try-error") & n_try_count < n_try) {   
    Sys.sleep(Sys.sleep_duration)
    n_try_count <- n_try_count+1
    try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                      quiet=TRUE),silent = TRUE)
    print(paste("trying... Download link not ready. Time elapsed (min):",
                round(as.numeric(paste(difftime(Sys.time(),start_time, units = "mins"))),2)))
  }
}


## 1.3 GET THE DATA ####
##------------------####

# Call function
# Create zip-file for download
download_GBIF_API(download_key=download_key,destfile_name="tmp.zip",n_try=25,Sys.sleep_duration=180)   # Try 25 times, wait 3 minutes in between each try
s
# UNZIPPING THE FILE:
# Get a list of the files within the archive by using "list=TRUE" in the unzip function.
archive_files <- unzip("tmp.zip", files = "NULL", list = T) 

# Get the occurrence.txt file in as a dataframe (using import from rio)
occurrence <- import(unzip("tmp.zip",
                           files="occurrence.txt"),header=T,sep="\t")

# If this has been done before, just use the following:
occurrence <- fread("occurrence.txt")

## Cite your data! 
# Finally but not at least, remember to cite your data properly:
paste("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())


## 2. ClEAN AND EXPLORE THE DATA ####
##-------------------------------####

names(occurrence)
str(occurrence[,1:80])
str(occurrence[,81:160])
str(occurrence[,161:235])

# Add species as a factor for evaluation of species numbers:
occurrence$fspecies <- as.factor(occurrence$species)

str(occurrence$fspecies)     # This gives a total of 9118 unique species names (no cleaning done) -
                             # Hence this also includes empty names and those with only genus

# Make the other taxonomic levels factors as well:
occurrence$kingdom <- as.factor(occurrence$kingdom)
occurrence$phylum <- as.factor(occurrence$phylum)
occurrence$class <- as.factor(occurrence$class)
occurrence$order <- as.factor(occurrence$order)
occurrence$family <- as.factor(occurrence$family)
occurrence$genus <- as.factor(occurrence$genus)

# If we divide it into kingdoms/taxonomic groups, the number of levels are:
nlevels(droplevels(occurrence[occurrence$kingdom=="Animalia", "fspecies"])$fspecies)       # Animals - 4712 species
nlevels(droplevels(occurrence[occurrence$phylum=="Chordata", "fspecies"])$fspecies)        # Chordates - 706 species
nlevels(droplevels(occurrence[occurrence$kingdom=="Plantae", "fspecies"])$fspecies)        # Plants - 2150 species
nlevels(droplevels(occurrence[occurrence$phylum=="Tracheophyta", "fspecies"])$fspecies)    # Vascular plants - 1445 species
nlevels(droplevels(occurrence[occurrence$kingdom=="Fungi", "fspecies"])$fspecies)          # Fungi - 2080 species

# Unfortunately, severel records have not been recorded on species level - these are taken out of the analysis
# to ensure a greater quality/certainty of the data:
GBIF <- occurrence
GBIF[GBIF$species==""] <- NA
GBIF[GBIF$fspecies==""] <- NA

# Remove observations with no species name:
GBIF <- GBIF[!is.na(GBIF$fspecies),]         # Decrease from 864715 records to 816247 records
GBIF <- droplevels(GBIF)

# If we divide it into kingdoms/taxonomic groups, the number of levels now are:
nlevels(droplevels(GBIF[GBIF$kingdom=="Animalia", "fspecies"])$fspecies)       # Animals - 4711 species
nlevels(droplevels(GBIF[GBIF$phylum=="Chordata", "fspecies"])$fspecies)        # Chordates - 705 species
nlevels(droplevels(GBIF[GBIF$kingdom=="Plantae", "fspecies"])$fspecies)        # Plants - 2149 species
nlevels(droplevels(GBIF[GBIF$phylum=="Tracheophyta", "fspecies"])$fspecies)    # Vascular plants - 1444 species
nlevels(droplevels(GBIF[GBIF$kingdom=="Fungi", "fspecies"])$fspecies)          # Fungi - 2079 species


## 2.1 Plot for assessment of coordinate uncertainty  ####
##----------------------------------------------------####
# Create files to figure out where to put the lines

tb <- table(occurrence$coordinateUncertaintyInMeters)
bp <- barplot(tb, xlab="Coordinate uncertainty in meters",
              ylab="Number of records", cex.lab=0.8, xaxt="n")

axis(1, at=c(0.7, 81.1, 115.9, 233.5, 342.7, 395.5, 516.7, 606.7, 570.3, 720.7),
     labels=c("1", "71", "100", "200", "300", "354", "500", "707", "1000", "1500"),
     las=2, cex.axis=0.8)
abline(v=395.6, col="red", lty=2)

rm(tb)
rm(bp)

# Remove data with high coordinate uncertainty
GBIF.354 <- GBIF[(GBIF$coordinateUncertaintyInMeters<=354 | is.na(GBIF$coordinateUncertaintyInMeters)),]
GBIF.354 <- droplevels(GBIF.354)

# If we divide it into kingdoms/taxonomic groups:
nlevels(droplevels(GBIF.354[GBIF.354$kingdom=="Animalia", "fspecies"])$fspecies)    # Animals - 4299 species
nlevels(droplevels(GBIF.354[GBIF.354$phylum=="Chordata", "fspecies"])$fspecies)     # Chordates - 654 species
nlevels(droplevels(GBIF.354[GBIF.354$kingdom=="Plantae", "fspecies"])$fspecies)     # Plants - 1593 species
nlevels(droplevels(GBIF.354[GBIF.354$phylum=="Tracheophyta", "fspecies"])$fspecies) # Vascular plants - 1060 species
nlevels(droplevels(GBIF.354[GBIF.354$kingdom=="Fungi", "fspecies"])$fspecies)       # Fungi - 1581 species


## 2.2 Plot the the years ####
##------------------------####

# Make the plots (uncleaned data!)
tb_GBIF <- table(occurrence$year)
bp_GBIF <- barplot(tb_GBIF, xlab="Year", ylab="Number of records", cex.lab=0.8, xaxt="n", main="GBIF")
axis(1, at=c(0.7, 7.9, 25.9, 81.1, 141.1, 189.1, 201.1, 207.1, 213.1, 219.1),
     labels=c("1764", "1820", "1850", "1900", "1950", "1990", "2000", "2005", "2010", "2015"),
     las=2, cex.axis=0.8)

rm(tb_GBIF)
rm(bp_GBIF)


## 2.3 Make the data spatial and clean-up  ####
##-----------------------------------------####
# Make the data spatial
coordinates(GBIF.354) <- ~decimalLongitude+decimalLatitude

# Define the original CRS and change it to match the Trondheim shapefile:
proj4string(GBIF.354) <- CRS("+init=epsg:4326")
GBIF.354 <- spTransform(GBIF.354, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs"))

# Only keep records from within Trondheim borders:
GBIF.354_trd <- GBIF.354[Trondheim,]
GBIF.354_trd@data <- droplevels(GBIF.354_trd@data)

# Have a look at a map:
plot(Trondheim, main="Records from GBIF with coordinate uncertainty <354")
plot(GBIF.354_trd, add=T, pch=".", col="blue", alpha=0.5)

# Factorise some of the columns
GBIF.354_trd@data[, c(
  "license", "kingdom", "phylum", "class", "order", "family", "genus", "species",
  "institutionID", "institutionCode","recordedBy", "basisOfRecord")] <- lapply(GBIF.354_trd@data[,c(
    "license", "kingdom", "phylum", "class", "order", "family", "genus",
    "species","institutionID", "institutionCode", "recordedBy", "basisOfRecord")], factor)

# Once again, have a look at the species richness - some might have dissapperead when removing area:
str(GBIF.354_trd$fspecies)                      # This gives a total of 7066 unique species names (no cleaning done)

# If we divide it into kingdoms/taxonomic groups:
nlevels(droplevels(GBIF.354_trd@data[GBIF.354_trd@data$kingdom=="Animalia", "fspecies"]))   # Animals - 4033 species
nlevels(droplevels(GBIF.354_trd@data[GBIF.354_trd$phylum=="Chordata", "fspecies"]))         # Chordates - 623 species
nlevels(droplevels(GBIF.354_trd@data[GBIF.354_trd$kingdom=="Plantae", "fspecies"]))         # Plants - 1520 species
nlevels(droplevels(GBIF.354_trd@data[GBIF.354_trd$phylum=="Tracheophyta", "fspecies"]))     # Vascular plants - 1017 species
nlevels(droplevels(GBIF.354_trd@data[GBIF.354_trd$kingdom=="Fungi", "fspecies"]))           # Fungi - 1463 species


## 2.4 Basis of record, institution code and license ####
##---------------------------------------------------####

# How does the data look when divided between basisOfRecord:
par(mar=c(7,4.1,4.1,4.1))
barplot(table(GBIF.354_trd@data$basisOfRecord), las=2, cex.names=0.6)

# How does the data look when divided between institusionCode:
par(mar=c(7,4.1,4.1,4.1))
barplot(table(GBIF.354_trd@data$institutionCode), las=2, cex.names=0.6)
# As expected, NOF is the most productive member here
# Problem: I cannot find all the institutions, as GBIF do not provide any kind of list with abbreviations,
# I will wait with this step

# LICENSE
barplot(table(GBIF.354_trd@data$license), las=2, cex.names=0.6)



## 2.5 Sort so that we mainly have newer data to match the maps ####
##--------------------------------------------------------------####
# Check that 'year' is an integer
str(GBIF.354_trd@data$year)

# Remove earlier observations - in in this case, will remove everything before 2013 (meaning: will retain the last 5 years)
GBIF.trd_2013 <- subset(GBIF.354_trd, year>=2013)

# Remove some old calculations, pontetially?
names(GBIF.trd_2013@data)

## 2.6 Have a look at the distribution again ####
##-------------------------------------------####

#  basisOfRecord:
par(mar=c(7,4.1,4.1,2))
barplot(table(GBIF.trd_2013@data$basisOfRecord), las=2, cex.names=0.6)

# institusionCode:
par(mar=c(6,4.1,4.1,2))
barplot(table(GBIF.trd_2013@data$institutionCode), las=2, cex.names=0.6)

# Log barplots
par(mar=c(7,4.1,4.1,2))
barplot(table(GBIF.trd_2013@data$basisOfRecord),
    las=2, log="y", cex.names=0.75)

par(mar=c(6,4.1,4.1,2))
barplot(table(GBIF.trd_2013@data$institutionCode),
        las=2, log="y", cex.names=0.6)

# Have a look at how the plant records are distributed amongs institutions:
{x <- table(GBIF.trd_2013@data[GBIF.trd_2013$kingdom=="Plantae", "institutionCode"])
x <- x[x>0]
barplot(x, las=2, cex.names=0.6)
rm(x)}


#########################################
##        RED- AND BLACKLIST         ####
#########################################

# A general problem here is the nomenclature - the same nomenclature is not used by Artsdatabanken and GBIF.
# This will be taken care of at some point

library(dplyr)
library(taxize)
library(data.table)
library(magrittr)

##--- 1 THE REDLIST  ---####
##------------------------####

# The cleaned redlist
redlist_norway <- read.csv2("redlist_threatened_Norway.csv")
# Clean it up a bit - some weird columns are emerging:
redlist_norway$X <- NULL
redlist_norway <- redlist_norway[,-c(7:9)]

# The names have already been through the Taxonomic Name Resolver once before (in the GBIF2-script),
# so no need to do that again. Instead, load the csv-file with accepted names
checked_names_redlist <- read.csv("checked_names.csv")

# Cleaning
checked_names_redlist$score <- as.numeric(checked_names_redlist$score)    # make score numeric numeric
checked_names_redlist[checked_names_redlist==""] <- NA    # Insert 'NA' if the cell is empty

# View species with a mismatch between submitted and accepted name:
View(checked_names_redlist[checked_names_redlist$score<1,])

checked_names_redlist$X <- NULL


##--- 1.1 FILTER THE NORWEGIAN REDLIST #### 
##---------------------------------------####

# Note that the Redlist I am using, is the "special one" I created based on the data from Artsdatabanken

# Merge the 'redlist_norway' with the 'checked_names' --> making a list of accepted names
# in the data frame!
redlist_norway <- merge(redlist_norway, checked_names_redlist[,1:5],
                        by.x=c("Vitenskapelig.navn"), by.y=c("submittedname"), all=TRUE)

# Make an empty column
redlist_norway$acc_name <- NA

# Fill the column with the accepted (or in other ways best) names:
redlist_norway$acc_name <- ifelse(is.na(redlist_norway$score), as.character(redlist_norway$Vitenskapelig.navn),
                                  ifelse(redlist_norway$score>=0.96 & !is.na(redlist_norway$acceptedname), as.character(redlist_norway$acceptedname),
                                         ifelse(redlist_norway$score>=0.96 & is.na(redlist_norway$acceptedname), as.character(redlist_norway$matchedname),
                                                ifelse(redlist_norway$score<0.96, as.character(redlist_norway$Vitenskapelig.navn), as.character(redlist_norway$Vitenskapelig.navn)))))

# Filter to only contain species from Trondheim
# (Obs! We are still only working with records with a low coordinate uncertainty!)
redlist_TRD.354m <- redlist_norway %>%
  filter(Vitenskapelig.navn %in% GBIF.354_trd@data$species |
           acc_name %in% GBIF.354_trd@data$species)


##--- 1.2 REDLIST TOUCH-UP  ---####
##-----------------------------####

# Prepare a dataset only consisting of redlisted records (new records, low uncertainty):
reds_2013 <- as.data.frame(GBIF.trd_2013) %>%
  filter(species %in% redlist_TRD.354m$Vitenskapelig.navn |
           species %in% redlist_TRD.354m$acc_name)
reds_2013 <- droplevels(reds_2013)


##--- 1.3 EXCLUDE FULLY MARINE SPECIES ---####
##----------------------------------------####
# For the final modeling (categorical), we will exclude fully marine species. These are here removed manually from
# the list after consulting Artsdatabanken. The removal (or lack of same) is defined by the following criteria:
# The "Naturtyper" in Artsdatabanken (https://artsdatabanken.no/Rodliste) is listed only as "M" (marine/"saltvanssystemer),
# or species in other ways known to be fully marine (e.g. Dipturus nidarosiensis - a skate).
# All birds are regarding as somewhat terrestrial (due to nesting), despite their "Naturtype"-classification

# I manually go through the "redlist_TRD.354m" to remove species only found in the ocean:
redlist_TRD_terr <- redlist_TRD.354m[!(redlist_TRD.354m$Vitenskapelig.navn=="Cystophora cristata" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Dipturus nidarosiensis" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Lophelia pertusa" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Lophodoris danielsseni" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Lutraria lutraria" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Mya arenaria" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Ostrea edulis" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Petromyzon marinus" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Pseudaxinella sulcata" |
                                         redlist_TRD.354m$Vitenskapelig.navn=="Squalus acanthias"),]

# Prepare a dataset only consisting of redlisted records (new records, low uncertainty, no marine species):
reds_2013_terr <- as.data.frame(GBIF.trd_2013) %>%
  filter(species %in% redlist_TRD_terr$Vitenskapelig.navn |
           species %in% redlist_TRD_terr$acc_name)
reds_2013_terr <- droplevels(reds_2013_terr)


##--- 2 THE BLACKLIST  ---####
##------------------------####
## OBS! It is very important to remember that the species referred to here are ALIEN species,
# not just the blacklisted ones!

blacklist <- read.csv2("Fremmedart2012.csv")    # Clean-up so that only species from TRD occurs will happen in the next steps
blacklist <- subset(blacklist, Norge.Svalbard=="N")  # Some species are native to mainland Norway, but invasive on Svalbard

# Clean-up some names:
blacklist$Vitenskapelig.navn <- gsub("\xd7","x",blacklist$Vitenskapelig.navn)
blacklist$Vitenskapelig.navn <- gsub("\xeb","ë",blacklist$Vitenskapelig.navn)
blacklist$Vitenskapelig.navn <- gsub("\xf8","oe",blacklist$Vitenskapelig.navn)

blacklist_names <- blacklist[,"Vitenskapelig.navn"]

##--- 2.1 CLEANING UP NAMES - TNRS  ####
##----------------------------------####

# The names have already been through the Taxonomic Name Resolver once before (in the GBIF2-script),
# so no need to do that again. Instead, load the csv-file with accepted names
checked_names_blacklist <- read.csv("checked_names_blacklist.csv")

# Cleaning
checked_names_blacklist$score <- as.numeric(checked_names_blacklist$score)    # make score numeric numeric
checked_names_blacklist[checked_names_blacklist==""] <- NA    # Insert 'NA' if the cell is empty

# View species with a mismatch between submitted and accepted name:
View(checked_names_blacklist[checked_names_blacklist$score<1,])
checked_names_blacklist$X <- NULL


##--- 2.2 PUT ACCEPTED NAMES IN BLACKLIST #### 
##----------------------------------------####

# Merge the 'blacklist' with the 'check_names_blacklist' --> making a list of accepted names
# in the data frame!
blacklist <- merge(blacklist, checked_names_blacklist[,1:5],
                   by.x=c("Vitenskapelig.navn"), by.y=c("submittedname"), all=TRUE)

# Make an empty column
blacklist$acc_name <- NA

# Fill the column with the accepted (or other way best) names:
blacklist$acc_name <- ifelse(is.na(blacklist$score), as.character(blacklist$Vitenskapelig.navn),
                             ifelse(blacklist$score>=0.96 & !is.na(blacklist$acceptedname), as.character(blacklist$acceptedname),
                                    ifelse(blacklist$score>=0.96 & is.na(blacklist$acceptedname), as.character(blacklist$matchedname),
                                           ifelse(blacklist$score<0.96, as.character(blacklist$Vitenskapelig.navn), as.character(blacklist$Vitenskapelig.navn)))))


##--- 2.3 BLACKLIST TOUCH-UP  ---####
##-------------------------------####

# Prepare dataset only consisting of blacklisted records:
blacks_2013 <- as.data.frame(GBIF.trd_2013) %>%
  filter(species %in% blacklist$Vitenskapelig.navn |
           species %in% blacklist$acc_name)
blacks_2013 <- droplevels(blacks_2013)


##--- 1.3 EXCLUDE FULLY MARINE SPECIES ---####
##----------------------------------------####
# For the final modeling (categorical), we will exclude fully marine species. These are here removed manually from
# the list after consulting Artsdatabanken. The removal (or lack of same) is defined by the following criteria:
# In Artsdatabanken (https://artsdatabanken.no/fremmedartslista2018) iit is described as "marine" in the species
# description, or the species in other ways known to be fully marine.
# All birds are regarding as somewhat terrestrial (due to nesting), despite their classification.

# I manually go through the "blacklist" to remove species only found in the ocean:
blacklist_terr <- blacklist[!(blacklist$Vitenskapelig.navn=="Acartia tonsa" |
                                blacklist$Vitenskapelig.navn=="Agardhiella subulata" |
                                blacklist$Vitenskapelig.navn=="Aglaothamnion halliae" |
                                blacklist$Vitenskapelig.navn=="Alitta succinea" |
                                blacklist$Vitenskapelig.navn=="Alkmaria romijni" |
                                blacklist$Vitenskapelig.navn=="Ammothea hilgendorfi" |
                                blacklist$Vitenskapelig.navn=="Amphibalanus amphitrite" |
                                blacklist$Vitenskapelig.navn=="Amphibalanus improvisus" |
                                blacklist$Vitenskapelig.navn=="Anguilla japonica" |
                                blacklist$Vitenskapelig.navn=="Anguilla rostrata" |
                                blacklist$Vitenskapelig.navn=="Anotrichium furcellatum" |
                                blacklist$Vitenskapelig.navn=="Antithamnion densum" |
                                blacklist$Vitenskapelig.navn=="Antithamnionella spirographidis" |
                                blacklist$Vitenskapelig.navn=="Antithamnionella ternifolia" |
                                blacklist$Vitenskapelig.navn=="Asparagopsis armata" |
                                blacklist$Vitenskapelig.navn=="Bonnemaisonia hamifera" |
                                blacklist$Vitenskapelig.navn=="Botrylloides violaceus" |
                                blacklist$Vitenskapelig.navn=="Bugula neritina" |
                                blacklist$Vitenskapelig.navn=="Bugula stolonifera" |
                                blacklist$Vitenskapelig.navn=="Callinectes sapidus" |
                                blacklist$Vitenskapelig.navn=="Caprella mutica" |
                                blacklist$Vitenskapelig.navn=="Celtodoryx ciocalyptoides" |
                                blacklist$Vitenskapelig.navn=="Cercopagis pengoi" |
                                blacklist$Vitenskapelig.navn=="Chara connivens" |
                                blacklist$Vitenskapelig.navn=="Chionoecetes opilio" |
                                blacklist$Vitenskapelig.navn=="Colpomenia peregrina" |
                                blacklist$Vitenskapelig.navn=="Cordylophora caspia" |
                                blacklist$Vitenskapelig.navn=="Corella eumyota" |
                                blacklist$Vitenskapelig.navn=="Corynophlaea verruculiformis" |
                                blacklist$Vitenskapelig.navn=="Crassostrea gigas" |
                                blacklist$Vitenskapelig.navn=="Crassostrea virginica" |
                                blacklist$Vitenskapelig.navn=="Cryptonemia hibernica" |
                                blacklist$Vitenskapelig.navn=="Dasya baillouviana" |
                                blacklist$Vitenskapelig.navn=="Diadumene lineata" |
                                blacklist$Vitenskapelig.navn=="Didemnum vexillum" |
                                blacklist$Vitenskapelig.navn=="Dreissena bugensis" |
                                blacklist$Vitenskapelig.navn=="Edwardsiella lineata" |
                                blacklist$Vitenskapelig.navn=="Elminius modestus" |
                                blacklist$Vitenskapelig.navn=="Ensis directus" |
                                blacklist$Vitenskapelig.navn=="Eriocheir sinensis" |
                                blacklist$Vitenskapelig.navn=="Evadne anonyx" |
                                blacklist$Vitenskapelig.navn=="Ficopomatus enigmaticus" |
                                blacklist$Vitenskapelig.navn=="Gammarus tigrinus" |
                                blacklist$Vitenskapelig.navn=="Glossanodon leioglossus" |
                                blacklist$Vitenskapelig.navn=="Goniadella gracilis" |
                                blacklist$Vitenskapelig.navn=="Gonionemus vertens" |
                                blacklist$Vitenskapelig.navn=="Gracilaria gracilis" |
                                blacklist$Vitenskapelig.navn=="Gracilaria vermiculophylla" |
                                blacklist$Vitenskapelig.navn=="Grateloupia subpectinata" |
                                blacklist$Vitenskapelig.navn=="Grateloupia turuturu" |
                                blacklist$Vitenskapelig.navn=="Hemigrapsus sanguineus" |
                                blacklist$Vitenskapelig.navn=="Hemigrapsus takanoi" |
                                blacklist$Vitenskapelig.navn=="Heterosiphonia japonica" |
                                blacklist$Vitenskapelig.navn=="Homarus americanus" |
                                blacklist$Vitenskapelig.navn=="Hydroides dianthus" |
                                blacklist$Vitenskapelig.navn=="Ischyrocerus commensalis" |
                                blacklist$Vitenskapelig.navn=="Lomentaria hakodatensis" |
                                blacklist$Vitenskapelig.navn=="Marenzelleria neglecta" |
                                blacklist$Vitenskapelig.navn=="Marenzelleria viridis" |
                                blacklist$Vitenskapelig.navn=="Micropogonias undulatus" |
                                blacklist$Vitenskapelig.navn=="Mnemiopsis leidyi" |
                                blacklist$Vitenskapelig.navn=="Molgula manhattensis" |
                                blacklist$Vitenskapelig.navn=="Monocorophium sextonae" |
                                blacklist$Vitenskapelig.navn=="Neogobius melanostomus" |
                                blacklist$Vitenskapelig.navn=="Neosiphonia harveyi" |
                                blacklist$Vitenskapelig.navn=="Ocenebra inornata" |
                                blacklist$Vitenskapelig.navn=="Ostrea chilensis" |
                                blacklist$Vitenskapelig.navn=="Paralithodes camtschatica" |
                                blacklist$Vitenskapelig.navn=="Perophora japonica" |
                                blacklist$Vitenskapelig.navn=="Petricolaria pholadiformis" |
                                blacklist$Vitenskapelig.navn=="Polyopes lancifolius" |
                                blacklist$Vitenskapelig.navn=="Polysiphonia subtilissima" |
                                blacklist$Vitenskapelig.navn=="Potamopyrgus antipodarum" |
                                blacklist$Vitenskapelig.navn=="Pseudobacciger harengulae" |
                                blacklist$Vitenskapelig.navn=="Rapana venosa" |
                                blacklist$Vitenskapelig.navn=="Sargassum muticum" |
                                blacklist$Vitenskapelig.navn=="Sebastes schlegelii" |
                                blacklist$Vitenskapelig.navn=="Solieria chordalis" |
                                blacklist$Vitenskapelig.navn=="Sphaerococcus coronopifolius" |
                                blacklist$Vitenskapelig.navn=="Styela clava" |
                                blacklist$Vitenskapelig.navn=="Teredo navalis" |
                                blacklist$Vitenskapelig.navn=="Tricellaria inopinata" |
                                blacklist$Vitenskapelig.navn=="Ulva pertusa" |
                                blacklist$Vitenskapelig.navn=="Umbra pygmaea" |
                                blacklist$Vitenskapelig.navn=="Undaria pinnatifida" |
                                blacklist$Vitenskapelig.navn=="Urosalpinx cinerea" |
                                blacklist$Vitenskapelig.navn=="Venerupis philippinarum" |
                                blacklist$Vitenskapelig.navn=="Vimba vimba" |
                                blacklist$Vitenskapelig.navn=="Watersipora subtorquata"),]

# Prepare dataset only consisting of blacklisted records:
blacks_2013_terr <- as.data.frame(GBIF.trd_2013) %>%
  filter(species %in% blacklist_terr$Vitenskapelig.navn |
           species %in% blacklist_terr$acc_name)
blacks_2013_terr <- droplevels(blacks_2013_terr)


##--- 2. MAKE THE DATAFRAMES SPATIAL PRIOR TO RASTERIZING AND PLOTTING  ---####
##-------------------------------------------------------------------------####

# Assign the coordinate columns
coordinates(reds_2013) <- ~decimalLongitude+decimalLatitude
coordinates(blacks_2013) <- ~decimalLongitude+decimalLatitude

coordinates(reds_2013_terr) <- ~decimalLongitude+decimalLatitude
coordinates(blacks_2013_terr) <- ~decimalLongitude+decimalLatitude

# OBS! We see here that the number of records are the same for the dataframes with and without marine species - 
# thus, for data with this spatial resolution and timeframe, they are not an issue.

# Define the CRS
proj4string(reds_2013) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
proj4string(blacks_2013) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

# Have a look:
plot(Trondheim, border="black")  # or use: DivMap_3(AR5, Trondheim, "")
plot(GBIF.trd_2013, col="blue", pch="*", add=TRUE)
plot(reds_2013, col="red", pch=3, add=TRUE)
plot(blacks_2013, col="black", pch=4, add=TRUE)



############################################
##        RASTERIZATION OF DATA         ####
############################################

##--- 1. PLOT AND RASTER OF ALL THE RECORDS ---####
##---------------------------------------------####
library(sp)
library(maptools)
library(raster)
library(rgdal)
library (data.table)
library(dplyr)
library(rgeos)

TrdBound <- Trondheim@bbox     # Find extent of the data

# create a raster with 500 m resolution - this should be changed to match the coordinate uncertainty of the data
TrdRaster <- SpatialGrid(GridTopology(TrdBound[,1], # lower left coordinate
                                      c(500,500), # pixel length and height
                                      c(round(diff(TrdBound[1,])/500), round(diff(TrdBound[2,])/500)))) # number of pixels in x and y direction
proj4string(TrdRaster) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs")

## If wanted, they can be plotted:
# plot(Trondheim)
# plot(TrdRaster, add=TRUE, col="gray")

# As a test, plot the points
plot(Trondheim, main="All records") +
  # plot(TrdRaster, add=T, col="gray") +
  plot(GBIF.trd_2013, add=T, col="blue", pch=".") +
  scalebar(d=4000, xy=c(551000, 7020300), type="bar", divs=4, cex=0.6)

# Find out which pixel each observation belongs to
RasterPoint <- over(GBIF.trd_2013, TrdRaster)

# Add that to the dataframe
GBIF.trd_2013@data$Pixelnr <- RasterPoint

# Make a table with the number of observations per pixel
pointstable <- table(RasterPoint)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.GBIF <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.GBIF[as.numeric(names(pointstable))] <- pointstable
TrdPointsRaster <- SpatialGridDataFrame(TrdRaster, d=data.frame(Npoints=RasterDF.GBIF))

# Plot it
# plot(TrdPointsRaster, col=rev(gray(0:max(TrdPointsRaster@data[,"Npoints"]) / max(TrdPointsRaster@data[,"Npoints"]))), main="All records")  +   
#  plot(Trondheim, add=TRUE)      
# points(GBIF.trd, col="green2", pch=1)    # This can be done to check that the points line up correctly


##--- 2. PLOT AND RASTER OF REDLISTED RECORDS  ---####
##------------------------------------------------####

# As a test, plot the points
par(mar=c(5.1,4.1,4.1,2.1))
plot(Trondheim, main="Redlisted records") +
  #plot(TrdRaster, add=T, col="gray") +
  plot(reds_2013, add=T, col="red", pch=".") 
# scalebar(d=4000, xy=c(551000, 7020300), type="bar", divs=4, cex=0.6)

# Find out which pixel each observation belongs to
RasterRed <- over(reds_2013, TrdRaster)

# Add that to the dataframe
reds_2013@data$Pixelnr <- RasterRed

# Make a table with the number of observations per pixel
redstable <- table(RasterRed)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.red <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.red[as.numeric(names(redstable))] <- redstable
TrdRedRaster <- SpatialGridDataFrame(TrdRaster, d=data.frame(Nred=RasterDF.red))

# Plot it
# plot(TrdRedRaster, col=rev(gray(0:max(TrdRedRaster@data[,"Nred"]) / max(TrdRedRaster@data[,"Nred"]))), main="Redlisted records")  # Colour might not work - if not, replace "red" with "gray"
# plot(Trondheim, add=TRUE)        
# points(reds_2013, col="red", pch=1)    # This can be done to check that the points line up correctly


##--- 3. PLOT AND RASTER OF BLACKLISTED RECORDS ---####
##-------------------------------------------------####

# As a test, plot the points
plot(Trondheim, main="Blacklisted records") +
  # plot(TrdRaster, add=T, col="gray") +
  plot(blacks_2013, add=T, col="black", pch=".") 

# Find out which pixel each observation belongs to
RasterBlack <- over(blacks_2013, TrdRaster)

# Add that to the dataframe
blacks_2013@data$Pixelnr <- RasterBlack

# Make a table with the number of observations per pixel
blackstable <- table(RasterBlack)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.black <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.black[as.numeric(names(blackstable))] <- blackstable
TrdBlackRaster <- SpatialGridDataFrame(TrdRaster, d=data.frame(Nblack=RasterDF.black))

# Plot it
#plot(TrdBlackRaster, col=rev(gray(0:max(TrdBlackRaster@data[,"Nblack"]) / max(TrdBlackRaster@data[,"Nblack"]))), main="Blacklisted records")  
#plot(Trondheim, add=TRUE)
# points(blacks, col="black", pch=1)    # This can be done to check that the points line up correctly

# OBS! REMEMBER TO DELETE RASTER-PLOT! IT CAUSES PROBLEMS IN THE PLOTTING DEVICE!

# Most of the raster just produced here are not used again - thus we delete them to save some memory:
rm(RasterDF.black)
rm(RasterDF.GBIF)
rm(RasterDF.red)



###############################
##        LANDCOVER        ####
###############################

##--- 1. AR5-MAPS   ---####
##---------------------####

# Load the shapefiles of the current AR5-maps
AR5 <- readOGR(dsn=path.expand("/home/ahomez/t/tanjakp/AR5"), layer="ar5")
# Check the CRS:
AR5@proj4string
# Change the CRS to match the data points
AR5 <- spTransform(AR5, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs"))

# We have a minor issue in that the AR5 goes slightly outside of the municipality border,
# which might cause some small issues later - therefore, we crop the AR5-polygons by "Trondheim" or "kommunegrense:
AR5_crop <- gIntersection(AR5, Trondheim, byid = TRUE)

# It is now just a SpatialPolygon - to re-create the dataframe-part:
row.names(AR5_crop) <- gsub(" 0", "", row.names(AR5_crop))    # Make the rownames identical to AR5
keep <- row.names(AR5_crop)                                   # The rownames to keep
AR5_data <- as.data.frame(AR5@data[keep, ])                   # The data from AR5 that we need
AR5_crop <- SpatialPolygonsDataFrame(AR5_crop, AR5_data)      # Make it a SpatialPolygonsDataframe

rm(keep)
rm(AR5_data)

# Have a look at a map:
AR5map(Trondheim, AR5_crop, "AR5 - 2013")
AR5legend(x=580000, y=7044010)
plot(Trondheim, add=TRUE)


##--- 2. ASSIGN LANDCOVER TO POINTS  ---####
##--------------------------------------####

reds_2013$Cover_AR5 <- reds_2013 %over% AR5_crop
summary(reds_2013)
blacks_2013$Cover_AR5 <- blacks_2013 %over% AR5_crop
summary(blacks_2013)

GBIF.trd_2013$Cover_AR5 <- GBIF.trd_2013 %over% AR5_crop
summary(GBIF.trd_2013)


##--- 3. CALCULATING HABITAT COVER IN THE RASTER CELLS  ---####
##---------------------------------------------------------####
# Convert the grid to a raster
ras <- raster(TrdRaster)

# Convert the raster to a polygon - for some reasons I can't get the 'Grid2Polygons' to work, hence the extra step
TrdRast_AR5 <- rasterToPolygons(ras, dissolve=FALSE)
TrdRast_AR5@data$Pixelnr <- c(1:NROW(TrdRast_AR5@data))

# Determine the overlap between the raster-polygons (grid cells) and the land cover
cover_in_grid_AR5 <- intersect(AR5_crop, TrdRast_AR5)

# We now need to make a new column, so that each point only have ONE type of land cover
# We have too many different cover categories to do this with a nested 'ifelse' (more than 50) - so we have to do
# it with several functions, unfortunately:
#############
cover_in_grid_AR5[cover_in_grid_AR5$artype==11, "Cvr"] <- "Developed_area"

cover_in_grid_AR5[cover_in_grid_AR5$artype==12, "Cvr"] <- "Communications_traffic"

cover_in_grid_AR5[cover_in_grid_AR5$artype==21 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Fully_cultivated_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==21 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Fully_cultivated_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==22 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Superficially_cultivated_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==22 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Superficially_cultivated_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==22 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Superficially_cultivated_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Hfgl_coniferous_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Hfgl_coniferous_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Hfgl_coniferous_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Hfgl_deciduous_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Hfgl_deciduous_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Hfgl_deciduous_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Hfgl_mix_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Hfgl_mix_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Hfgl_mix_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==39 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Hfgl_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==39 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Hfgl_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==39 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Hfgl_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==99 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Hfgl_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==99 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Hfgl_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==23 & cover_in_grid_AR5$artreslag==99 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Hfgl_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_coniferous_impediment_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==42, "Cvr"] <- "Forest_coniferous_impediment_bedrock"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_coniferous_impediment_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_coniferous_impediment_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_coniferous_impediment_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_coniferous_lowprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_coniferous_lowprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_coniferous_lowprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_coniferous_lowprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_coniferous_lowprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_coniferous_mediumprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_coniferous_mediumprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_coniferous_mediumprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_coniferous_mediumprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_coniferous_highprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_coniferous_highprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_coniferous_highprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_coniferous_veryhighprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_coniferous_veryhighprod_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_deciduous_impediment_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==42, "Cvr"] <- "Forest_deciduous_impediment_bedrock"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_deciduous_impediment_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_deciduous_impediment_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_deciduous_impediment_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_deciduous_lowprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_deciduous_lowprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_deciduous_lowprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_deciduous_lowprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_deciduous_mediumprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_deciduous_mediumprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_deciduous_mediumprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_deciduous_mediumprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_deciduous_highprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_deciduous_highprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_deciduous_highprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_deciduous_veryhighprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_deciduous_veryhighprod_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_mix_impediment_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==42, "Cvr"] <- "Forest_mix_impediment_bedrock"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_mix_impediment_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_mix_impediment_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_mix_impediment_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_mix_lowprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_mix_lowprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_mix_lowprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==12 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_mix_lowprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_mix_mediumprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Forest_mix_mediumprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_mix_mediumprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_mix_mediumprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Forest_mix_highprod_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_mix_highprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_mix_highprod_organic_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Forest_mix_veryhighprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==30 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==45, "Cvr"] <- "Forest_mix_veryhighprod_organic_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==41, "Cvr"] <- "Ofg_impediment_boulder"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==42, "Cvr"] <- "Ofg_impediment_bedrock"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Ofg_impediment_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Ofg_impediment_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==11 & cover_in_grid_AR5$argrunnf==46, "Cvr"] <- "Ofg_impediment_artificial"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Ofg_mediumprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==13 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Ofg_mediumprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==43, "Cvr"] <- "Ofg_highprod_shallow_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==14 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Ofg_highprod_soil"
cover_in_grid_AR5[cover_in_grid_AR5$artype==50 & cover_in_grid_AR5$arskogbon==15 & cover_in_grid_AR5$argrunnf==44, "Cvr"] <- "Ofg_veryhighprod_soil"

cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==11, "Cvr"] <- "Marsh_coniferous_impediment"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==12, "Cvr"] <- "Marsh_coniferous_lowprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==13, "Cvr"] <- "Marsh_coniferous_mediumprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==14, "Cvr"] <- "Marsh_coniferous_highprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==31 & cover_in_grid_AR5$arskogbon==15, "Cvr"] <- "Marsh_coniferous_veryhighprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==11, "Cvr"] <- "Marsh_deciduous_impediment"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==12, "Cvr"] <- "Marsh_deciduous_lowprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==13, "Cvr"] <- "Marsh_deciduous_mediumprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==14, "Cvr"] <- "Marsh_deciduous_highprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==32 & cover_in_grid_AR5$arskogbon==15, "Cvr"] <- "Marsh_deciduous_veryhighprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==11, "Cvr"] <- "Marsh_mix_impediment"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==12, "Cvr"] <- "Marsh_mix_lowprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==13, "Cvr"] <- "Marsh_mix_mediumprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==14, "Cvr"] <- "Marsh_mix_highprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==33 & cover_in_grid_AR5$arskogbon==15, "Cvr"] <- "Marsh_mix_veryhighprod"
cover_in_grid_AR5[cover_in_grid_AR5$artype==60 & cover_in_grid_AR5$artreslag==39 & cover_in_grid_AR5$arskogbon==11, "Cvr"] <- "Marsh_open_impediment"

cover_in_grid_AR5[cover_in_grid_AR5$artype==70, "Cvr"] <- "Snow_glacier"

cover_in_grid_AR5[cover_in_grid_AR5$artype==81, "Cvr"] <- "Freshwater"

cover_in_grid_AR5[cover_in_grid_AR5$artype==82, "Cvr"] <- "Ocean"

cover_in_grid_AR5[cover_in_grid_AR5$artype==99, "Cvr"] <- "Not_mapped"


cover_in_grid_AR5$Cvr <- as.factor(cover_in_grid_AR5$Cvr)
#############

# Calculate the area (in square meters of each land cover type in each cell)
cover_in_grid_AR5$area_m2<-gArea(cover_in_grid_AR5, byid=T)

library(reshape2)
# make it a regular dataframe
df_AR5 <- data.frame(cover_in_grid_AR5)

# Only retain the interesting information
d_melt_AR5 <- melt(df_AR5, id=c("Cvr", "Pixelnr"), measure= "area_m2")
head(d_melt_AR5)

# Calculate the area of each kind of land cover within each rastercell around a point
d_pivot_AR5 <- dcast(d_melt_AR5, Pixelnr~Cvr, fun.aggregate = sum)   
str(d_pivot_AR5)

# Assign coverage data to the observation points
TrdRast_AR5@data <- merge(TrdRast_AR5@data, d_pivot_AR5, by="Pixelnr", all=TRUE) 

# Remove some of the unnecessary files:
rm(df_AR5)
rm(TrdBound)



## Compare species list from the full dataset and the used dataset ####
##-----------------------------------------------------------------####
splist <- data.frame(full_list=c(levels(occurrence$fspecies)))
splist$model_list <- NA

for(i in 1:NROW(splist)){
  splist[i,"model_list"] <- ifelse(splist[i,"full_list"] %in% levels(GBIF.trd_2013@data[,"species"]),
                                   paste(splist[i,"full_list"]),
                                   paste("NA"))
}
splist[splist$model_list=="NA","model_list"] <- NA


# Find out how the lost species distributes:
View(as.data.frame(sort(table(occurrence[occurrence$fspecies %in% splist[is.na(splist$model_list),"full_list"], "species"]),
                        decreasing = TRUE)))
# To have a better look at the "lost species":
lost <- as.data.frame(c(sort(table(occurrence[occurrence$fspecies %in% splist[is.na(splist$model_list),"full_list"], "species"]), decreasing = TRUE)))
lost <- tibble::rownames_to_column(lost, var = "species")

# Distribution among phyla:
sort(table(occurrence[occurrence$species %in% lost$species, "phylum"]), decreasing=TRUE)
View(as.data.frame(occurrence[occurrence$species %in% lost[-1, "species"], c("kingdom", "phylum", "class", "species")]))  # Remove the ones with no species name

