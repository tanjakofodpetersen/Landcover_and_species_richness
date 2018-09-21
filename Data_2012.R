###########################
## DATA FROM 2008-2012 ####
###########################
# OBS! We are still working only with data with a coordinate uncertainty less than 354 m

# Remove data outside of the wanted time period:
GBIF.trd_2012 <- subset(GBIF.354_trd, year<=2012 & year>=2008)    # Significantly fewer point than the newer dataset

# Remove some old calculations
names(GBIF.trd_2012@data)



#########################################
##        RED- AND BLACKLIST         ####
#########################################

# A general problem here is the nomenclature - the same nomenclature is not used by Artsdatabanken and GBIF.
# This will be taken care of at some point

library(dplyr)
library(taxize)
library(data.table)
library(magrittr)

##--- 1. THE REDLIST  ---####
##-----------------------####

# Prepare a dataset only consisting of redlisted records (older records, low uncertainty):
reds_2012 <- as.data.frame(GBIF.trd_2012) %>%
  filter(species %in% redlist_TRD.354m$Vitenskapelig.navn |
           species %in% redlist_TRD.354m$acc_name)
reds_2012 <- droplevels(reds_2012)

# Make a dataframe based on the "non-marine" redlist, to see if these have been influencing our analyses:
reds_2012_terr <- as.data.frame(GBIF.trd_2012) %>%
  filter(species %in% redlist_TRD_terr$Vitenskapelig.navn |
           species %in% redlist_TRD_terr$acc_name)
reds_2012_terr <- droplevels(reds_2012_terr)        # Seemingly, one observation is removed by this (thus not a lot)
                                                    # However, some of the following analyses should be redone, if we are
                                                    # to use this dataset



##--- 2. THE BLACKLIST  ---####
##-------------------------####

# Prepare dataset only consisting of blacklisted records:
blacks_2012 <- as.data.frame(GBIF.trd_2012) %>%
  filter(species %in% blacklist$Vitenskapelig.navn |
           species %in% blacklist$acc_name)
blacks_2012 <- droplevels(blacks_2012)

# Make a dataframe based on the "non-marine" redlist, to see if these have been influencing our analyses:
blacks_2012_terr <- as.data.frame(GBIF.trd_2012) %>%
  filter(species %in% blacklist_terr$Vitenskapelig.navn |
           species %in% blacklist_terr$acc_name)
blacks_2012_terr <- droplevels(blacks_2012_terr)         # No issues here


##--- 3. MAKE THE DATAFRAMES SPATIAL PRIOR TO RASTERIZING AND PLOTTING  ---####
##-------------------------------------------------------------------------####

# Assign the coordinate columns
coordinates(reds_2012) <- ~decimalLongitude+decimalLatitude
coordinates(blacks_2012) <- ~decimalLongitude+decimalLatitude

# Define the CRS
proj4string(reds_2012) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
proj4string(blacks_2012) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

# Have a look, and compare the maps to the newer maps:
par(mfrow=c(2,3))
par(mar=c(1,1,4,1))
plot(Trondheim, border="black", main="All records, 2013-18")
plot(GBIF.trd_2013, col="blue", pch=".", add=TRUE)
plot(Trondheim, border="black", main="Threatened records, 2013-18")
plot(reds_2013, col="red", pch=".", add=TRUE)
plot(Trondheim, border="black", main="Alien records, 2013-18")
plot(blacks_2013, col="black", pch=".", add=TRUE)

plot(Trondheim, border="black", main="All records, 2008-12")
plot(GBIF.trd_2012, col="blue", pch=".", add=TRUE)
plot(Trondheim, border="black", main="Threatened records, 2008-12")
plot(reds_2012, col="red", pch=".", add=TRUE)
plot(Trondheim, border="black", main="Alien records, 2008-12")
plot(blacks_2012, col="black", pch=".", add=TRUE)


############################################
##        RASTERIZATION OF DATA         ####
############################################

##--- 1. PLOT AND RASTER OF ALL THE RECORDS ---####
##---------------------------------------------####

# Find out which pixel each observation belongs to
RasterPoint_2012 <- over(GBIF.trd_2012, TrdRaster)

# Add that to the dataframe
GBIF.trd_2012@data$Pixelnr <- RasterPoint_2012

# Make a table with the number of observations per pixel
pointstable_2012 <- table(RasterPoint_2012)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.GBIF_2012 <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.GBIF_2012[as.numeric(names(pointstable_2012))] <- pointstable_2012
TrdPointsRaster_2012 <- SpatialGridDataFrame(TrdRaster, d=data.frame(Npoints=RasterDF.GBIF_2012))


##--- 2. PLOT AND RASTER OF REDLISTED RECORDS  ---####
##------------------------------------------------####
# Find out which pixel each observation belongs to
RasterRed_2012 <- over(reds_2012, TrdRaster)

# Add that to the dataframe
reds_2012@data$Pixelnr <- RasterRed_2012

# Make a table with the number of observations per pixel
redstable_2012 <- table(RasterRed_2012)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.red_2012 <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.red_2012[as.numeric(names(redstable_2012))] <- redstable_2012
TrdRedRaster_2012 <- SpatialGridDataFrame(TrdRaster, d=data.frame(Nred=RasterDF.red_2012))


##--- 3. PLOT AND RASTER OF BLACKLISTED RECORDS ---####
##-------------------------------------------------####

# Find out which pixel each observation belongs to
RasterBlack_2012 <- over(blacks_2012, TrdRaster)

# Add that to the dataframe
blacks_2012@data$Pixelnr <- RasterBlack_2012

# Make a table with the number of observations per pixel
blackstable_2012 <- table(RasterBlack_2012)

# Generate a spatialGridDataFrame with number of points per pixel
RasterDF.black_2012 <- rep(0, nrow(coordinates(TrdRaster)))
RasterDF.black_2012[as.numeric(names(blackstable_2012))] <- blackstable_2012
TrdBlackRaster_2012 <- SpatialGridDataFrame(TrdRaster, d=data.frame(Nblack=RasterDF.black_2012))

rm(RasterDF.black_2012)
rm(RasterDF.GBIF_2012)
rm(RasterDF.red_2012)


###############################
##        LANDCOVER        ####
###############################

##--- 1. AR5-MAPS   ---####
##---------------------####

# Load the shapefiles of the current AR5-maps
AR5_2012 <- readOGR(dsn=path.expand("/home/ahomez/t/tanjakp/AR5"), layer="ar5_2012")
# Check the CRS:
AR5_2012@proj4string
# Change the CRS to match the data points
AR5_2012 <- spTransform(AR5_2012, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs"))

# Change the names of the columns to match the ones from the newest AR5:
library(dplyr)
AR5_2012@data <- rename(AR5_2012@data, c("arealres_4"="artype",
                        "arealres_3"="artreslag",
                        "arealres_2"="arskogbon",
                        "arealressu"="argrunnf"))

names(AR5_2012@data)

# Have a look at a map:
AR5map.2012(Trondheim, AR5_2012, "AR5 - 2012")
AR5legend.2012(x=580000, y=7044010)
plot(Trondheim, add=TRUE)

# OBS! It is VERY important to note here that there are no polygons coded "81" or "82", but only "80" - thus
# sea- and freshwater has not been separated


##--- 2. ASSIGN LANDCOVER TO POINTS  ---####
##--------------------------------------####

reds_2012$Cover_AR5 <- reds_2012 %over% AR5_2012
summary(reds_2012)
blacks_2012$Cover_AR5 <- blacks_2012 %over% AR5_2012
summary(blacks_2012)

GBIF.trd_2012$Cover_AR5 <- GBIF.trd_2012 %over% AR5_2012
summary(GBIF.trd_2012)


##--- 3. CALCULATING HABITAT COVER IN THE RASTER CELLS  ---####
##---------------------------------------------------------####

# We now need to have a SpatialPolygonsDataframe with the habitat cover for this as well:
TrdRast_AR5.2012 <- rasterToPolygons(ras, dissolve=FALSE)
TrdRast_AR5.2012@data$Pixelnr <- c(1:NROW(TrdRast_AR5.2012@data))

# Determine the overlap between the raster-polygons (grid cells) and the land cover. We have some issues
# with the shapefiles from the municipality (I get an error regarding self-intersection). So first we'll work
# around that with the following lines of code:
sum(gIsValid(AR5_2012, byid=TRUE)==FALSE)          # Is there a problem?
AR5_2012 <- gBuffer(AR5_2012, byid=TRUE, width=0)  # If yes, work around the problem
sum(gIsValid(AR5_2012, byid=TRUE)==FALSE)          # Check again

cover_in_grid_AR5.2012 <- intersect(AR5_2012, TrdRast_AR5.2012)

# We now need to make a new column, so that each point only have ONE type of land cover
# We have too many different cover categories to do this with a nested 'ifelse' (more than 50) - so we have to do
# it with several functions, unfortunately.

# OBS! Some of the landcover types found in the newer maps are not present in the older ones
# some of these has been removed as they caused problems - everything should be correctly classified now
#############
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==11, "Cvr"] <- "Developed_area"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==12, "Cvr"] <- "Communications_traffic"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==21 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Fully_cultivated_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==21 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Fully_cultivated_organic_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==22 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Superficially_cultivated_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==23 & cover_in_grid_AR5.2012$artreslag==99 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Hfgl_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==23 & cover_in_grid_AR5.2012$artreslag==99 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Hfgl_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==23 & cover_in_grid_AR5.2012$artreslag==99 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Hfgl_organic_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_coniferous_impediment_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==42, "Cvr"] <- "Forest_coniferous_impediment_bedrock"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_coniferous_impediment_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_coniferous_impediment_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_coniferous_impediment_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_coniferous_lowprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_coniferous_lowprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_coniferous_lowprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_coniferous_lowprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_coniferous_lowprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_coniferous_mediumprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_coniferous_mediumprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_coniferous_mediumprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_coniferous_mediumprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_coniferous_highprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_coniferous_highprod_organic_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_deciduous_impediment_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==42, "Cvr"] <- "Forest_deciduous_impediment_bedrock"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_deciduous_impediment_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_deciduous_impediment_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_deciduous_impediment_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_deciduous_lowprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_deciduous_lowprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_deciduous_lowprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_deciduous_lowprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_deciduous_mediumprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_deciduous_mediumprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_deciduous_mediumprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_deciduous_mediumprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_deciduous_highprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_deciduous_highprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_deciduous_highprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_deciduous_highprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==15 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_deciduous_veryhighprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==15 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_deciduous_veryhighprod_organic_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_mix_impediment_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==42, "Cvr"] <- "Forest_mix_impediment_bedrock"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_mix_impediment_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_mix_impediment_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_mix_impediment_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_mix_lowprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_mix_lowprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_mix_lowprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==12 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_mix_lowprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_mix_mediumprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_mix_mediumprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_mix_mediumprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_mix_mediumprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Forest_mix_highprod_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Forest_mix_highprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_mix_highprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_mix_highprod_organic_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==15 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Forest_mix_veryhighprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==30 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==15 & cover_in_grid_AR5.2012$argrunnf==45, "Cvr"] <- "Forest_mix_veryhighprod_organic_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==41, "Cvr"] <- "Ofg_impediment_boulder"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==42, "Cvr"] <- "Ofg_impediment_bedrock"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Ofg_impediment_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Ofg_impediment_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==11 & cover_in_grid_AR5.2012$argrunnf==46, "Cvr"] <- "Ofg_impediment_artificial"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Ofg_mediumprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==13 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Ofg_mediumprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==43, "Cvr"] <- "Ofg_highprod_shallow_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==14 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Ofg_highprod_soil"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==50 & cover_in_grid_AR5.2012$arskogbon==15 & cover_in_grid_AR5.2012$argrunnf==44, "Cvr"] <- "Ofg_veryhighprod_soil"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==11, "Cvr"] <- "Marsh_coniferous_impediment"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==12, "Cvr"] <- "Marsh_coniferous_lowprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==13, "Cvr"] <- "Marsh_coniferous_mediumprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==14, "Cvr"] <- "Marsh_coniferous_highprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==31 & cover_in_grid_AR5.2012$arskogbon==15, "Cvr"] <- "Marsh_coniferous_veryhighprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==11, "Cvr"] <- "Marsh_deciduous_impediment"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==12, "Cvr"] <- "Marsh_deciduous_lowprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==13, "Cvr"] <- "Marsh_deciduous_mediumprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==14, "Cvr"] <- "Marsh_deciduous_highprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==32 & cover_in_grid_AR5.2012$arskogbon==15, "Cvr"] <- "Marsh_deciduous_veryhighprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==11, "Cvr"] <- "Marsh_mix_impediment"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==12, "Cvr"] <- "Marsh_mix_lowprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==13, "Cvr"] <- "Marsh_mix_mediumprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==14, "Cvr"] <- "Marsh_mix_highprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==33 & cover_in_grid_AR5.2012$arskogbon==15, "Cvr"] <- "Marsh_mix_veryhighprod"
cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==60 & cover_in_grid_AR5.2012$artreslag==39 & cover_in_grid_AR5.2012$arskogbon==11, "Cvr"] <- "Marsh_open_impediment"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==70, "Cvr"] <- "Snow_glacier"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==80, "Cvr"] <- "Water"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==81, "Cvr"] <- "Freshwater"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==82, "Cvr"] <- "Ocean"

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==99, "Cvr"] <- "Not_mapped"


cover_in_grid_AR5.2012$Cvr <- as.factor(cover_in_grid_AR5.2012$Cvr)
#############

# Calculate the area (in square meters of each land cover type in each cell)
cover_in_grid_AR5.2012$area_m2<-gArea(cover_in_grid_AR5.2012, byid=T)

library(reshape2)
# make it a regular dataframe
df_AR5.2012 <- data.frame(cover_in_grid_AR5.2012)

# Only retain the interesting information
d_melt_AR5.2012 <- melt(df_AR5.2012, id=c("Cvr", "Pixelnr"), measure= "area_m2")
head(d_melt_AR5.2012)

# Calculate the area of each kind of land cover within each rastercell around a point
d_pivot_AR5.2012 <- dcast(d_melt_AR5.2012, Pixelnr~Cvr, fun.aggregate = sum)   
str(d_pivot_AR5.2012)

# Assign coverage data to the observation points
TrdRast_AR5.2012@data <- merge(TrdRast_AR5.2012@data, d_pivot_AR5.2012, by="Pixelnr", all=TRUE) 

# Remove some of the unnecessary files:
rm(df_AR5.2012)
rm(TrdBound)




##############################################################
###  COMMUNITY MATRICES FOR SPECIES RICHNESS CALCULATIONS  ###
##############################################################

# This is done for the data from 2008-2012 and a coordinate undertainty of less than 354 m

##--- 1.1 CREATE THE NEEDED OBJECTS, AND MAKE THE COMMUNITY MATRIX  ---####
##--- 1.1.1 ALL DATA                                                ---####
##---------------------------------------------------------------------####

## Here: made for the GBIF-data with an uncertainty <354 m collected between year 2008-2012

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
GBIF.trd_2012@data$fPixelnr <- factor(GBIF.trd_2012@data$Pixelnr) 
GBIF.trd_2012@data <- droplevels(GBIF.trd_2012@data)

rar.data_2012 <- as.data.frame(GBIF.trd_2012@data[,c("species", "fPixelnr")])
rar.data_2012 <- droplevels(rar.data_2012)

# Create empty matrix
est_2012 <- matrix(data=NA, ncol=nlevels(rar.data_2012$species), nrow=nlevels(rar.data_2012$fPixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est_2012) <- levels(rar.data_2012$species)
rownames(est_2012) <- levels(rar.data_2012$fPixelnr)

# Make a tally of the rar.data
tallied_2012 <- rar.data_2012 %>%
  group_by(species, fPixelnr) %>%
  tally()

##--- 1.1.2 RED- AND BLACKLISTED ---####
##----------------------------------####
# Red and blacklisted observations only:
reds_2012@data$fPixelnr <- factor(reds_2012@data$Pixelnr) 
reds_2012@data <- droplevels(reds_2012@data)
blacks_2012@data$fPixelnr <- factor(blacks_2012@data$Pixelnr) 
blacks_2012@data <- droplevels(blacks_2012@data)

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
rar.reds_2012 <- as.data.frame(reds_2012@data[,c("species", "fPixelnr")])
rar.reds_2012 <- droplevels(rar.reds_2012)
rar.blacks_2012 <- as.data.frame(blacks_2012@data[,c("species", "fPixelnr")])
rar.blacks_2012 <- droplevels(rar.blacks_2012)

# Create empty matrix
est.reds_2012 <- matrix(data=NA, ncol=nlevels(rar.reds_2012$species), nrow=nlevels(rar.reds_2012$fPixelnr))
est.blacks_2012 <- matrix(data=NA, ncol=nlevels(rar.blacks_2012$species), nrow=nlevels(rar.blacks_2012$fPixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est.reds_2012) <- levels(rar.reds_2012$species)
rownames(est.reds_2012) <- levels(rar.reds_2012$fPixelnr)
colnames(est.blacks_2012) <- levels(rar.blacks_2012$species)
rownames(est.blacks_2012) <- levels(rar.blacks_2012$fPixelnr)

# Make a tally of the rar.data
tallied.reds_2012 <- rar.reds_2012 %>%
  group_by(species, fPixelnr) %>%
  tally()

tallied.blacks_2012 <- rar.blacks_2012 %>%
  group_by(species, fPixelnr) %>%
  tally()


##--- 1.2 RUN THE FUNCTION AND FILL IN THE COMMUNITY MATRIX  ---####
##--------------------------------------------------------------####

## ALL OBSERVATIONS:
## The full matrix and dataframe are too much to handle for the programme (it either gets stuck or takes way too long.
## Therefore, the est-matrix are split up)
est1_2012 <- est_2012[,c(1:600)]      
est2_2012 <- est_2012[,c(601:1200)]    
est3_2012 <- est_2012[,c(1201:1800)]    
est4_2012 <- est_2012[,c(1801:2296)]   

# Run the function(s)
# 1.
for(r in 1:dim(est1_2012)[1]){
  for(c in 1:dim(est1_2012)[2]){
    est1_2012[r,c]=ntally(i=r, j=c, Tally=tallied_2012, Com.matrix=est1_2012)}
}

# 2.
for(r in 1:dim(est2_2012)[1]){
  for(c in 1:dim(est2_2012)[2]){
    est2_2012[r,c]=ntally(i=r, j=c, Tally=tallied_2012, Com.matrix=est2_2012)}
}

# 3.
for(r in 1:dim(est3_2012)[1]){
  for(c in 1:dim(est3_2012)[2]){
    est3_2012[r,c]=ntally(i=r, j=c, Tally=tallied_2012, Com.matrix=est3_2012)}
}

# 4.
for(r in 1:dim(est4_2012)[1]){
  for(c in 1:dim(est4_2012)[2]){
    est4_2012[r,c]=ntally(i=r, j=c, Tally=tallied_2012, Com.matrix=est4_2012)}
}

# Combine all the small matrices to one large community matrix
est.all_2012 <- cbind(est1_2012, est2_2012, est3_2012, est4_2012) 


# Save the results, so you can reload them without running the function again
write.csv(est.all_2012, file="Comm_matrix_GBIF_2012_August2018.csv")
## Try to reload the dataframe to check that everything is okay:
# est.all <- read.csv("Comm_matrix_GBIF_August2018.csv", row.names=1)

# Remove all the smaller est-files
#rm(est_2012)
#rm(est1_2012)
#rm(est2_2012)
#rm(est3_2012)
#rm(est4_2012)

### REDLISTED
for(r in 1:dim(est.reds_2012)[1]){
  for(c in 1:dim(est.reds_2012)[2]){
    est.reds_2012[r,c]=ntally(i=r, j=c, Tally=tallied.reds_2012, Com.matrix=est.reds_2012)}
}

# Save the results, so you can reload them without running the function again
write.csv(est.reds_2012, file="Comm_matrix_GBIF_2012_August2018_redlisted.csv")
# est.reds <- read.csv("Comm_matrix_GBIF_August2018_redlisted.csv", header=TRUE, row.names = 1)

### BLACKLISTED
for(r in 1:dim(est.blacks_2012)[1]){
  for(c in 1:dim(est.blacks_2012)[2]){
    est.blacks_2012[r,c]=ntally(i=r, j=c, Tally=tallied.blacks_2012, Com.matrix=est.blacks_2012)}
}

# Save the results, so you can reload them without running the function again
write.csv(est.blacks_2013, file="Comm_matrix_GBIF_August2018_2012_blacklisted.csv")
# est.blacks <- read.csv("Comm_matrix_GBIF_August2018_blacklisted.csv", header=TRUE, row.names = 1)

# Check that the community matrices seems reasonable:
dim(est.all_2012)
hist(rowSums(est.all_2012))
hist(log(rowSums(est.all_2012)))
View(as.data.frame(rowSums(est.all_2012)))

dim(est.reds_2012)
hist(log(rowSums(est.reds_2012)))
hist(rowSums(est.reds_2012))
View(as.data.frame(rowSums(est.reds_2012)))

dim(est.blacks_2012)
hist(log(rowSums(est.blacks_2012)))
hist(rowSums(est.blacks_2012))
View(as.data.frame(rowSums(est.blacks_2012)))


##--- 2. CALCULATE THE ESTIMATED SPECIES RICHNESS FOR EACH OF THE GRID CELLS ---####
##------------------------------------------------------------------------------####
# Estimate species richness for each site/grid cell, and save the results. For cells with only one
# observation, the returned value will  be NA
est.sp_2012 <- estimateR(est.all_2012)
est.sp.reds_2012 <- estimateR(est.reds_2012)
est.sp.blacks_2012 <- estimateR(est.blacks_2012)

# Rotate it and make it a dataframe
est.sp_2012 <- as.data.frame(t(est.sp_2012))
est.sp.reds_2012 <- as.data.frame(t(est.sp.reds_2012))
est.sp.blacks_2012 <- as.data.frame(t(est.sp.blacks_2012))

# Add the rownames (Pixelnr) as a variable
est.sp_2012 <- tibble::rownames_to_column(est.sp_2012, var="Pixelnr")
est.sp.reds_2012 <- tibble::rownames_to_column(est.sp.reds_2012, var="Pixelnr")
est.sp.blacks_2012 <- tibble::rownames_to_column(est.sp.blacks_2012, var="Pixelnr")

# Add these calculations (Species richness) to the SpatialPointsDataframes (later: also to the TrdRast SpatialPolygonsDataFrame)
GBIF.trd_2012 <- merge(GBIF.trd_2012, est.sp_2012, all=TRUE)
reds_2012 <- merge(reds_2012, est.sp.reds_2012, all=TRUE)
blacks_2012 <- merge(blacks_2012, est.sp.blacks_2012, all=TRUE)

# Also add the the number of estimated redlisted species to the TrdRast-data for plotting
# First, retrieve the needed data for each of the raster cells 
GBIF_sp_2012 <- unique(GBIF.trd_2012@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(GBIF_sp_2012) <- c("Pixelnr", "S.obs_2012", "S.chao1_2012", "se.chao1_2012")

reds_sp_2012 <- unique(reds_2012@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(reds_sp_2012) <- c("Pixelnr", "S.obs_reds_2012", "S.chao1_reds_2012", "se.chao1_reds_2012")

blacks_sp_2012 <- unique(blacks_2012@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(blacks_sp_2012) <- c("Pixelnr", "S.obs_blacks_2012", "S.chao1_blacks_2012", "se.chao1_blacks_2012")

# Merge both into the TrdRast_AR5
TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, GBIF_sp_2012, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5.2012)

TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, reds_sp_2012, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5.2012)

TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, blacks_sp_2012, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5.2012)

rm(GBIF_sp_2012)
rm(reds_sp_2012)
rm(blacks_sp_2012)



##--- 3. NUMBER OF SAMPLES/RECORDS IN EACH CELL ---####
##-------------------------------------------------####
# Add number of observations to the dataframes (these are picked up from the "Rasterization'-script).
# This needs to be done for all records, red- and blacklisted.
# First, add the pixelnumber as a column in TrdPointsRaster, and then merge them:
TrdPointsRaster_2012@data <- tibble::rownames_to_column(TrdPointsRaster_2012@data)
TrdRedRaster_2012@data <- tibble::rownames_to_column(TrdRedRaster_2012@data)
TrdBlackRaster_2012@data <- tibble::rownames_to_column(TrdBlackRaster_2012@data)

GBIF.trd_2012@data <- merge(GBIF.trd_2012@data, TrdPointsRaster_2012@data, by.x="Pixelnr", by.y="rowname", all=TRUE)
names(GBIF.trd_2012)[names(GBIF.trd_2012) == 'Npoints'] <- 'Ntotal'
reds_2012@data <- merge(reds_2012@data, TrdRedRaster_2012@data, by.x="Pixelnr", by.y="rowname", all=TRUE)
blacks_2012@data <- merge(blacks_2012@data, TrdBlackRaster_2012@data, by.x="Pixelnr", by.y="rowname", all=TRUE)


# Also add these to the TrdRast_AR5 dataframe for plotting - first, retrieve the needed data, and remove
# cells not within Trondheim borders:
cells_in_trondheim_2012 <- TrdRast_AR5.2012@data[!is.na(TrdRast_AR5.2012@data$Communications_traffic), "Pixelnr"]

GBIF_sp_2012.2 <- unique(GBIF.trd_2012@data[,c("Pixelnr", "Ntotal")])
GBIF_sp_2012.2 <- GBIF_sp_2012.2 %>%
  filter(Pixelnr %in% cells_in_trondheim_2012)
reds_sp_2012.2 <- unique(reds_2012@data[,c("Pixelnr", "Nred")])
reds_sp_2012.2 <- reds_sp_2012.2 %>%
  filter(Pixelnr %in% cells_in_trondheim_2012)
blacks_sp_2012.2 <- unique(blacks_2012@data[,c("Pixelnr", "Nblack")])
blacks_sp_2012.2 <- blacks_sp_2012.2 %>%
  filter(Pixelnr %in% cells_in_trondheim_2012)

# Merge both into the TrdRast_AR5
TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, GBIF_sp_2012.2, by="Pixelnr", all=TRUE)
TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, reds_sp_2012.2, by="Pixelnr", all=TRUE)
TrdRast_AR5.2012 <- merge(TrdRast_AR5.2012, blacks_sp_2012.2, by="Pixelnr", all=TRUE)

rm(GBIF_sp_2012.2)
rm(reds_sp_2012.2)
rm(blacks_sp_2012.2)


##--- 4. HAVE A LOOK AT SOME MAPS ---####
##------------------------------------####
# All observations, reds and blacks - make rasters and maps
raster_2012 <- stack(rasterize(TrdRast_AR5.2012, ras,
                               field = "S.obs_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               field = "S.chao1_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               fiel = "Ntotal"),
                     rasterize(TrdRast_AR5.2012, ras,
                               field = "S.obs_reds_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               field = "S.chao1_reds_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               fiel = "Nred"),
                     rasterize(TrdRast_AR5.2012, ras,
                               field = "S.obs_blacks_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               field = "S.chao1_blacks_2012"),
                     rasterize(TrdRast_AR5.2012, ras,
                               fiel = "Nblack"))
names(raster_2012) <- c("S.obs_2012", "S.chao1_2012", "Ntotal",
                        "S.obs_reds_2012", "S.chao1_reds_2012", "Nred",
                        "S.obs_blacks_2012", "S.chao1_blacks_2012", "Nblack")

crs(raster_2012) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(raster_2012)
#plot(Trondheim, add=TRUE)

# We can plot it with more meaningfull/comparable axes
par(mfrow=c(3,3))
par(mar=c(0.5,0.5, 1, 2))
plot(raster_2012$S.obs_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed species richness", zlim=c(0,250))
plot(raster_2012$S.chao1_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated species richness", zlim=c(0,5800))
plot(raster_2012$Ntotal, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of records", zlim=c(0,29100))

plot(raster_2012$S.obs_reds_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed redlisted species richness", zlim=c(0,250))
plot(raster_2012$S.chao1_reds_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated redlisted species richness", zlim=c(0,250))
plot(raster_2012$Nred, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of redlisted records", zlim=c(0,6500))

plot(raster_2012$S.obs_blacks_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed blacklisted species richness", zlim=c(0,250))
plot(raster_2012$S.chao1_blacks_2012, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated blacklisted species richness", zlim=c(0,250))
plot(raster_2012$Nblack, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of blacklisted records", zlim=c(0,6500))





##--- 4.1 POTENTIAL "PRUNING" OF DATA ---####
##---------------------------------------####

# An issue for our further analyses is the uneven number of records/samples in each grid cell.
# This can either be dealt with by using the number of observed species as an offset, and thus
# all cells for which we have data.
# Another solution is to only work with the ESR - to do this, the data needs some "pruning":

# Have a look at how the number of samples in each grid cell is ditributed.
# Most cells have 0 observations in them - hence this bar is cut out of the plot:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(TrdRast_AR5.2012@data$Ntotal), ylim=c(0,50), xlab="Total number of records in cell", ylab="Frequency")
barplot(table(TrdRast_AR5.2012@data$Nred), ylim=c(0,50), xlab="Number of redlisted records in cell", ylab="Frequency")
barplot(table(TrdRast_AR5.2012@data$Nblack), ylim=c(0,50), xlab="Number of alien records in cell", ylab="Frequency")

# One rule for pruning could be only using cells with a minimum number of records, e.g. 10, 15, 20 etc.
# Using all cells in the grid gives us 2249 cells.
# Number of cells with any number of records in (thus, no zero or NA):
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                        !is.na(TrdRast_AR5.2012@data$Communications_traffic),])   # 968

# Number of cells with 10 or more records:
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>=10 &
                        !is.na(TrdRast_AR5.2012@data$Communications_traffic),])   # 583
# Number of cells with 15 or more records:
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>=15 &
                        !is.na(TrdRast_AR5.2012@data$Communications_traffic),])   # 516
# Number of cells with 20 or more records:
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>=20 &
                        !is.na(TrdRast_AR5.2012@data$Communications_traffic),])   # 471


# Another criterion for pruning is coefficient of variation (the SE of the estimated number of species/ESR)
# This should not be too large, e.g. less than 0.2 census Ballesteros-Meija et al. (2013).
# First, we add that calculation to the dataframe, then look at the results:
TrdRast_AR5.2012@data$CoV_2012 <- TrdRast_AR5.2012@data$se.chao1_2012/TrdRast_AR5.2012@data$S.chao1_2012

range(TrdRast_AR5.2012@data[!is.na(TrdRast_AR5.2012@data$CoV_2012), "CoV_2012"])
hist(TrdRast_AR5.2012@data$CoV_2012)
View(TrdRast_AR5.2012@data[!is.na(TrdRast_AR5.2012@data$CoV_2012), c(1, 63:65, 72, 75)])

# Number of cells with a CoV < 0.2 (less than or equal to):
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$CoV_2012<=0.2 &
                        !is.na(TrdRast_AR5.2012@data$CoV_2012),])   # 374
# Number of cells with a CoV < 0.25 (less than or equal to):
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$CoV_2012<=0.25 &
                        !is.na(TrdRast_AR5.2012@data$CoV_2012),])   # 431
# Number of cells with a CoV < 0.3 (less than or equal to):
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$CoV_2012<=0.3 &
                        !is.na(TrdRast_AR5.2012@data$CoV_2012),])   # 494

### In the further analyses, we will continue with some minorly "pruned" data - in this case,
# we'll go for cells with more than 10 records and a CoV <0.25. The total number of cells
# used for analysis are then:
NROW(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$CoV_2012<=0.25 &
                        TrdRast_AR5.2012@data$Ntotal>=10 &
                        !is.na(TrdRast_AR5.2012@data$Communications_traffic) &
                        !is.na(TrdRast_AR5.2012@data$CoV_2012),])   # 271

# The Pixelnr's are: 
cells_analyses.2012 <- TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$CoV_2012<=0.25 &
                                     TrdRast_AR5.2012@data$Ntotal>=10 &
                                     !is.na(TrdRast_AR5.2012@data$Communications_traffic) &
                                     !is.na(TrdRast_AR5.2012@data$CoV_2012), "Pixelnr"]
TrdRast_analyses.2012 <- TrdRast_AR5.2012[TrdRast_AR5.2012@data$CoV_2012<=0.25 &
                                  TrdRast_AR5.2012@data$Ntotal>=10 &
                                  !is.na(TrdRast_AR5.2012@data$Communications_traffic) &
                                  !is.na(TrdRast_AR5.2012@data$CoV_2012), ]

# Have a look at a map:
AR5map.2012(Trondheim, AR5_2012, "")
plot(Trondheim, add=TRUE)

plot(TrdRast_AR5.2012, border="gray", add=T)   # The entire grid
plot(TrdRast_AR5.2012[TrdRast_AR5.2012@data$Ntotal>0 &
                   !is.na(TrdRast_AR5.2012@data$Ntotal),],
     col=rgb(190, 190, 190, alpha = 175, maxColorValue = 255),border=NA, add=T)  # Cells with records
plot(TrdRast_analyses.2012,
     col=rgb(255, 0, 0, alpha = 125, maxColorValue = 255), border=NA, add=T) # Cells after pruning



###############################################
###  (SPATIAL) MODELS OF LANDCOVER EFFECTS  ###
###     ON THREATENED AND ALIEN SPECIES     ###
###############################################

# The idea now is to see if we can see any significant relationships between the landcover in a grid
# cell, and the number of threatened and/or alien species in that particular grid cell.
# However, as the sampling effort in the grid cells are uneven, we need to make measures to counteract this.

# This can either be dealt with by using the number of observed species as an offset, and thus include
# all cells for which we have data.
# Pros: (1) more grid cells included, (2) less "fidgeting" with the raw data
# Cons: (1) Have in previous analyses shown to be extremely difficult to make work with spatial models,
#       (2) Data might be highly skewed and uncertain
# Another way to deal with the issues are using the ESR - hence the previous "pruning"

##--- 1. USING OFFSET OR 'PRUNED' DATA? ---####
##-----------------------------------------####

# If we only look at the "pruned" cells, we can compare the total ESR and the ESR of threatened and alien
# species, and evaluate if these numbers are worth continuing with:

View(TrdRast_analyses.2012@data[, c(1, 63:64, 66:67, 69:70)])
TrdRast_analyses.2012@data$S.chao1_2012 < TrdRast_analyses.2012@data$S.chao1_reds_2012
TrdRast_analyses.2012@data$S.chao1_2012 < TrdRast_analyses.2012@data$S.chao1_blacks_2012
# In all cases, the total ESR is never lower then the other - good!

# We can have a look at the distribution of the species numbers from each of the two datasets
# to check for normality:
par(mfrow=c(1,3))

### All cells with observations:
barplot(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                      !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_2012"]),
        main="Obs. species 2012", xlab="# species")
barplot(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                      !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_reds_2012"]),
        main="Obs. reds 2012", xlab="# species")
barplot(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                      !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_blacks_2012"]),
        main="Obs. blacks 2012", xlab="# species")

# Here we have a very high number of cells with very few observations - it might be reasonable to
# look at log-transformed numbers instead:
barplot(log(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                          !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_2012"])),
        main="log(Obs.species 2012)", xlab="# species")
barplot(log(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                          !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_reds_2012"])),
        main="log(Obs.reds 2012)", xlab="# species")
barplot(log(table(TrdRast_AR5.2012@data[TrdRast_AR5.2012@data$Ntotal>0 &
                                          !is.na(TrdRast_AR5.2012@data$Ntotal), "S.obs_blacks_2012"])),
        main="log(Obs.blacks 2012)", xlab="# species")

# It is unclear to me which data would be best to use.


### The 'pruned' dataset:
barplot(table(TrdRast_analyses.2012$S.obs_2012),
        main="Obs.species_pruned 2012", xlab="# species")
barplot(table(TrdRast_analyses.2012$S.obs_reds_2012),
        main="Obs.reds_pruned 2012", xlab="# species")
barplot(table(TrdRast_analyses.2012$S.obs_blacks_2012),
        main="Obs.blacks_pruned 2012", xlab="# species")

barplot(log(table(TrdRast_analyses.2012$S.obs_2012)),
        main="log(Obs.species_pruned 2012)", xlab="# species")
barplot(log(table(TrdRast_analyses.2012$S.obs_reds_2012)),
        main="log(Obs.reds_pruned 2012)", xlab="# species")
barplot(log(table(TrdRast_analyses.2012$S.obs_blacks_2012)),
        main="log(Obs.blacks_pruned 2012)", xlab="# species")
# These were the raw numbers - if we're using this dataset. we should continue the analyses on the ESR:
barplot(table(TrdRast_analyses.2012$S.chao1_2012),
        main="ESR 2012", xlab="# species")
barplot(table(TrdRast_analyses.2012$S.chao1_reds_2012),
        main="ESR_reds 2012", xlab="# species")
barplot(table(TrdRast_analyses.2012$S.chao1_blacks_2012),
        main="ESR_blacks 2012", xlab="# species")

barplot(log(table(TrdRast_analyses.2012$S.chao1_2012)),
        main="log(ESR) 2012", xlab="# species")
barplot(log(table(TrdRast_analyses.2012$S.chao1_reds_2012)),
        main="log(ESR_reds) 2012", xlab="# species")
barplot(log(table(TrdRast_analyses.2012$S.chao1_blacks_2012)),
        main="log(ESR_blacks) 2012", xlab="# species")

# This last dataset might be better off - I'll continue with that for now, and discuss how to proceed with
# supervisors later on



##--- 2. DECIDING ON WHAT VARIABLES TO USE ---####
##--------------------------------------------####

# If I only want to look at the landcover types in the grid cells, I need to pick out the important ones.
# Otherwise, we have far to many for them all to be significant.


##--- 2.1 DATA EXPLORATION ---####
##----------------------------####
# A lot of the area inside the administrative area is covered by ocean.
# It could pontetially give us more information, if we look at the percentage of each habitat type within
# each grid cell, and then make boxplots?

# Calculate the total area in each cell (some cells are less than 250000 since not the entire cell is
# within Trondheim boundaries)
TrdRast_rel.2012 <- TrdRast_analyses.2012
TrdRast_rel.2012@data$total_area <- rowSums(TrdRast_rel.2012@data[, 3:62])

# Recalculate all areas as percentage of total cell area:
for(i in 1:dim(TrdRast_rel.2012@data)[1]) {
  for(j in 3:62) {
    TrdRast_rel.2012@data[i,j] = (TrdRast_rel.2012@data[i,j])/(TrdRast_rel.2012@data[i,"total_area"])
  }
}

# Replace 'NA's with zeros:
TrdRast_rel.2012@data[is.na(TrdRast_rel.2012@data)] <- 0

# Potentially plot this as a boxplot, just to have an idea of the variation:
par(mfrow=c(1,1))
par(mar=c(12,4.1,4.1,2.1))
boxplot(TrdRast_rel.2012@data[, c(3:62)], las=2, cex.names=0.6, cex.axis=0.6)

# Have a look at some relationships, outliers etc.:
source("HighstatLibV10.R")

# Outliers:
MyVar <- c("Communications_traffic", "Developed_area", "Forest_coniferous_highprod_organic_soil", 
           "Forest_coniferous_highprod_soil", "Forest_coniferous_impediment_bedrock", 
           "Forest_coniferous_impediment_organic_soil", "Forest_coniferous_impediment_shallow_soil", 
           "Forest_coniferous_impediment_soil", "Forest_coniferous_lowprod_boulder", 
           "Forest_coniferous_lowprod_organic_soil",  "Forest_coniferous_lowprod_shallow_soil", 
           "Forest_coniferous_lowprod_soil", "Forest_coniferous_mediumprod_organic_soil", 
           "Forest_coniferous_mediumprod_shallow_soil", "Forest_coniferous_mediumprod_soil", 
           "Forest_deciduous_highprod_organic_soil", "Forest_deciduous_highprod_shallow_soil", 
           "Forest_deciduous_highprod_soil", "Forest_deciduous_impediment_bedrock", 
           "Forest_deciduous_impediment_organic_soil", "Forest_deciduous_impediment_shallow_soil", 
           "Forest_deciduous_impediment_soil", "Forest_deciduous_lowprod_organic_soil", 
           "Forest_deciduous_lowprod_soil", "Forest_deciduous_mediumprod_organic_soil", 
           "Forest_deciduous_mediumprod_shallow_soil", "Forest_deciduous_mediumprod_soil", 
           "Forest_mix_highprod_shallow_soil", "Forest_mix_highprod_soil",
           "Forest_mix_impediment_organic_soil", "Forest_mix_impediment_shallow_soil", 
           "Forest_mix_impediment_soil", "Forest_mix_lowprod_organic_soil", 
           "Forest_mix_lowprod_shallow_soil", "Forest_mix_lowprod_soil",
           "Forest_mix_mediumprod_organic_soil", "Forest_mix_mediumprod_shallow_soil",
           "Forest_mix_mediumprod_soil", "Fully_cultivated_organic_soil", "Fully_cultivated_soil",                     
           "Hfgl_organic_soil", "Hfgl_shallow_soil", "Hfgl_soil", "Marsh_coniferous_highprod",                
           "Marsh_coniferous_impediment", "Marsh_coniferous_lowprod", "Marsh_coniferous_mediumprod",
           "Marsh_deciduous_impediment", "Marsh_deciduous_mediumprod", "Marsh_mix_impediment",                     
           "Marsh_mix_lowprod", "Marsh_open_impediment", "Ofg_highprod_soil", "Ofg_impediment_bedrock",                   
           "Ofg_impediment_boulder", "Ofg_impediment_shallow_soil", "Ofg_impediment_soil",
           "Ofg_mediumprod_soil", "Superficially_cultivated_soil", "Water",                                    
           "S.obs_2012", "S.chao1_2012", "S.obs_reds_2012", "S.chao1_reds_2012",                   
           "S.obs_blacks_2012", "S.chao1_blacks_2012", "Ntotal", "Nred", "Nblack")

Mydotplot(TrdRast_rel.2012@data[,MyVar[1:17]])
Mydotplot(TrdRast_rel.2012@data[,MyVar[18:34]])
Mydotplot(TrdRast_rel.2012@data[,MyVar[35:52]])
Mydotplot(TrdRast_rel.2012@data[,MyVar[53:69]])
# We do have quite a few outliers, some of which might be dealt with by combining categories

# Relationships (it's a little confusing to see them all, so I'll split it up a bit)
Myxyplot(TrdRast_rel.2012@data, MyVar[c(1:2, 39:43, 53:60)], "S.chao1_reds_2012", 
         MyYlab = "ESR of redlisted species")
Myxyplot(TrdRast_rel.2012@data, MyVar[3:38], "S.chao1_reds_2012", 
         MyYlab = "ESR of redlisted species")
Myxyplot(TrdRast_rel.2012@data, MyVar[c(44:52)], "S.chao1_reds_2012", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_rel.2012@data, MyVar[c(1:2, 39:43, 53:60)], "S.chao1_blacks_2012", 
         MyYlab = "ESR of alien species")
Myxyplot(TrdRast_rel.2012@data, MyVar[3:38], "S.chao1_blacks_2012", 
         MyYlab = "ESR of alien species")
Myxyplot(TrdRast_rel.2012@data, MyVar[c(44:52)], "S.chao1_blacks_2012", 
         MyYlab = "ESR of alien species")

# Zero inflation?
sum(TrdRast_rel.2012@data$S.chao1_reds_2012 == 0)  #Number of zeros
100 * sum(TrdRast_rel.2012@data$S.chao1_reds_2012 == 0) / nrow(TrdRast_rel.2012@data)  #% of zeros - not too bad!

sum(TrdRast_rel.2012@data$S.chao1_blacks_2012 == 0)  #Number of zeros
100 * sum(TrdRast_rel.2012@data$S.chao1_blacks_2012 == 0) / nrow(TrdRast_rel.2012@data)  #% of zeros - worse!



##--- 2.2 POOLING DATA ---####
##------------------------####
# As I will mainly be working with terrestrial (potentially also limnic) species, I will exclude ocean -
# this habitat type is not well covered either.

# Our habitats of interest are:
# Communications and traffic + Developed area  (both threatened and alien)
# Agriculture (both threatened and alien)
# Forest - potentially divided? (both threatened and alien)
# Freshwater - this (or distance to it?) might be important
# Home fields grazing land ('Innmarksbeite') (mainly threatened species, probably)
# Open firm ground - potentially not all subcategories (mainly alien species, probably)
# In the last category, we can take out 'boulder' nothing added by this one

# Creating a new dataframe with joined categories (first summing, then calculating relative area afterwards)
TrdRast_rel2.2012_raw <- TrdRast_analyses.2012
TrdRast_rel2.2012_raw@data$total_area <- rowSums(TrdRast_rel2.2012_raw@data[, 3:62])

# Sum the 'interesting' habitat types, and remove the uninteresting ones:
TrdRast_rel2.2012_raw@data$Developed <- rowSums(TrdRast_rel2.2012_raw@data[, c("Communications_traffic", "Developed_area")])
TrdRast_rel2.2012_raw@data$Agriculture <- rowSums(TrdRast_rel2.2012_raw@data[, c("Fully_cultivated_organic_soil",
                                                               "Fully_cultivated_soil",
                                                               "Superficially_cultivated_soil")])
TrdRast_rel2.2012_raw@data$Forest <- rowSums(TrdRast_rel2.2012_raw@data[, c("Forest_coniferous_highprod_organic_soil",
                                                                    "Forest_coniferous_highprod_soil",          
                                                                    "Forest_coniferous_impediment_bedrock",
                                                                    "Forest_coniferous_impediment_organic_soil",
                                                                    "Forest_coniferous_impediment_shallow_soil",
                                                                    "Forest_coniferous_impediment_soil",        
                                                                    "Forest_coniferous_lowprod_boulder",
                                                                    "Forest_coniferous_lowprod_organic_soil",   
                                                                    "Forest_coniferous_lowprod_shallow_soil",
                                                                    "Forest_coniferous_lowprod_soil",           
                                                                    "Forest_coniferous_mediumprod_organic_soil",
                                                                    "Forest_coniferous_mediumprod_shallow_soil",
                                                                    "Forest_coniferous_mediumprod_soil",
                                                                    "Forest_deciduous_highprod_organic_soil",   
                                                                    "Forest_deciduous_highprod_shallow_soil",
                                                                    "Forest_deciduous_highprod_soil",           
                                                                    "Forest_deciduous_impediment_bedrock", 
                                                                    "Forest_deciduous_impediment_organic_soil", 
                                                                    "Forest_deciduous_impediment_shallow_soil",
                                                                    "Forest_deciduous_impediment_soil",         
                                                                    "Forest_deciduous_lowprod_organic_soil",
                                                                    "Forest_deciduous_lowprod_soil",            
                                                                    "Forest_deciduous_mediumprod_organic_soil",
                                                                    "Forest_deciduous_mediumprod_shallow_soil", 
                                                                    "Forest_deciduous_mediumprod_soil",          
                                                                    "Forest_mix_highprod_shallow_soil",         
                                                                    "Forest_mix_highprod_soil",                  
                                                                    "Forest_mix_impediment_organic_soil",       
                                                                    "Forest_mix_impediment_shallow_soil",        
                                                                    "Forest_mix_impediment_soil",               
                                                                    "Forest_mix_lowprod_organic_soil",           
                                                                    "Forest_mix_lowprod_shallow_soil",          
                                                                    "Forest_mix_lowprod_soil",                  
                                                                    "Forest_mix_mediumprod_organic_soil",       
                                                                    "Forest_mix_mediumprod_shallow_soil",        
                                                                    "Forest_mix_mediumprod_soil")])
TrdRast_rel2.2012_raw@data$hfgl <- rowSums(TrdRast_rel2.2012_raw@data[, c("Hfgl_organic_soil",
                                                        "Hfgl_shallow_soil", "Hfgl_soil")])
TrdRast_rel2.2012_raw@data$ofg <- rowSums(TrdRast_rel2.2012_raw@data[, c("Ofg_highprod_soil",
                                                       "Ofg_impediment_bedrock", "Ofg_impediment_shallow_soil",
                                                       "Ofg_impediment_soil", "Ofg_mediumprod_soil")])
TrdRast_rel2.2012_raw <- TrdRast_rel2.2012_raw[, -c(3:61)]
TrdRast_rel2.2012 <- TrdRast_rel2.2012_raw[,c(1:2, 4:17, 3, 18:22)]    # Rearrange order of columns

# Recalculate all areas as percentage of total cell area - it might be useful to wait with this step:
for(i in 1:dim(TrdRast_rel2.2012@data)[1]) {
  for(j in 17:22) {
    TrdRast_rel2.2012@data[i,j] = (TrdRast_rel2.2012@data[i,j])/(TrdRast_rel2.2012@data[i,"total_area"])
  }
}

# Replace 'NA's with zeros:
TrdRast_rel2.2012@data[is.na(TrdRast_rel2.2012@data)] <- 0



