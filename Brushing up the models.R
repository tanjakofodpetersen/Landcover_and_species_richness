############################################
### ADDITIONS AFTER MEETING AUGUST 28TH ####
############################################

# This is the script containing the models which will be used for the future paper!

# 1.  Splitting of the predictor variables: espcially OFG seems to be causing some odd results - have a look at some maps (mapview?),
#     looking at the subgroups of OFG, to determine if some (e.g. 'artificial') should rather be grouped with 'Developed'
      # (Potentially) DONE!

# 2.  Redo the modelling with e.g. log(x+k)-transformation - this will help a little bit on the non-normality, and more importantly,
#     it will prevent your model-predictions from being negative
      # The results of this modelling approach were quite far from satisfactory.
      # The model predictions gave results which were completely out of bounds.
      # Try doing this analysis again based on the variable picked out from the clustering and ISA

# 3.  Splitting of the predictor variables: potentially speak with some of the people at the museum - they might have some ideas
#     regarding your groupings

# 4.  Predictor variables: a potential idea rather than using area or percentage of area is to have a look at PCA - can ALL the many
#     habitat types be defined by PCA, and do we see any patterns if we map the values of the PCA-axes? Can we make the models with
#     PCA axes rather than 'raw' habitat? This might be either very informative, or VERY confusing

# 5.  Predictor variables: instead of having ocean and freshwater split, make one "water" category, and instead make a cut-off at the
#     land-ocean boundary - we are not using the marine species anyways
      # DONE!

# 6.  Start a 'bullet-point'-manuscript. Equally important: figure out if the focus of this paper will be on methods or on biodiversity


##--- 5. PREDICTOR VARIABLES - OCEAN ---####
##--------------------------------------####

# We need to find a way to re-classify some of the polygons in the 2012-AR5, so that we can take out "Ocean" of the calculations
# First, let's have a look at whether or not these overlap more or less in the datasets:
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
plot(Trondheim,  main="")                                                    # Administrative border
plot(AR5_2012[AR5_2012$artype==80,], border="yellow", col="navy", add=TRUE)    # 'Water' in AR5_2012
# text(AR5_2012[AR5_2012$artype==80,], labels=AR5_2012@data[AR5_2012@data$artype==80, "id"], col="red", cex=0.5)   # Optional id of the polygons
# plot(AR5[AR5$artype==82,], border="red",
#     col=rgb(135, 206, 235, alpha=125, maxColorValue = 255), add=TRUE)       # 'Ocean' in AR5
# plot(AR5[AR5$artype==81,], border=NA,
#     col=rgb(0, 255, 255, alpha=125, maxColorValue = 255), add=TRUE)       # 'Freshwater' in AR5

# Aggregate the 2013-ocean polygons - this might make it easier to overlay:
ocean_large = aggregate(AR5[AR5$artype==82,], by = "artype")
plot(ocean_large, add=T,col=rgb(135, 206, 235, alpha=125, maxColorValue = 255), border=NA)

# Find the ID's of the AR5_2012-'Water' polygons, which fall within the the Ocean-polygon:
#ocean <- over(AR5[AR5$artype==82,], AR5_2012[AR5_2012$artype==80,])
ocean <- raster::intersect(ocean_large, AR5_2012[AR5_2012$artype==80,])
ocean$id <- as.character(ocean$id)

# Reclassify the polygons with ID's matching the "ocean":
AR5_2012@data$artype <- as.character(AR5_2012@data$artype)

AR5_2012@data[AR5_2012@data$id %in% ocean$id, "artype"] <- '82'
View(AR5_2012@data[AR5_2012@data$id %in% ocean$id, ] )

# Turn 'artype' back to a factor:
AR5_2012@data$artype <- as.factor(AR5_2012@data$artype)
levels(AR5_2012@data$artype)

# See if it helped:
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
plot(Trondheim,  main="")                                                                # Administrative border
plot(AR5_2012[AR5_2012$artype==80,], border="hotpink", col="cyan", lwd=0.5, add=TRUE)    # 'Water' in AR5_2012
plot(AR5_2012[AR5_2012$artype==82,], border="red", col="navy", add=TRUE)                 # 'Ocean' in AR5_2012

rm(ocean_large)





##--- 1. PREDICTOR VARIABLES - OPEN FIRM GROUND ---####
##-------------------------------------------------####
# The 'mapview' package cannot be installed on the server - we thus have to look at it differently
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
plot(Trondheim,  main="")                                                    # Administrative border
plot(AR5[AR5$artype==11,], border="lightpink", col="lightpink", add=TRUE)    # Developed area
plot(AR5[AR5$artype==12,], border="hotpink", col="hotpink", add=TRUE)        # Communications/traffic
 
plot(AR5[AR5$artype==50 & AR5$arskogbon==11 & AR5$argrunnf==41,],           # ofg, impediment, bedrock
     border="gray10", col="gray10", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==11 & AR5$argrunnf==42,],           # ofg, impediment, boulder
     border="gray30", col="gray30", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==11 & AR5$argrunnf==43,],           # ofg, impediment, shallow soil
     border="chocolate4", col="chocolate4", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==11 & AR5$argrunnf==44,],           # ofg, impediment, soil
     border="chocolate3", col="chocolate3", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==11 & AR5$argrunnf==46,],           # ofg, impediment, artificial
     border="blue", col="blue", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==13 & AR5$argrunnf==43,],           # ofg, medium, shallow soil
     border="olivedrab1", col="olivedrab1", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==13 & AR5$argrunnf==44,],               # ofg, medium, soil
     border="olivedrab3", col="olivedrab3", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==14 & AR5$argrunnf==43,],               # ofg, High, shallow soil
     border="olivedrab4", col="olivedrab4", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==14 & AR5$argrunnf==44,],               # ofg, High, soil
     border="greenyellow", col="greenyellow", add=TRUE)
plot(AR5[AR5$artype==50 & AR5$arskogbon==15 & AR5$argrunnf==44,],               # ofg, very high, soil
     border="green", col="green", add=TRUE)

# Make this map in desktop-RStudio to use 'mapview' and asses the polygons more properly
# A problem here is that the 'ofg_imp_art' was not classified in the older AR5

# Have a look at the potential relationships as well:
MyVar <- c("Communications_traffic", "Developed_area", "Forest_coniferous_highprod_organic_soil",
           "Forest_coniferous_highprod_shallow_soil", "Forest_coniferous_highprod_soil",
           "Forest_coniferous_impediment_bedrock", "Forest_coniferous_impediment_organic_soil",
           "Forest_coniferous_impediment_shallow_soil", "Forest_coniferous_impediment_soil",
           "Forest_coniferous_lowprod_boulder", "Forest_coniferous_lowprod_organic_soil",
           "Forest_coniferous_lowprod_shallow_soil", "Forest_coniferous_lowprod_soil",
           "Forest_coniferous_mediumprod_organic_soil", "Forest_coniferous_mediumprod_shallow_soil",
           "Forest_coniferous_mediumprod_soil", "Forest_coniferous_veryhighprod_soil",
           "Forest_deciduous_highprod_organic_soil", "Forest_deciduous_highprod_soil",
           "Forest_deciduous_impediment_bedrock", "Forest_deciduous_impediment_organic_soil",
           "Forest_deciduous_impediment_shallow_soil", "Forest_deciduous_impediment_soil",
           "Forest_deciduous_lowprod_organic_soil", "Forest_deciduous_lowprod_soil",
           "Forest_deciduous_mediumprod_organic_soil", "Forest_deciduous_mediumprod_shallow_soil",
           "Forest_deciduous_mediumprod_soil", "Forest_mix_highprod_soil",
           "Forest_mix_impediment_organic_soil", "Forest_mix_impediment_shallow_soil",
           "Forest_mix_impediment_soil", "Forest_mix_lowprod_organic_soil",
           "Forest_mix_lowprod_shallow_soil", "Forest_mix_lowprod_soil",
           "Forest_mix_mediumprod_organic_soil", "Forest_mix_mediumprod_shallow_soil",
           "Forest_mix_mediumprod_soil", "Freshwater", "Fully_cultivated_organic_soil",
           "Fully_cultivated_soil", "Hfgl_deciduous_soil", "Hfgl_organic_soil", "Hfgl_shallow_soil",
           "Hfgl_soil", "Marsh_coniferous_highprod", "Marsh_coniferous_impediment",
           "Marsh_coniferous_lowprod", "Marsh_coniferous_mediumprod", "Marsh_deciduous_impediment",
           "Marsh_deciduous_mediumprod", "Marsh_mix_impediment", "Marsh_mix_lowprod", "Marsh_open_impediment",
           "Ocean", "Ofg_highprod_soil" , "Ofg_impediment_artificial", "Ofg_impediment_bedrock",
           "Ofg_impediment_boulder", "Ofg_impediment_shallow_soil", "Ofg_impediment_soil",
           "Ofg_mediumprod_soil", "Superficially_cultivated_organic_soil",
           "Superficially_cultivated_shallow_soil", "Superficially_cultivated_soil")

### Relationships
Myxyplot(TrdRast_AR5@data, MyVar[1:20], "S.chao1_reds_2013", 
         MyYlab = "ESR of redlisted species")
Myxyplot(TrdRast_AR5@data, MyVar[21:40], "S.chao1_reds_2013", 
         MyYlab = "ESR of redlisted species")
Myxyplot(TrdRast_AR5@data, MyVar[41:65], "S.chao1_reds_2013", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_AR5@data, MyVar, "S.chao1_blacks_2013", 
         MyYlab = "ESR of alien species")


# ofg_bedrock only found at mountain tops, ofg_boulder mainly found in mountains and near coastlines --> combine
# ofg_imp_ss mainly found in the forest, coastlines and near mountaineous areas - should potentially be combine with the category above
# ofg_imp_s heavily associated with buildings, roads etc. --> this should probably be separated from the ones above
# ofg_art is quarries, sport arenas as alike - should either be combinded with 'Developed' or with ofg_imp_s.
                # Also, these seem to have been the same category as ofg_imp_s in the 2012-maps
# ofg_medium_s is difficult - not as "urban" as the two above, but more urban than the top ones
# ofg_high_s can be described as the category above.

# I suggest splitting the ofg-category as follows:
# ofg_imp_bedrock + ofg_imp_boulder + ofg_imp_ss --> ofg_imp
# ofg_imp_s + ofg_art + ofg_medium_s + ofg_high_s --> ofg_urban

# Before moving on, calculate the total area of each grid cell in the dataframe:
TrdRast_AR5@data$total_area <- rowSums(TrdRast_AR5@data[, c(3:68)])

plot(TrdRast_AR5, col=TrdRast_AR5@data$total_area, border="gray")  # Something is off
plot(Trondheim, add=TRUE)

##--- 1.1 REMAKE THE DATAFRAMES ---####
##---     (almost) FROM SCRATCH ---####
##--- 1.1.1 Maps 2013-2018      ---####
##---------------------------------####
# From the previous scripts: cells with "enough" data:
TrdRast_model <- TrdRast_AR5[TrdRast_AR5@data$CoV_2013<=0.25 &      # Only cells with low CoV
                                  TrdRast_AR5@data$Ntotal>=10 &     # Only cells with >10 records
                                  !is.na(TrdRast_AR5@data$Communications_traffic) &    # Only cells within Trondheim
                                  !is.na(TrdRast_AR5@data$CoV_2013), ]        # Only cells with a valid CoV

TrdRast_model$total_area2 <- rowSums(TrdRast_model@data[,c(3:68)])
TrdRast_model@data$total_area == TrdRast_model@data$total_area2   

# Creating a new dataframe with joined categories (first summing, then calculating relative area afterwards)
# Sum the 'interesting' habitat types, and remove the uninteresting ones:
TrdRast_model@data$Developed <- rowSums(TrdRast_model@data[, c(3:4)])
TrdRast_model@data$Agriculture <- rowSums(TrdRast_model@data[, c(42:43,65:67)])
TrdRast_model@data$Forest <- rowSums(TrdRast_model@data[, c(5:40)])
TrdRast_model@data$Marsh <- rowSums(TrdRast_model@data[, c(48:56)])
TrdRast_model@data$hfgl <- rowSums(TrdRast_model@data[, c(44:47)])
TrdRast_model@data$ofg_imp <- rowSums(TrdRast_model@data[, c(60:62)])
TrdRast_model@data$ofg_urban <- rowSums(TrdRast_model@data[, c(58:59,63:64)])

# Freshwater, ocean and NA are keps as they are (column 41, 57 and 68)

# Test that the total_area is still the same:
TrdRast_model@data$total_area3 <- rowSums(TrdRast_model@data[, c(41, 57, 68, 84:90)])
TrdRast_model@data$total_area == TrdRast_model@data$total_area3   # SOMETHING IS NOT RIGHT!
View(as.data.frame(matrix(data=c(TrdRast_model@data$total_area, TrdRast_model@data$total_area2, TrdRast_model@data$total_area3), nrow=length(TrdRast_model@data$total_area))))
# All the columns are the same, despite what is printed
TrdRast_model@data$total_area2 <- NULL
TrdRast_model@data$total_area3 <- NULL

TrdRast_model <- TrdRast_model[, -c(3:40, 42:56, 58:67)]
# Reorder the columns:
TrdRast_model <- TrdRast_model[, c(1:2, 6:26, 3:5)]


# Recalculate all areas as percentage of total cell area:
for(i in 1:dim(TrdRast_model@data)[1]) {
  for(j in 17:26) {
    TrdRast_model@data[i,j] = (TrdRast_model@data[i,j])/(TrdRast_model@data[i,"total_area"])
  }
}

# Replace 'NA's with zeros
TrdRast_model@data[is.na(TrdRast_model@data)] <- 0



##--- 1.1.2 Maps 2008-2012      ---####
##---------------------------------####
# This one has to me remade more or less from scratch (I think so, at least)
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

cover_in_grid_AR5.2012[cover_in_grid_AR5.2012$artype==80, "Cvr"] <- "Freshwater"

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

names(TrdRast_AR5.2012)

### OBS! It is important to note here that we no longer have the data on GBIF-records etc. in the dataframe - these need
### to be added, should we need them again!

TrdRast_2012 <- TrdRast_AR5.2012
TrdRast_2012@data$total_area <- rowSums(TrdRast_2012@data[, 3:63])

# Sum the 'interesting' habitat types, and remove the uninteresting ones:
TrdRast_2012@data$Developed.2012 <- rowSums(TrdRast_2012@data[, c(3:4)])   
TrdRast_2012@data$Agriculture.2012 <- rowSums(TrdRast_2012@data[, c(42:43,63)])
TrdRast_2012@data$Forest.2012 <- rowSums(TrdRast_2012@data[, c(5:40)])
TrdRast_2012@data$Marsh.2012 <- rowSums(TrdRast_2012@data[, c(47:55)])
TrdRast_2012@data$hfgl.2012 <- rowSums(TrdRast_2012@data[, c(44:46)])
TrdRast_2012@data$ofg_imp.2012 <- rowSums(TrdRast_2012@data[, c(58:60)])
TrdRast_2012@data$ofg_urban.2012 <- rowSums(TrdRast_2012@data[, c(57,61:62)])
# Freshwater and Ocean are used as they are (column 41 and 56)

TrdRast_2012 <- TrdRast_2012[, -c(3:40, 42:55, 57:63)]
TrdRast_2012 <- TrdRast_2012[,c(1:2, 5:12, 3:4)]    # Rearrange order of columns

# Recalculate all areas as percentage of total cell area - it might be useful to wait with this step:
for(i in 1:dim(TrdRast_2012@data)[1]) {
  for(j in 4:12) {
    TrdRast_2012@data[i,j] = (TrdRast_2012@data[i,j])/(TrdRast_2012@data[i,"total_area"])
  }
}

# Replace 'NA's with zeros and clean up a column name:
TrdRast_2012@data[is.na(TrdRast_2012@data)] <- 0
names(TrdRast_2012@data)[names(TrdRast_2012@data) == 'Freshwater'] <- 'Freshwater.2012'
names(TrdRast_2012@data)[names(TrdRast_2012@data) == 'Ocean'] <- 'Ocean.2012'

# Add these calculations to the dataframe with the pruned cells from 2013:
TrdRast_model@data <- merge(TrdRast_model@data, TrdRast_2012@data[,c(1,4:12)], by="Pixelnr", all.x=TRUE)

# Calculate the change in the different landcover types in each cell:
TrdRast_model$delta_Developed <- TrdRast_model@data$Developed - TrdRast_model@data$Developed.2012
TrdRast_model$delta_Agriculture <- TrdRast_model@data$Agriculture - TrdRast_model@data$Agriculture.2012
TrdRast_model$delta_Forest <- TrdRast_model@data$Forest - TrdRast_model@data$Forest.2012
TrdRast_model$delta_hfgl <- TrdRast_model@data$hfgl - TrdRast_model@data$hfgl.2012
TrdRast_model$delta_ofg_imp <- TrdRast_model@data$ofg_imp - TrdRast_model@data$ofg_imp.2012
TrdRast_model$delta_ofg_urban <- TrdRast_model@data$ofg_urban - TrdRast_model@data$ofg_urban.2012
TrdRast_model$delta_freshwater <- TrdRast_model@data$Freshwater - TrdRast_model@data$Freshwater.2012
TrdRast_model$delta_ocean <- TrdRast_model@data$Ocean - TrdRast_model@data$Ocean.2012
TrdRast_model$delta_Marsh <- TrdRast_model@data$Marsh - TrdRast_model@data$Marsh.2012


##--- 2. MODELLING, POTENTIALLY WITH LOG-TRANSFORMATION ---####
##--- 2.1 DATA EXPLORATION                              ---####
##---------------------------------------------------------
source("HighstatLibV10.R")

### Outliers:
MyVar <- c("S.chao1_reds_2013", "S.chao1_blacks_2013", "Developed", "Agriculture", "Forest", "hfgl",
           "ofg_imp", "ofg_urban", "Freshwater", "Ocean",
           "Freshwater.2012", "Ocean.2012", "Developed.2012", "Agriculture.2012", "Forest.2012",
           "hfgl.2012", "ofg_imp.2012", "ofg_urban.2012",
           "delta_Developed", "delta_Agriculture", "delta_Forest", "delta_hfgl",
           "delta_ofg_imp", "delta_ofg_urban", "delta_freshwater", "delta_ocean" )

Mydotplot(TrdRast_model@data[,MyVar])
# We do have a few outliers, which we might have to deal with.
# In this case, we have one grid cell with a massive alien ESR - completely out of bounds compared to
# all other estimates and values. It is probably way to high, it might be
# messing with my analysis.
# Have a look at where the culprit is:
AR5map(Trondheim, AR5, "2013-2018") +
  plot(TrdRast_model[TrdRast_model@data$S.chao1_blacks_2013>100,], add=T, col="red")

# That is Ringve Botanical Garden - hence the high number of alien species!
# Thus, I'll remove that cell from further analysis:
TrdRast_model <- TrdRast_model[!TrdRast_model@data$S.chao1_blacks_2013>100,]

# Look again:
Mydotplot(TrdRast_model@data[,MyVar])
AR5map(Trondheim, AR5, "2013-2018") +
  plot(TrdRast_model[TrdRast_model@data$S.chao1_blacks_2013>60,], add=T, col="red")
# We might still have an outlier, but we'll try to include that anyways


# Make boxplots to assess the spread of the different variables:
par(mfrow=c(4,5))
par(mar=c(0.5,2,3,0.5))
for(i in c(7,10,17:25,35:42)){
  boxplot(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], main=colnames(TrdRast_model@data[i]),
          cex.main=0.7, cex.axis=0.6)
}

# Once again, we can see quite the spread in the variables, and a lot of outliers - but probably not as bad as before

### Colinearity
pairs(TrdRast_model@data[, MyVar[c(1:10,19:26)]], 
      lower.panel = panel.cor)
# The only colinear variables will not be modelled simultaneously - we have to be careful not to use the land cover
# (area) from both time periods, as these are highly colinear

### Relationships
Myxyplot(TrdRast_model@data, MyVar[c(3:10,19:26)], "S.chao1_reds_2013", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_model@data, MyVar[c(3:10,19:26)], "S.chao1_blacks_2013", 
         MyYlab = "ESR of alien species")
# We cannot see any clear relationships from this, unfortunately

### Zero inflation?
sum(TrdRast_model@data$S.chao1_reds_2013 == 0)  #Number of zeros
100 * sum(TrdRast_model@data$S.chao1_reds_2013 == 0) / nrow(TrdRast_model@data)  #% of zeros - not too bad!

sum(TrdRast_model@data$S.chao1_blacks_2013 == 0)  #Number of zeros
100 * sum(TrdRast_model@data$S.chao1_blacks_2013 == 0) / nrow(TrdRast_model@data)  #% of zeros - worse!
# We might have some zeroinflation here

### Normal distribution of data
# This has to be done for each variable individually - I will try to do it in loops
# Density plots:
par(mfrow=c(4,5))
par(mar=c(2,2,3,0.5))
for(i in c(7,10,17:25,35:42)){
  plot(density.default(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i]), main=colnames(TrdRast_model@data[i]),
       cex.main=0.75, cex.axis=0.6)
}       # It is highly debatable, if these show normal distribution

# Histogram
par(mfrow=c(4,5))
par(mar=c(2,2,3,0.5))
for(i in c(7,10,17:25,35:42)){
  hist(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], main=colnames(TrdRast_model@data[i]),
       cex.main=0.75, cex.axis=0.6)
}       # It is highly debatable, if these show normal distribution. Our main problem is that
# the values for any variable cannot go below zero, but as we have zero-values, the gamma distribution is useless

# QQplots
par(mfrow=c(4,5))
for(i in c(7,10,17:25,35:42)){
  qqnorm(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], main=colnames(TrdRast_model@data[i]),
         cex.main=0.75, cex.axis=0.6, cex.lab=0.6) ; qqline(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], col="red")
}       # It is highly debatable, if these show normal distribution

# NONE of the variables have normal distribution! This is likely something we have to deal with





##--- 2.2 DATA TRANSFORMATION ---####
##-------------------------------####

# As we have multiple outliers and a lack of normal distribution, it is worth looking into
# transformation of the variables, and then some subsequent data exploration of the transformed variables.
# One of the issues might also be the large number of zeros in some of the variables, especially the response.
# Another reason for this is as we're modelling something as if it has a Gaussian distribution, we're allowing
# it to be negative, which our response cannot be - log-transformation is a way to deal with this.
# First, I'll have a look at whether to use log or log10. The constant added will be 0.001


##--- 2.2.1 Response variables - log(x+k) ---####
##-------------------------------------------####
# Transformation of the response variables (here we have to add a constant to make the calculations, as log(0) is meaningsless):
TrdRast_model$log_chao.reds <- NA
TrdRast_model$log_chao.blacks <- NA
for(i in 1:NROW(TrdRast_model@data)){
  TrdRast_model@data[i,"log_chao.reds"] <- log(TrdRast_model@data[i,"S.chao1_reds_2013"] + 0.001)
  TrdRast_model@data[i,"log_chao.blacks"] <- log(TrdRast_model@data[i,"S.chao1_blacks_2013"] + 0.001)
  }

MyVar <- c("log_chao.reds", "log_chao.blacks", "Developed", "Agriculture", "Forest",
           "hfgl", "ofg_imp", "ofg_urban", "Freshwater", "Ocean",
           "delta_Developed", "delta_Agriculture", "delta_Forest", "delta_hfgl",
           "delta_ofg_imp", "delta_ofg_urban", "delta_freshwater", "delta_ocean" )

Mydotplot(TrdRast_model@data[,MyVar])
# We might still have some issues here - it doesn't look quite good

# Make boxplots to assess the spread of the different variables:
par(mfrow=c(1,2))
par(mar=c(0.5,2,3,0.5))
for(i in c(43:44)){
  boxplot(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], main=colnames(TrdRast_model@data[i]),
          cex.main=0.7, cex.axis=0.6)
}
# Here the transformation have made a difference

### Relationships
Myxyplot(TrdRast_model@data, MyVar[3:18], "log_chao.reds", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_model@data, MyVar[3:18], "log_chao.blacks", 
         MyYlab = "ESR of alien species")
# We cannot see any clear relationships from this, unfortunately


### Normal distribution of data
# Density plots:
par(mfrow=c(1,2))
par(mar=c(2,2,3,0.5))
for(i in c(43:44)){
  plot(density.default(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i]), main=colnames(TrdRast_model@data[i]),
       cex.main=0.75, cex.axis=0.6)
}       # Clearly not

# QQplots
par(mfrow=c(1,2))
for(i in c(43:44)){
  qqnorm(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], main=colnames(TrdRast_model@data[i]),
         cex.main=0.75, cex.axis=0.6, cex.lab=0.6) ; qqline(TrdRast_model@data[!is.na(TrdRast_model@data[,i]),i], col="red")
}       # Clearly not


# For later mapping, we'll also add the log-transformed ESR of al species to the dataframe:
TrdRast_model$log_chao.all <- NA
for(i in 1:NROW(TrdRast_model@data)){
  TrdRast_model@data[i,47] <- log(TrdRast_model@data[i,4] + 0.001)
}




##--- 2.3. PRELIMINARY MODELLING (NON-SPATIAL) ---####
##--- 2.3.1 Model 1 - threatened species       ---####
##------------------------------------------------####

M1_ns <- glm(log_chao.reds ~  Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl
             + delta_ofg_imp + delta_ofg_urban + delta_freshwater ,
             family = "gaussian",
             data = TrdRast_model@data)

summary(M1_ns)


### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(3051.6 - 2414.7) / 3051.6    # Relatively large, actually

### Model validation 2: Is everything significant?
drop1(M1_ns, test = "Chi")
step(M1_ns) #Backwards selection using AIC

# According to 'step()', we can remove some of the variables - the optimal model being:
# log_chao.reds ~ Developed + Agriculture + Forest + hfgl + ofg_imp + ofg_urban
M1.2_ns <- glm(log_chao.reds ~ Developed + Agriculture + Forest + hfgl + ofg_imp + ofg_urban,
               family = "gaussian", 
               data = TrdRast_model@data)
summary(M1.2_ns)
# The coefficient values seem a little odd - the relationships are in many cases opposite of what I expected

# Plot residuals vs fitted values (M1)
F1_ns <- fitted(M1_ns)
E1_ns <- resid(M1_ns, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_ns, 
     y = E1_ns,
     xlab = "Fitted values - M1",
     ylab = "Pearson residuals - M1",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot residuals vs fitted values (M1.2)
F1.2_ns <- fitted(M1.2_ns)
E1.2_ns <- resid(M1.2_ns, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1.2_ns, 
     y = E1.2_ns,
     xlab = "Fitted values - M1.2",
     ylab = "Pearson residuals - M1.2",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_model@data$E1_ns <- E1_ns
Myxyplot(TrdRast_model@data, MyVar, "E1_ns")
TrdRast_model@data$E1_ns <- NULL

TrdRast_model@data$E1.2_ns <- E1.2_ns
Myxyplot(TrdRast_model@data, MyVar, "E1.2_ns")
TrdRast_model@data$E1.2_ns <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E1_ns)
hist(E1.2_ns)



##--- 2.3.2 Model 2 - alien species ---####
##-------------------------------------####

M2_ns <- glm(log_chao.blacks ~  Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl
             + delta_ofg_imp + delta_ofg_urban + delta_freshwater ,
             family = "gaussian",
             data = TrdRast_model@data)

summary(M2_ns)

### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(5076.4 - 3725.0) / 5076.4    # Relatively large, actually?

### Model validation 2: Is everything significant?
drop1(M2_ns, test = "Chi")
step(M2_ns) #Backwards selection using AIC

# According to 'step()', we can remove quite a few of the variables - the optimal model being:
# log_chao.blacks ~ Developed + Marsh + ofg_imp + ofg_urban + delta_Forest
M2.2_ns <- glm(log_chao.blacks ~ Developed + Marsh + ofg_imp + ofg_urban + delta_Forest,
               family = "gaussian", 
               data = TrdRast_model@data)
summary(M2.2_ns)


# Plot residuals vs fitted values
F2_ns <- fitted(M2_ns)
E2_ns <- resid(M2_ns, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2_ns, 
     y = E2_ns,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)       

F2.2_ns <- fitted(M2.2_ns)
E2.2_ns <- resid(M2.2_ns, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2.2_ns, 
     y = E2.2_ns,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)       

# Plot the residuals vs each covariate     
TrdRast_model@data$E2_ns <- E2_ns
Myxyplot(TrdRast_model@data, MyVar, "E2_ns")
TrdRast_model@data$E2_ns <- NULL

TrdRast_model@data$E2.2_ns <- E2.2_ns
Myxyplot(TrdRast_model@data, MyVar, "E2.2_ns")
TrdRast_model@data$E2.2_ns <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E2_ns)
hist(E2.2_ns)




##--- 2.4 SPATIAL AUTOCORRELATION- threatened species ---####
##--- 2.4.1 Testing for SAC - Chao1_reds              ---####
##-------------------------------------------------------####
library(spdep)
library(ncf)

# We have already subsetted the dataset, but we need the coordinates.
# OBS! We have to only use the cells for which have data on all variables - otherwise we get errors:
xy_ns <- coordinates(TrdRast_model)

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model$log_chao.reds) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model, col=(TrdRast_model$log_chao.reds + 6.907755))   # The colours cannot be negative - add the numerical value of the lowest
par(mar=c(5,1,5,2.5))
image(y=(-7):5,z=t((-7):5), col=rev(col.heat), axes=FALSE, main="log(threatened\n+0.01)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog1_ns <- correlog(xy_ns[,1], xy_ns[,2], residuals(M1_ns), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog1_ns$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_ns[,1], xy_ns[,2], col=c("blue", "red")[sign(resid(M1_ns))/2+1.5], pch=19,
     cex=abs(resid(M1_ns))/max(resid(M1_ns))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
M1_ns.nb <- dnearneigh(as.matrix(xy_ns[,1:2]), 0, 1500) # Find the neighbors - give lower and upper distance class here
# OBS! The classes are in euclidian distance (m), thus we need a reasonable distance to define
# a neighbouring grid cell. Here, I have chosen to use 1.5 km 
# to make it reasonable! Otherwise, use the following list:
# w_pp <- poly2nb(TrdRast_pp, row.names=TrdRast_pp$Pixelnr)     # Find the neighbors
# ww_pp <-  nb2listw(w_pp, style='B', zero.policy = TRUE)       # Make it a "listw" spatial object
M1_ns.listw <- nb2listw(M1_ns.nb, zero.policy = T)   
# Turns neighbourhood object into a weighted list
# this next step takes often several minutes to run:
GlobMT1_ns <- moran.test(residuals(M1_ns), listw=M1_ns.listw, zero.policy = T)
GlobMT1_ns

# We seemingly have SAC in these model residuals.
# Lets have a look at whether there is SAC in the data itself rather than only the residuals:
moran(TrdRast_model$log_chao.reds, M1_ns.listw, n=length(M1_ns.listw$neighbours),
      S0=Szero(M1_ns.listw), zero.policy = TRUE)    # Calculate Moran's I

# Test for significance:
moran.test(TrdRast_model$log_chao.reds, M1_ns.listw, randomisation=FALSE,
           alternative = "two.sided", zero.policy = TRUE)     # Using linear regression based logic and assumptions

MC_ns <- moran.mc(TrdRast_model$log_chao.reds, M1_ns.listw,
                  zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_ns, main=NULL)     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_ns$statistic, lty=2, col="red")

# Make a correlogram
sp.corr_ns <- sp.correlogram(M1_ns.nb, TrdRast_model$log_chao.reds, order=8, method="I", zero.policy = TRUE)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(sp.corr_ns)

# So, we have autocorrelation in the data itself and in the model residuals
# Test the reduced model as well:
GlobMT1.2_ns <- moran.test(residuals(M1.2_ns), listw=M1_ns.listw, zero.policy = T)
GlobMT1.2_ns

# SAC in both models


##--- 2.4.2 Testing for SAC - Chao1_blacks             ---####
##--------------------------------------------------------####

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model$log_chao.blacks) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model, col=(TrdRast_model$log_chao.blacks + 6.907755)) # The colours cannot be negative - add the numerical value of the lowest
par(mar=c(5,1,5,2.5))
image(y=-7:6,z=t(-7:6), col=rev(col.heat), axes=FALSE, main="log(alien\n+00.1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog2_ns <- correlog(xy_ns[,1], xy_ns[,2], residuals(M2_ns), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2_ns$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_ns[,1], xy_ns[,2], col=c("blue", "red")[sign(resid(M2_ns))/2+1.5], pch=19,
     cex=abs(resid(M2_ns))/max(resid(M2_ns))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT2_ns <- moran.test(residuals(M2_ns), listw=M1_ns.listw, zero.policy = T)
GlobMT2_ns

# We have SAC, as long as we use a reasonable neighbouring distance

### For the reduced model:
# Make a correlogram:
correlog2.2_ns <- correlog(xy_ns[,1], xy_ns[,2], residuals(M2.2_ns), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2.2_ns$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_ns[,1], xy_ns[,2], col=c("blue", "red")[sign(resid(M2.2_ns))/2+1.5], pch=19,
     cex=abs(resid(M2.2_ns))/max(resid(M2.2_ns))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT2.2_ns <- moran.test(residuals(M2.2_ns), listw=M1_ns.listw, zero.policy = T)
GlobMT2.2_ns


##--- 2.5 DEALING WITH SAC ---####
##----------------------------####

# Something is off with the resulting models. The numbers obtained are giving predictions which are way out
# of reasonable values. Come back to this, and potentially have fresh look at the pooling of habitat
# categories, or which of the habitats to in- or exclude

##--- 2.5.1 Chao1_reds     ---####
##----------------------------####

# To deal with the spatial autocorrelation, we can use a GLS with an added correlation structure - we thus also
# need to figure out what kind of correlation structure is apppriate.
# The steps here are from the Zuur-book and the Dorman-paper:
require(nlme)
summary(gls.exp_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                            delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                            delta_ofg_urban + delta_freshwater ,
                          data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.gauss_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model@data, correlation=corGaus(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.spher_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model@data, correlation=corSpher(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.lin_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                            delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                            delta_ofg_urban + delta_freshwater ,
                          data=TrdRast_model@data, correlation=corLin(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.Ratio_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model@data, correlation=corRatio(form=~~xy_ns[,1]+xy_ns[,2])))

AIC(M1_ns, gls.exp_ns, gls.gauss_ns, gls.spher_ns, gls.lin_ns, gls.Ratio_ns)
# All spatial correlations makes a better model.
# Since the deltaAIC between the two best models are less than 2, we go for the simplest one: gls.exp
# Remove the others (for space):
rm(gls.gauss_ns)
rm(gls.spher_ns)
rm(gls.lin_ns)
rm(gls.Ratio_ns)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp_ML_ns <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                               delta_ofg_urban + delta_freshwater ,
                             data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2]), method = "ML"))

drop1(gls.exp_ML_ns, test = "Chi")
library(MASS)
stepAIC(gls.exp_ML_ns)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the optimal model is:
# gls(log_chao.reds ~ Developed + Agriculture + hfgl + ofg_imp + ofg_urban)
# This is similar to the optimal model for the non-spatial approach - the coefficients seem a little odd though

gls.exp_threat <- gls(log_chao.reds ~ Developed + Agriculture + hfgl + ofg_imp + ofg_urban,
                       data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2]))
summary(gls.exp_threat)


##--- 2.5.2 Chao1_blacks      ---####
##-------------------------------####

# To deal with the spatial autocorrelation, we can use a GLS with an added correlation structure - we thus also
# need to figure out what kind of correlation structure is apppriate.
# The steps here are from the Zuur-book and the Dorman-paper:
summary(gls.exp.b_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.gauss.b_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model@data, correlation=corGaus(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.spher.b_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model@data, correlation=corSpher(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.lin.b_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model@data, correlation=corLin(form=~xy_ns[,1]+xy_ns[,2])))
summary(gls.Ratio.b_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model@data, correlation=corRatio(form=~~xy_ns[,1]+xy_ns[,2])))

AIC(M2_ns, gls.exp.b_ns, gls.gauss.b_ns, gls.spher.b_ns, gls.lin.b_ns, gls.Ratio.b_ns)
# All spatial correlations makes a better model.
# To evaluate which correlation structure is the best one, we can look at the AIC.
# Since the deltaAIC between the two best models are less than 2, we go for the simplest one: gls.exp
rm(gls.gauss.b_ns, gls.spher.b_ns, gls.lin.b_ns, gls.Ratio.b_ns)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp.b_ML_ns <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                 delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                 delta_ofg_urban + delta_freshwater ,
                               data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2]), method = "ML"))

drop1(gls.exp.b_ML_ns, test = "Chi")
stepAIC(gls.exp.b_ML_ns)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the optimal model is:
# gls(log_chao.blacks ~ Developed + Marsh + ofg_imp + ofg_urban + delta_Forest + delta_Marsh)
# This is somewhat similar to the optimal model for the non-spatial approach!

gls.exp_alien <- gls(log_chao.blacks ~ Developed + Marsh + ofg_imp + ofg_urban + delta_Forest + delta_Marsh,
                         data=TrdRast_model@data, correlation=corExp(form=~xy_ns[,1]+xy_ns[,2]))
summary(gls.exp_alien)



##--- 2.6 MAPS WITH THE DATA ---####
##------------------------------####
par(mar=c(0.5, 0.5, 5, 0.5))
# Map showing the investigated cells (the function is defined in the "Functions"-script:
DivMap(AR5, Trondheim, TrdRast_AR5, "")
plot(TrdRast_AR5[TrdRast_AR5@data$Ntotal>0 &
                   !is.na(TrdRast_AR5@data$Ntotal),],
     col=rgb(169, 169, 169, alpha = 150, maxColorValue = 255), border=NA, add=T)     # Cells with records
plot(TrdRast_model,
     col=rgb(255, 0, 0, alpha = 100, maxColorValue = 255), border=NA, add=T)         # Cells used in the analyses

library(RColorBrewer)
library(gplots)

##--- 2.6.1 Maps showing the number of records in each grid cell ---####
##------------------------------------------------------------------####
# Get the numbers to base the colour on:
my_colours_pre.rec_all <- c(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>0 &
                                               !is.na(TrdRast_AR5@data$Ntotal), "Ntotal"])
my_colours_pre.rec_red <- c(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>0 &
                                               !is.na(TrdRast_AR5@data$Ntotal), "Nred"])
my_colours_pre.rec_black <- c(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>0 &
                                                 !is.na(TrdRast_AR5@data$Ntotal), "Nblack"])

# Make the vectors with colour names:
my_colours_rec_all <- rep(0, length(my_colours_pre.rec_all))
for(i in 1:length(my_colours_pre.rec_all)){
  my_colours_rec_all[i] <- ifelse(my_colours_pre.rec_all[i]<=10, paste("#7F00FFFF"),
                                  ifelse(my_colours_pre.rec_all[i]>10 & my_colours_pre.rec_all[i]<=20, paste("#001AFFFF"),
                                         ifelse(my_colours_pre.rec_all[i]>20 & my_colours_pre.rec_all[i]<=50, paste("#00B3FFFF"),
                                                ifelse(my_colours_pre.rec_all[i]>50 & my_colours_pre.rec_all[i]<=100, paste("#00FFFFFF"),
                                                       ifelse(my_colours_pre.rec_all[i]>100 & my_colours_pre.rec_all[i]<=200, paste("#00FF19FF"),
                                                              ifelse(my_colours_pre.rec_all[i]>200 & my_colours_pre.rec_all[i]<=400, paste("#80FF00FF"),
                                                                     ifelse(my_colours_pre.rec_all[i]>400 & my_colours_pre.rec_all[i]<=1000, paste("#FFE500FF"),
                                                                            ifelse(my_colours_pre.rec_all[i]>1000 & my_colours_pre.rec_all[i]<=5000, paste("#FF9900FF"),
                                                                                   ifelse(my_colours_pre.rec_all[i]>5000 & my_colours_pre.rec_all[i]<=10000, paste("#FF4D00FF"),
                                                                                          ifelse(my_colours_pre.rec_all[i]>10000 & my_colours_pre.rec_all[i]<30000, paste("#FF0000FF"), 'NA'))))))))))
}

my_colours_rec_red <- rep(0, length(my_colours_pre.rec_red))
for(i in 1:length(my_colours_pre.rec_red)){
  my_colours_rec_red[i] <- ifelse(my_colours_pre.rec_red[i]>0 & my_colours_pre.rec_red[i]<=10, paste("#7F00FFFF"),
                                  ifelse(my_colours_pre.rec_red[i]>10 & my_colours_pre.rec_red[i]<=25, paste("#001AFFFF"),
                                         ifelse(my_colours_pre.rec_red[i]>25 & my_colours_pre.rec_red[i]<=50, paste("#00B3FFFF"),
                                                ifelse(my_colours_pre.rec_red[i]>50 & my_colours_pre.rec_red[i]<=100, paste("#00FFFFFF"),
                                                       ifelse(my_colours_pre.rec_red[i]>100 & my_colours_pre.rec_red[i]<=200, paste("#00FF19FF"),
                                                              ifelse(my_colours_pre.rec_red[i]>200 & my_colours_pre.rec_red[i]<=500, paste("#80FF00FF"),
                                                                     ifelse(my_colours_pre.rec_red[i]>500 & my_colours_pre.rec_red[i]<=1000, paste("#FFE500FF"),
                                                                            ifelse(my_colours_pre.rec_red[i]>1000 & my_colours_pre.rec_red[i]<=2500, paste("#FF9900FF"),
                                                                                   ifelse(my_colours_pre.rec_red[i]>2500 & my_colours_pre.rec_red[i]<=4000, paste("#FF4D00FF"),
                                                                                          ifelse(my_colours_pre.rec_red[i]>4000 & my_colours_pre.rec_red[i]<6500, paste("#FF0000FF"), "#BEBEBE"))))))))))
}

my_colours_rec_black <- rep(0, length(my_colours_pre.rec_black))
for(i in 1:length(my_colours_pre.rec_black)){
  my_colours_rec_black[i] <- ifelse(my_colours_pre.rec_black[i]>0 & my_colours_pre.rec_black[i]<=10, paste("#7F00FFFF"),
                                    ifelse(my_colours_pre.rec_black[i]>10 & my_colours_pre.rec_black[i]<=25, paste("#001AFFFF"),
                                           ifelse(my_colours_pre.rec_black[i]>25 & my_colours_pre.rec_black[i]<=50, paste("#00B3FFFF"),
                                                  ifelse(my_colours_pre.rec_black[i]>50 & my_colours_pre.rec_black[i]<=100, paste("#00FFFFFF"),
                                                         ifelse(my_colours_pre.rec_black[i]>100 & my_colours_pre.rec_black[i]<=200, paste("#00FF19FF"),
                                                                ifelse(my_colours_pre.rec_black[i]>200 & my_colours_pre.rec_black[i]<=500, paste("#80FF00FF"),
                                                                       ifelse(my_colours_pre.rec_black[i]>500 & my_colours_pre.rec_black[i]<=1000, paste("#FFE500FF"),
                                                                              ifelse(my_colours_pre.rec_black[i]>1000 & my_colours_pre.rec_black[i]<=2500, paste("#FF9900FF"),
                                                                                     ifelse(my_colours_pre.rec_black[i]>2500 & my_colours_pre.rec_black[i]<=4000, paste("#FF4D00FF"),
                                                                                            ifelse(my_colours_pre.rec_black[i]>4000 & my_colours_pre.rec_black[i]<6500, paste("#FF0000FF"), "#BEBEBE"))))))))))
}


# Plot the grid cells and colour according to numbers:
par(mfrow=c(3,1))
par(mar=c(0.5, 0.5, 5, 0.5))

# All records
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of records in \n500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$Ntotal > 0 & !is.na(TrdRast_AR5@data$Ntotal), "Ntotal"],
     col=my_colours_rec_all, border=my_colours_rec_all, add=T)
legend("topright", legend=c("1-10", "11-20", "21-50", "51-100", "101-200",
                            "201-400", "401-1000", "1001-5000", "5001-10000", "10001-30000"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Threatened records
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of threatened \nrecords in 500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$Ntotal > 0 & !is.na(TrdRast_AR5@data$Ntotal), "Nred"],
     col=my_colours_rec_red, border=my_colours_rec_red, add=T)
legend("topright", legend=c("0", "1-10", "11-25", "26-50", "51-100", "101-200",
                            "201-500", "501-1000", "1001-2500", "2501-4000", "4001-6500"),
       fill=c("#BEBEBE", "#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Alien records
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of alien \nrecords in 500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$Ntotal > 0 & !is.na(TrdRast_AR5@data$Ntotal), "Nblack"],
     col=my_colours_rec_black, border=my_colours_rec_black, add=T)
legend("topright", legend=c("0", "1-10", "11-25", "26-50", "51-100", "101-200",
                            "201-500", "501-1000", "1001-2500", "2501-4000", "4001-6500"),
       fill=c("#BEBEBE", "#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)


##--- 2.6.2 Maps showing the observed species richness ---####
##--------------------------------------------------------####


# Get the numbers to base the colour on:
my_colours_pre.spec_all <- c(TrdRast_AR5@data[TrdRast_AR5@data$S.obs_2013>0 &
                                                !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_2013"])
my_colours_pre.spec_red <- c(TrdRast_AR5@data[TrdRast_AR5@data$S.obs_2013>0 &
                                                !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_reds_2013"])
my_colours_pre.spec_black <- c(TrdRast_AR5@data[TrdRast_AR5@data$S.obs_2013>0 &
                                                  !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_blacks_2013"])

# Make the vectors with colour names:
my_colours_spec_all <- rep(0, length(my_colours_pre.spec_all))
for(i in 1:length(my_colours_pre.spec_all)){
  my_colours_spec_all[i] <- ifelse(my_colours_pre.spec_all[i]<=5, paste("#7F00FFFF"),
                                   ifelse(my_colours_pre.spec_all[i]>5 & my_colours_pre.spec_all[i]<=10, paste("#001AFFFF"),
                                          ifelse(my_colours_pre.spec_all[i]>10 & my_colours_pre.spec_all[i]<=25, paste("#00B3FFFF"),
                                                 ifelse(my_colours_pre.spec_all[i]>25 & my_colours_pre.spec_all[i]<=50, paste("#00FFFFFF"),
                                                        ifelse(my_colours_pre.spec_all[i]>50 & my_colours_pre.spec_all[i]<=75, paste("#00FF19FF"),
                                                               ifelse(my_colours_pre.spec_all[i]>75 & my_colours_pre.spec_all[i]<=100, paste("#80FF00FF"),
                                                                      ifelse(my_colours_pre.spec_all[i]>100 & my_colours_pre.spec_all[i]<=125, paste("#FFE500FF"),
                                                                             ifelse(my_colours_pre.spec_all[i]>125 & my_colours_pre.spec_all[i]<=150, paste("#FF9900FF"),
                                                                                    ifelse(my_colours_pre.spec_all[i]>150 & my_colours_pre.spec_all[i]<=200, paste("#FF4D00FF"),
                                                                                           ifelse(my_colours_pre.spec_all[i]>200 & my_colours_pre.spec_all[i]<250, paste("#FF0000FF"), 'NA'))))))))))
}

my_colours_spec_red <- rep(0, length(my_colours_pre.spec_red))
for(i in 1:length(my_colours_pre.spec_red)){
  my_colours_spec_red[i] <- ifelse(my_colours_pre.spec_red[i]==1, paste("#7F00FFFF"),
                                   ifelse(my_colours_pre.spec_red[i]==2, paste("#001AFFFF"),
                                          ifelse(my_colours_pre.spec_red[i]>2 & my_colours_pre.spec_red[i]<=4, paste("#00B3FFFF"),
                                                 ifelse(my_colours_pre.spec_red[i]>4 & my_colours_pre.spec_red[i]<=7, paste("#00FFFFFF"),
                                                        ifelse(my_colours_pre.spec_red[i]>7 & my_colours_pre.spec_red[i]<=10, paste("#00FF19FF"),
                                                               ifelse(my_colours_pre.spec_red[i]>10 & my_colours_pre.spec_red[i]<=15, paste("#80FF00FF"),
                                                                      ifelse(my_colours_pre.spec_red[i]>15 & my_colours_pre.spec_red[i]<=20, paste("#FFE500FF"),
                                                                             ifelse(my_colours_pre.spec_red[i]>20 & my_colours_pre.spec_red[i]<=25, paste("#FF9900FF"),
                                                                                    ifelse(my_colours_pre.spec_red[i]>25 & my_colours_pre.spec_red[i]<=30, paste("#FF4D00FF"),
                                                                                           ifelse(my_colours_pre.spec_red[i]>30 & my_colours_pre.spec_red[i]<40, paste("#FF0000FF"), "#BEBEBE"))))))))))
}

my_colours_spec_black <- rep(0, length(my_colours_pre.spec_black))
for(i in 1:length(my_colours_pre.spec_black)){
  my_colours_spec_black[i] <- ifelse(my_colours_pre.spec_black[i]==1, paste("#7F00FFFF"),
                                     ifelse(my_colours_pre.spec_black[i]==2, paste("#001AFFFF"),
                                            ifelse(my_colours_pre.spec_black[i]>2 & my_colours_pre.spec_black[i]<=4, paste("#00B3FFFF"),
                                                   ifelse(my_colours_pre.spec_black[i]>4 & my_colours_pre.spec_black[i]<=7, paste("#00FFFFFF"),
                                                          ifelse(my_colours_pre.spec_black[i]>7 & my_colours_pre.spec_black[i]<=10, paste("#00FF19FF"),
                                                                 ifelse(my_colours_pre.spec_black[i]>10 & my_colours_pre.spec_black[i]<=15, paste("#80FF00FF"),
                                                                        ifelse(my_colours_pre.spec_black[i]>15 & my_colours_pre.spec_black[i]<=20, paste("#FFE500FF"),
                                                                               ifelse(my_colours_pre.spec_black[i]>20 & my_colours_pre.spec_black[i]<=25, paste("#FF9900FF"),
                                                                                      ifelse(my_colours_pre.spec_black[i]>25 & my_colours_pre.spec_black[i]<=30, paste("#FF4D00FF"),
                                                                                             ifelse(my_colours_pre.spec_black[i]>30 & my_colours_pre.spec_black[i]<40, paste("#FF0000FF"), "#BEBEBE"))))))))))
}

# Plot the grid cells and colour according to numbers
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of observed species in \n500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$S.obs_2013 > 0 & !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_2013"],
     col=my_colours_spec_all, border=my_colours_spec_all, add=T)
legend("topright", legend=c("1-5", "6-10", "11-25", "26-50", "51-75",
                            "76-100", "101-125", "126-150", "151-200", "201-232"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Threatened records
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of observed threatened \nspecies in 500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$S.obs_2013 > 0 & !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_reds_2013"],
     col=my_colours_spec_red, border=my_colours_spec_red, add=T)
legend("topright", legend=c("0", "1", "2", "3-4", "5-7", "8-10",
                            "11-15", "16-20", "21-25", "26-30", "31-38"),
       fill=c("#BEBEBE", "#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Alien records
DivMap(AR5, Trondheim, TrdRast_AR5, "Number of observed alien \nspecies in 500m x 500m cell")
plot(TrdRast_AR5[TrdRast_AR5@data$S.obs_2013 > 0 & !is.na(TrdRast_AR5@data$S.obs_2013), "S.obs_blacks_2013"],
     col=my_colours_spec_black, border=my_colours_spec_black, add=T)
legend("topright", legend=c("0", "1", "2", "3-4", "5-7", "8-10",
                            "11-15", "16-20", "21-25", "26-30", "31-38"),
       fill=c("#BEBEBE", "#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)


##--- 2.6.3 Map showing the Estimated Species Richness (Chao1) ---####
##----------------------------------------------------------------####
# OBS! Here we are only working with the cells with "enough" data - the criteria can be seen in the
# "Community matrices"-script. It is the data used for parameterizing the models:

# Get the numbers to base the colour on:
my_colours_pre.ESR_all <- c(TrdRast_model@data$log_chao.all)   
my_colours_pre.ESR_red <- c(TrdRast_model@data$log_chao.reds)
my_colours_pre.ESR_black <- c(TrdRast_model@data$log_chao.blacks)

# Make the vector with colour names:
my_colours_ESR_all <- rep(0, length(my_colours_pre.ESR_all))
for(i in 1:length(my_colours_pre.ESR_all)){
  my_colours_ESR_all[i] <- ifelse(my_colours_pre.ESR_all[i]<=0, paste("#7F00FFFF"),
                                  ifelse(my_colours_pre.ESR_all[i]>0 & my_colours_pre.ESR_all[i]<=1, paste("#001AFFFF"),
                                         ifelse(my_colours_pre.ESR_all[i]>1 & my_colours_pre.ESR_all[i]<=2, paste("#00B3FFFF"),
                                                ifelse(my_colours_pre.ESR_all[i]>2 & my_colours_pre.ESR_all[i]<=3, paste("#00FFFFFF"),
                                                       ifelse(my_colours_pre.ESR_all[i]>3 & my_colours_pre.ESR_all[i]<=4, paste("#00FF19FF"),
                                                              ifelse(my_colours_pre.ESR_all[i]>4 & my_colours_pre.ESR_all[i]<=5, paste("#80FF00FF"),
                                                                     ifelse(my_colours_pre.ESR_all[i]>5 & my_colours_pre.ESR_all[i]<=6, paste("#FFE500FF"),
                                                                            ifelse(my_colours_pre.ESR_all[i]>6 & my_colours_pre.ESR_all[i]<=7, paste("#FF9900FF"),
                                                                                   ifelse(my_colours_pre.ESR_all[i]>7 & my_colours_pre.ESR_all[i]<=8, paste("#FF4D00FF"),
                                                                                          ifelse(my_colours_pre.ESR_all[i]>8 & my_colours_pre.ESR_all[i]<9, paste("#FF0000FF"), 'NA'))))))))))
}

my_colours_ESR_red <- rep(0, length(my_colours_pre.ESR_red))
for(i in 1:length(my_colours_pre.ESR_red)){
  my_colours_ESR_red[i] <- ifelse(my_colours_pre.ESR_red[i]>(-7) & my_colours_pre.ESR_red[i]<=0, paste("#7F00FFFF"),
                                  ifelse(my_colours_pre.ESR_red[i]>0 & my_colours_pre.ESR_red[i]<=0.5, paste("#001AFFFF"),
                                         ifelse(my_colours_pre.ESR_red[i]>0.5 & my_colours_pre.ESR_red[i]<=1, paste("#00B3FFFF"),
                                                ifelse(my_colours_pre.ESR_red[i]>1 & my_colours_pre.ESR_red[i]<=1.5, paste("#00FFFFFF"),
                                                       ifelse(my_colours_pre.ESR_red[i]>1.5 & my_colours_pre.ESR_red[i]<=2, paste("#00FF19FF"),
                                                              ifelse(my_colours_pre.ESR_red[i]>2 & my_colours_pre.ESR_red[i]<=2.5, paste("#80FF00FF"),
                                                                     ifelse(my_colours_pre.ESR_red[i]>2.5 & my_colours_pre.ESR_red[i]<=3, paste("#FFE500FF"),
                                                                            ifelse(my_colours_pre.ESR_red[i]>3 & my_colours_pre.ESR_red[i]<=3.5, paste("#FF9900FF"),
                                                                                   ifelse(my_colours_pre.ESR_red[i]>3.5 & my_colours_pre.ESR_red[i]<=4, paste("#FF4D00FF"),
                                                                                          ifelse(my_colours_pre.ESR_red[i]>4 & my_colours_pre.ESR_red[i]<4.5, paste("#FF0000FF"), "#BEBEBE"))))))))))
}

my_colours_ESR_black <- rep(0, length(my_colours_pre.ESR_black))
for(i in 1:length(my_colours_pre.ESR_black)){
  my_colours_ESR_black[i] <- ifelse(my_colours_pre.ESR_black[i]>(-7) & my_colours_pre.ESR_black[i]<=0, paste("#7F00FFFF"),
                                    ifelse(my_colours_pre.ESR_black[i]>0 & my_colours_pre.ESR_black[i]<=0.5, paste("#001AFFFF"),
                                           ifelse(my_colours_pre.ESR_black[i]>0.5 & my_colours_pre.ESR_black[i]<=1, paste("#00B3FFFF"),
                                                  ifelse(my_colours_pre.ESR_black[i]>1 & my_colours_pre.ESR_black[i]<=1.5, paste("#00FFFFFF"),
                                                         ifelse(my_colours_pre.ESR_black[i]>1.5 & my_colours_pre.ESR_black[i]<=2, paste("#00FF19FF"),
                                                                ifelse(my_colours_pre.ESR_black[i]>2 & my_colours_pre.ESR_black[i]<=2.5, paste("#80FF00FF"),
                                                                       ifelse(my_colours_pre.ESR_black[i]>2.5 & my_colours_pre.ESR_black[i]<=3, paste("#FFE500FF"),
                                                                              ifelse(my_colours_pre.ESR_black[i]>3 & my_colours_pre.ESR_black[i]<=3.5, paste("#FF9900FF"),
                                                                                     ifelse(my_colours_pre.ESR_black[i]>3.5 & my_colours_pre.ESR_black[i]<=4, paste("#FF4D00FF"),
                                                                                            ifelse(my_colours_pre.ESR_black[i]>4 & my_colours_pre.ESR_black[i]<4.5, paste("#FF0000FF"), "#BEBEBE"))))))))))
}


# Plot the grid cells and colour according to numbers
# All species:
DivMap(AR5, Trondheim, TrdRast_AR5, "ESR (chao1) in \n500m x 500m cell")
plot(TrdRast_fm2[TrdRast_fm2@data$S.chao1_2013 > 0, "S.chao1_2013"],
     col=my_colours_ESR_all, border=my_colours_ESR_all, add=T)

legend("topright", legend=c("1-10", "11-25", "26-50", "51-100", "101-200",
                            "201-500", "501-1000", "1001-2000", "2001-3500", "3500-5050"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Threatened species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of threatened species \n(chao1)) in 500m x 500m cell")
plot(TrdRast_model[, "log_chao.reds"],
     col=my_colours_ESR_red, border=my_colours_ESR_red, add=T)
legend("topright", legend=c("(-7)-0", "0-0.5", "0.5-1", "1-1.5", "1.5-2",
                            "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Alien species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of alien species \n(chao1)) in 500m x 500m cell")
plot(TrdRast_model[, "log_chao.blacks"],
     col=my_colours_ESR_black, border=my_colours_ESR_black, add=T)
legend("topright", legend=c("(-7)-0", "0-0.5", "0.5-1", "1-1.5", "1.5-2",
                            "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)


##--- 2.6.4 Map showing richness estimatings based on the spatial models (threatened and alien species) ---####
##--- 2.6.4.1 Making predictions from models                                                            ---####
##---------------------------------------------------------------------------------------------------------####

# Now we want to try and make predictions on the number of either threatened or alien species based on
# the spatial models.
# For that, we need a dataset with the variables included in the model(s): the landcover data (percentage of cover)
# for all of Trondheim. 

# Creating a new dataframe with joined categories (first summing, then calculating relative area afterwards)
data_predict <- TrdRast_AR5[, c(1, 3:68)]
data_predict@data$total_area <- rowSums(data_predict@data[, 2:67])

# Sum the 'interesting' habitat types, and remove the uninteresting ones:
data_predict@data$Developed <- rowSums(data_predict@data[, c(2:3)])
data_predict@data$Agriculture <- rowSums(data_predict@data[, c(41:42,64:66)])
data_predict@data$Forest <- rowSums(data_predict@data[, c(4:39)])
data_predict@data$Marsh <- rowSums(data_predict@data[, c(47:55)])
data_predict@data$hfgl <- rowSums(data_predict@data[, c(43:46)])
data_predict@data$ofg_imp <- rowSums(data_predict@data[, c(59:61)])
data_predict@data$ofg_urban <- rowSums(data_predict@data[, c(57:58,62:63)])

data_predict <- data_predict[, -c(2:39, 41:55, 57:66)]
data_predict@data <- data_predict@data[,c(1, 5:12, 2:4)]    # Rearrange order of columns

# Recalculate all areas as percentage of total cell area:
for(i in 1:dim(data_predict@data)[1]) {
  for(j in 3:12) {
    data_predict@data[i,j] = (data_predict@data[i,j])/(data_predict@data[i,"total_area"])
  }
}

# Remove rows with NA (cells outside of Trondheim):
data_predict <- data_predict[!is.na(data_predict$total_area),]

# Merge the needed datasets:
data_predict@data <- merge(data_predict@data, TrdRast_2012@data[,c(1,4:12)], by="Pixelnr", all.x=TRUE)

# Calculate the change in the different landcover types in each cell. 
data_predict$delta_Developed <- data_predict@data$Developed - data_predict@data$Developed.2012
data_predict$delta_Agriculture <- data_predict@data$Agriculture - data_predict@data$Agriculture.2012
data_predict$delta_Forest <- data_predict@data$Forest - data_predict@data$Forest.2012
data_predict$delta_hfgl <- data_predict@data$hfgl - data_predict@data$hfgl.2012
data_predict$delta_ofg_imp <- data_predict@data$ofg_imp - data_predict@data$ofg_imp.2012
data_predict$delta_ofg_urban <- data_predict@data$ofg_urban - data_predict@data$ofg_urban.2012
data_predict$delta_freshwater <- data_predict@data$Freshwater - data_predict@data$Freshwater.2012
data_predict$delta_ocean <- data_predict@data$Ocean - data_predict@data$Ocean.2012
data_predict$delta_Marsh <- data_predict@data$Marsh - data_predict@data$Marsh.2012

# Remove grid cells covered only by ocean - these are useless in this model:
data_predict <- data_predict[!data_predict@data$Ocean==1,]

### Make the predictions for threatened and alien species:
data_predict$predict_reds <- predict(gls.exp_threat, newdata=data_predict)
data_predict$predict_blacks <- predict(gls.exp_alien, newdata=data_predict)

range(data_predict$predict_reds)
range(data_predict$predict_blacks)


##--- 2.6.4.2 Making maps ---####
##---------------------------####
# Get the numbers to base the colour on:
my_colours_pre.pred_red <- c(data_predict@data$predict_reds)
my_colours_pre.pred_black <- c(data_predict@data$predict_blacks)

# Make the vector with colour names:
my_colours_pred_red <- rep(0, length(my_colours_pre.pred_red))
for(i in 1:length(my_colours_pre.pred_red)){
  my_colours_pred_red[i] <- ifelse(my_colours_pre.pred_red[i]>(-2) & my_colours_pre.pred_red[i]<=0, paste("#7F00FFFF"),
                                   ifelse(my_colours_pre.pred_red[i]>0 & my_colours_pre.pred_red[i]<=0.5, paste("#001AFFFF"),
                                          ifelse(my_colours_pre.pred_red[i]>0.5 & my_colours_pre.pred_red[i]<=1, paste("#00B3FFFF"),
                                                 ifelse(my_colours_pre.pred_red[i]>1 & my_colours_pre.pred_red[i]<=1.5, paste("#00FFFFFF"),
                                                        ifelse(my_colours_pre.pred_red[i]>1.5 & my_colours_pre.pred_red[i]<=2, paste("#00FF19FF"),
                                                               ifelse(my_colours_pre.pred_red[i]>2 & my_colours_pre.pred_red[i]<=3, paste("#80FF00FF"),
                                                                      ifelse(my_colours_pre.pred_red[i]>3 & my_colours_pre.pred_red[i]<=4, paste("#FFE500FF"),
                                                                             ifelse(my_colours_pre.pred_red[i]>4 & my_colours_pre.pred_red[i]<=5, paste("#FF9900FF"),
                                                                                    ifelse(my_colours_pre.pred_red[i]>5 & my_colours_pre.pred_red[i]<=6, paste("#FF4D00FF"),
                                                                                           ifelse(my_colours_pre.pred_red[i]>6 & my_colours_pre.pred_red[i]<7.5, paste("#FF0000FF"), "#BEBEBE"))))))))))
}

my_colours_pred_black <- rep(0, length(my_colours_pre.pred_black))
for(i in 1:length(my_colours_pre.pred_black)){
  my_colours_pred_black[i] <- ifelse(my_colours_pre.pred_black[i]>(-5) & my_colours_pre.pred_black[i]<=0, paste("#7F00FFFF"),
                                     ifelse(my_colours_pre.pred_black[i]>0 & my_colours_pre.pred_black[i]<=0.5, paste("#001AFFFF"),
                                            ifelse(my_colours_pre.pred_black[i]>0.5 & my_colours_pre.pred_black[i]<=1, paste("#00B3FFFF"),
                                                   ifelse(my_colours_pre.pred_black[i]>1 & my_colours_pre.pred_black[i]<=1.5, paste("#00FFFFFF"),
                                                          ifelse(my_colours_pre.pred_black[i]>1.5 & my_colours_pre.pred_black[i]<=2, paste("#00FF19FF"),
                                                                 ifelse(my_colours_pre.pred_black[i]>2 & my_colours_pre.pred_black[i]<=3, paste("#80FF00FF"),
                                                                        ifelse(my_colours_pre.pred_black[i]>3 & my_colours_pre.pred_black[i]<=4, paste("#FFE500FF"),
                                                                               ifelse(my_colours_pre.pred_black[i]>4 & my_colours_pre.pred_black[i]<=5, paste("#FF9900FF"),
                                                                                      ifelse(my_colours_pre.pred_black[i]>5 & my_colours_pre.pred_black[i]<=6, paste("#FF4D00FF"),
                                                                                             ifelse(my_colours_pre.pred_black[i]>6 & my_colours_pre.pred_black[i]<7.5, paste("#FF0000FF"), "#BEBEBE"))))))))))
}


# Plot the grid cells and colour according to numbers
par(mfrow=c(1,2))

# Threatened species:
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell")
plot(data_predict,
     col=my_colours_pred_red, border=my_colours_pred_red, add=T)
legend("topright", legend=c("-2-0", "0-0.5", "0.5-1", "1-1.5", "1.5-2",
                            "2-3", "3-4", "4-5", "5-6", "6-7.5"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)

# Alien species:
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell")
plot(data_predict,
     col=my_colours_pred_black, border=my_colours_pred_black, add=T)
legend("topright", legend=c("-5-0", "0-0.5", "0.5-1", "1-1.5", "1.5-2",
                            "2-3", "3-4", "4-5", "5-6", "6-7.5"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=0.6)




##--- 2.6.5 Maps comparing all of the data and predictions ---####
##--- 2.6.5.1 Make the vectors with colour names           ---####
##------------------------------------------------------------####

# The vectors with colour names needs to have the same range, if the maps are to be comparable
# Chao1:
colours_all_ESR <- rep(0, length(my_colours_pre.ESR_all))
for(i in 1:length(my_colours_pre.ESR_all)){
  colours_all_ESR[i] <- ifelse(my_colours_pre.ESR_all[i]<=0, paste("#7F00FFFF"),
                               ifelse(my_colours_pre.ESR_all[i]>0 & my_colours_pre.ESR_all[i]<=1, paste("#001AFFFF"),
                                      ifelse(my_colours_pre.ESR_all[i]>1 & my_colours_pre.ESR_all[i]<=2, paste("#00B3FFFF"),
                                             ifelse(my_colours_pre.ESR_all[i]>2 & my_colours_pre.ESR_all[i]<=3, paste("#00FFFFFF"),
                                                    ifelse(my_colours_pre.ESR_all[i]>3 & my_colours_pre.ESR_all[i]<=4, paste("#00FF19FF"),
                                                           ifelse(my_colours_pre.ESR_all[i]>4 & my_colours_pre.ESR_all[i]<=5, paste("#80FF00FF"),
                                                                  ifelse(my_colours_pre.ESR_all[i]>5 & my_colours_pre.ESR_all[i]<=6, paste("#FFE500FF"),
                                                                         ifelse(my_colours_pre.ESR_all[i]>6 & my_colours_pre.ESR_all[i]<=7, paste("#FF9900FF"),
                                                                                ifelse(my_colours_pre.ESR_all[i]>7 & my_colours_pre.ESR_all[i]<=8, paste("#FF4D00FF"),
                                                                                       ifelse(my_colours_pre.ESR_all[i]>8 & my_colours_pre.ESR_all[i]<9, paste("#FF0000FF"), 'NA'))))))))))
}

colours_red_ESR <- rep(0, length(my_colours_pre.ESR_red))
for(i in 1:length(my_colours_pre.ESR_red)){
  colours_red_ESR[i] <- ifelse(my_colours_pre.ESR_red[i]>-7 & my_colours_pre.ESR_red[i]<=0, paste("#7F00FFFF"),
                               ifelse(my_colours_pre.ESR_red[i]>0 & my_colours_pre.ESR_red[i]<=1, paste("#001AFFFF"),
                                      ifelse(my_colours_pre.ESR_red[i]>1 & my_colours_pre.ESR_red[i]<=2, paste("#00B3FFFF"),
                                             ifelse(my_colours_pre.ESR_red[i]>2 & my_colours_pre.ESR_red[i]<=3, paste("#00FFFFFF"),
                                                    ifelse(my_colours_pre.ESR_red[i]>3 & my_colours_pre.ESR_red[i]<=4, paste("#00FF19FF"),
                                                           ifelse(my_colours_pre.ESR_red[i]>4 & my_colours_pre.ESR_red[i]<=5, paste("#80FF00FF"),
                                                                  ifelse(my_colours_pre.ESR_red[i]>5 & my_colours_pre.ESR_red[i]<=6, paste("#FFE500FF"),
                                                                         ifelse(my_colours_pre.ESR_red[i]>6 & my_colours_pre.ESR_red[i]<=7, paste("#FF9900FF"),
                                                                                ifelse(my_colours_pre.ESR_red[i]>7 & my_colours_pre.ESR_red[i]<=8, paste("#FF4D00FF"),
                                                                                       ifelse(my_colours_pre.ESR_red[i]>8 & my_colours_pre.ESR_red[i]<9, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

colours_black_ESR <- rep(0, length(my_colours_pre.ESR_black))
for(i in 1:length(my_colours_pre.ESR_black)){
  colours_black_ESR[i] <- ifelse(my_colours_pre.ESR_black[i]>-7 & my_colours_pre.ESR_black[i]<=0, paste("#7F00FFFF"),
                                 ifelse(my_colours_pre.ESR_black[i]>0 & my_colours_pre.ESR_black[i]<=1, paste("#001AFFFF"),
                                        ifelse(my_colours_pre.ESR_black[i]>1 & my_colours_pre.ESR_black[i]<=2, paste("#00B3FFFF"),
                                               ifelse(my_colours_pre.ESR_black[i]>2 & my_colours_pre.ESR_black[i]<=3, paste("#00FFFFFF"),
                                                      ifelse(my_colours_pre.ESR_black[i]>3 & my_colours_pre.ESR_black[i]<=4, paste("#00FF19FF"),
                                                             ifelse(my_colours_pre.ESR_black[i]>4 & my_colours_pre.ESR_black[i]<=5, paste("#80FF00FF"),
                                                                    ifelse(my_colours_pre.ESR_black[i]>5 & my_colours_pre.ESR_black[i]<=6, paste("#FFE500FF"),
                                                                           ifelse(my_colours_pre.ESR_black[i]>6 & my_colours_pre.ESR_black[i]<=7, paste("#FF9900FF"),
                                                                                  ifelse(my_colours_pre.ESR_black[i]>7 & my_colours_pre.ESR_black[i]<=8, paste("#FF4D00FF"),
                                                                                         ifelse(my_colours_pre.ESR_black[i]>8 & my_colours_pre.ESR_black[i]<9, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

# Predicted:
colours_red_pred <- rep(0, length(my_colours_pre.pred_red))
for(i in 1:length(my_colours_pre.pred_red)){
  colours_red_pred[i] <- ifelse(my_colours_pre.pred_red[i]>-7 & my_colours_pre.pred_red[i]<=0, paste("#7F00FFFF"),
                                ifelse(my_colours_pre.pred_red[i]>0 & my_colours_pre.pred_red[i]<=1, paste("#001AFFFF"),
                                       ifelse(my_colours_pre.pred_red[i]>1 & my_colours_pre.pred_red[i]<=2, paste("#00B3FFFF"),
                                              ifelse(my_colours_pre.pred_red[i]>2 & my_colours_pre.pred_red[i]<=3, paste("#00FFFFFF"),
                                                     ifelse(my_colours_pre.pred_red[i]>3 & my_colours_pre.pred_red[i]<=4, paste("#00FF19FF"),
                                                            ifelse(my_colours_pre.pred_red[i]>4 & my_colours_pre.pred_red[i]<=5, paste("#80FF00FF"),
                                                                   ifelse(my_colours_pre.pred_red[i]>5 & my_colours_pre.pred_red[i]<=6, paste("#FFE500FF"),
                                                                          ifelse(my_colours_pre.pred_red[i]>6 & my_colours_pre.pred_red[i]<=7, paste("#FF9900FF"),
                                                                                 ifelse(my_colours_pre.pred_red[i]>7 & my_colours_pre.pred_red[i]<=8, paste("#FF4D00FF"),
                                                                                        ifelse(my_colours_pre.pred_red[i]>8 & my_colours_pre.pred_red[i]<9, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

colours_black_pred <- rep(0, length(my_colours_pre.pred_black))
for(i in 1:length(my_colours_pre.pred_black)){
  colours_black_pred[i] <- ifelse(my_colours_pre.pred_black[i]>-7 & my_colours_pre.pred_black[i]<=0, paste("#7F00FFFF"),
                                  ifelse(my_colours_pre.pred_black[i]>0 & my_colours_pre.pred_black[i]<=1, paste("#001AFFFF"),
                                         ifelse(my_colours_pre.pred_black[i]>1 & my_colours_pre.pred_black[i]<=2, paste("#00B3FFFF"),
                                                ifelse(my_colours_pre.pred_black[i]>2 & my_colours_pre.pred_black[i]<=3, paste("#00FFFFFF"),
                                                       ifelse(my_colours_pre.pred_black[i]>3 & my_colours_pre.pred_black[i]<=4, paste("#00FF19FF"),
                                                              ifelse(my_colours_pre.pred_black[i]>4 & my_colours_pre.pred_black[i]<=5, paste("#80FF00FF"),
                                                                     ifelse(my_colours_pre.pred_black[i]>5 & my_colours_pre.pred_black[i]<=6, paste("#FFE500FF"),
                                                                            ifelse(my_colours_pre.pred_black[i]>6 & my_colours_pre.pred_black[i]<=7, paste("#FF9900FF"),
                                                                                   ifelse(my_colours_pre.pred_black[i]>7 & my_colours_pre.pred_black[i]<=8, paste("#FF4D00FF"),
                                                                                          ifelse(my_colours_pre.pred_black[i]>8 & my_colours_pre.pred_black[i]<9, paste("#FF0000FF"), '#BEBEBE'))))))))))
}





##--- 2.6.5.2 Make the maps                                ---####
##------------------------------------------------------------####
par(mfrow=c(3,2))
par(mar=c(0.5,0.5,6,0.5))


DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR) in \n500m x 500m cell")
plot(TrdRast_model[, "log_chao.all"],
     col=colours_all_ESR, border=colours_all_ESR, add=T, cex.main=0.75)

plot(0,type='n',axes=FALSE,ann=FALSE)

legend("top", legend=c("-7-0", "0-1", "1-2", "2-3", "3-4",
                       "4-5", "5-6", "6-7", "7-8", "8-9"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), ncol=2, bty="n", cex=0.6)

# Threatened species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of threatened species) \n in 500m x 500m cell")
plot(TrdRast_model[, "log_chao.reds"],
     col=colours_red_ESR, border=colours_red_ESR, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell")
plot(data_predict,
     col=colours_red_pred, border=colours_red_pred, add=T, cex.main=0.75)

# Alien species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of alien species) \nin 500m x 500m cell")
plot(TrdRast_model[, "log_chao.blacks"],
     col=colours_black_ESR, border=colours_black_ESR, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell")
plot(data_predict,
     col=colours_black_pred, border=colours_black_pred, add=T, cex.main=0.75)




##--- 2.7 LIMITED DATASETS ---####
##----------------------------####

# The models tried out above gave outrageous predictions, likely due to the large amounts of zeros in the
# data, thus giving two peaks in the density plot.
# What happens if we leave out those grid cells? (absolutely not optimal, but a last resort)
# This is potentially not the direction we want to take, as it leaves out information: where
# the species are NOT (we thus have no "True Absence").

# These models do not provide reasonable predictions either

##--- 2.7.1 CREATING DATAFRAMES AND DATA EXPLORATION ---####
##------------------------------------------------------####
# Remove the grid cells without any registrations of either threatened or alien species:
TrdRast_model_red <- TrdRast_model[!TrdRast_model@data$S.chao1_reds_2013==0,]
TrdRast_model_black <- TrdRast_model[!TrdRast_model@data$S.chao1_blacks_2013==0,]

### Outliers:
MyVar <- c("log_chao.reds", "log_chao.blacks", "Developed", "Agriculture", "Forest", "hfgl",
           "ofg_imp", "ofg_urban", "Freshwater", "Ocean",
           "Freshwater.2012", "Ocean.2012", "Developed.2012", "Agriculture.2012", "Forest.2012",
           "hfgl.2012", "ofg_imp.2012", "ofg_urban.2012",
           "delta_Developed", "delta_Agriculture", "delta_Forest", "delta_hfgl",
           "delta_ofg_imp", "delta_ofg_urban", "delta_freshwater", "delta_ocean" )

Mydotplot(TrdRast_model_red@data[,MyVar])
Mydotplot(TrdRast_model_black@data[,MyVar])
# We do have a few outliers, which we might have to deal with.

### Colinearity
pairs(TrdRast_model_red@data[, MyVar[c(1,3:10,19:26)]], 
      lower.panel = panel.cor)
pairs(TrdRast_model_black@data[, MyVar[c(2:10,19:26)]], 
      lower.panel = panel.cor)

### Relationships
Myxyplot(TrdRast_model_red@data, MyVar[3:18], "log_chao.reds", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_model_black@data, MyVar[3:18], "log_chao.blacks", 
         MyYlab = "ESR of alien species")
# We cannot see any clear relationships from this, unfortunately


### Normal distribution of data
# Density plots:
par(mfrow=c(1,1))
par(mar=c(2,2,3,0.5))
for(i in c(45)){
  plot(density.default(TrdRast_model_red@data[!is.na(TrdRast_model_red@data[,i]),i]),
       main=colnames(TrdRast_model_red@data[i]),
       cex.main=0.75, cex.axis=0.6)
}       
for(i in c(46)){
  plot(density.default(TrdRast_model_black@data[!is.na(TrdRast_model_black@data[,i]),i]),
       main=colnames(TrdRast_model_black@data[i]),
       cex.main=0.75, cex.axis=0.6)
}

# QQplots
for(i in c(45)){
  qqnorm(TrdRast_model_red@data[!is.na(TrdRast_model_red@data[,i]),i], main=colnames(TrdRast_model_red@data[i]),
         cex.main=0.75, cex.axis=0.6, cex.lab=0.6) ; qqline(TrdRast_model_red@data[!is.na(TrdRast_model_red@data[,i]),i], col="red")
}       
for(i in c(46)){
  qqnorm(TrdRast_model_black@data[!is.na(TrdRast_model_black@data[,i]),i], main=colnames(TrdRast_model_black@data[i]),
         cex.main=0.75, cex.axis=0.6, cex.lab=0.6) ; qqline(TrdRast_model_black@data[!is.na(TrdRast_model_black@data[,i]),i], col="red")
}


##--- 2.8 PRELIMINARY MODELLING (NON-SPATIAL) ---####
##--- 2.8.1 Model 1 - threatened species      ---####
##-----------------------------------------------####

M1_red <- glm(log_chao.reds ~  Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl
             + delta_ofg_imp + delta_ofg_urban + delta_freshwater ,
             family = "gaussian",
             data = TrdRast_model_red@data)

summary(M1_red)


### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(271.93 - 178.22) / 271.93    # Relatively large, actually

### Model validation 2: Is everything significant?
drop1(M1_red, test = "Chi")
step(M1_red) #Backwards selection using AIC

# According to 'step()', we can remove some of the variables - the optimal model being:
# log_chao.reds ~ Developed + Agriculture + Forest + Marsh + delta_freshwater
M1.2_red <- glm(log_chao.reds ~ Developed + Agriculture + Forest + Marsh + delta_freshwater,
               family = "gaussian", 
               data = TrdRast_model_red@data)
summary(M1.2_red)
# The coefficient values seem a little odd - the relationships are in many cases opposite of what I expected

# Plot residuals vs fitted values (M1)
F1_red <- fitted(M1_red)
E1_red <- resid(M1_red, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_red, 
     y = E1_red,
     xlab = "Fitted values - M1",
     ylab = "Pearson residuals - M1",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot residuals vs fitted values (M1.2)
F1.2_red <- fitted(M1.2_red)
E1.2_red <- resid(M1.2_red, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1.2_red, 
     y = E1.2_red,
     xlab = "Fitted values - M1.2",
     ylab = "Pearson residuals - M1.2",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_model_red@data$E1_red <- E1_red
Myxyplot(TrdRast_model_red@data, MyVar, "E1_red")
TrdRast_model_red@data$E1_red <- NULL

TrdRast_model_red@data$E1.2_red <- E1.2_red
Myxyplot(TrdRast_model_red@data, MyVar, "E1.2_red")
TrdRast_model_red@data$E1.2_red <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E1_red)
hist(E1.2_red)



##--- 2.8.2 Model 2 - alien species ---####
##-------------------------------------####

M2_black <- glm(log_chao.blacks ~  Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl
             + delta_ofg_imp + delta_ofg_urban + delta_freshwater ,
             family = "gaussian",
             data = TrdRast_model_black@data)

summary(M2_black)

### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(220.45 - 180.14) / 220.45    # Relatively large, actually?

### Model validation 2: Is everything significant?
drop1(M2_black, test = "Chi")
step(M2_black) #Backwards selection using AIC

# According to 'step()', we can remove quite a few of the variables - the optimal model being:
# log_chao.blacks ~ Developed + Agriculture + Forest + Marsh + ofg_urban + delta_Developed
M2.2_black <- glm(log_chao.blacks ~ Developed + Agriculture + Forest +  Marsh + ofg_urban + delta_Developed,
               family = "gaussian", 
               data = TrdRast_model_black@data)
summary(M2.2_black)


# Plot residuals vs fitted values
F2_black <- fitted(M2_black)
E2_black <- resid(M2_black, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2_black, 
     y = E2_black,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)       

F2.2_black <- fitted(M2.2_black)
E2.2_black <- resid(M2.2_black, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2.2_black, 
     y = E2.2_black,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)       

# Plot the residuals vs each covariate     
TrdRast_model_black@data$E2_black <- E2_black
Myxyplot(TrdRast_model_black@data, MyVar, "E2_black")
TrdRast_model_black@data$E2_black <- NULL

TrdRast_model_black@data$E2.2_black <- E2.2_black
Myxyplot(TrdRast_model_black@data, MyVar, "E2.2_black")
TrdRast_model_black@data$E2.2_black <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E2_black)
hist(E2.2_black)


##--- 2.9 SPATIAL AUTOCORRELATION- threatened species ---####
##--- 2.9.1 Testing for SAC - Chao1_reds              ---####
##-------------------------------------------------------####
library(spdep)
library(ncf)

# We have already subsetted the dataset, but we need the coordinates.
# OBS! We have to only use the cells for which have data on all variables - otherwise we get errors:
xy_red <- coordinates(TrdRast_model_red)

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model_red$log_chao.reds) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model_red, col=(TrdRast_model_red$log_chao.reds))   
par(mar=c(5,1,5,2.5))
image(y=0:5,z=t(0:5), col=rev(col.heat), axes=FALSE, main="log(threatened\n+0.01)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog1_red <- correlog(xy_red[,1], xy_red[,2], residuals(M1_red), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog1_red$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_red[,1], xy_red[,2], col=c("blue", "red")[sign(resid(M1_red))/2+1.5], pch=19,
     cex=abs(resid(M1_red))/max(resid(M1_red))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
M1_red.nb <- dnearneigh(as.matrix(xy_red[,1:2]), 0, 1500) # Find the neighbors - give lower and upper distance class here
# OBS! The classes are in euclidian distance (m), thus we need a reasonable distance to define
# a neighbouring grid cell. Here, I have chosen to use 1.5 km 
# to make it reasonable! Otherwise, use the following list:
# w_pp <- poly2nb(TrdRast_pp, row.names=TrdRast_pp$Pixelnr)     # Find the neighbors
# ww_pp <-  nb2listw(w_pp, style='B', zero.policy = TRUE)       # Make it a "listw" spatial object
M1_red.listw <- nb2listw(M1_red.nb, zero.policy = T)   
# Turns neighbourhood object into a weighted list
# this next step takes often several minutes to run:
GlobMT1_red <- moran.test(residuals(M1_red), listw=M1_red.listw, zero.policy = T)
GlobMT1_red

# We do not have SAC in these model residuals.
# Lets have a look at whether there is SAC in the data itself rather than only the residuals:
moran(TrdRast_model_red$log_chao.reds, M1_red.listw, n=length(M1_red.listw$neighbours),
      S0=Szero(M1_red.listw), zero.policy = TRUE)    # Calculate Moran's I

# Test for significance:
moran.test(TrdRast_model_red$log_chao.reds, M1_red.listw, randomisation=FALSE,
           alternative = "two.sided", zero.policy = TRUE)     # Using linear regression based logic and assumptions

MC_red <- moran.mc(TrdRast_model_red$log_chao.reds, M1_red.listw,
                  zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_red, main=NULL)     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_red$statistic, lty=2, col="red")

# Make a correlogram
sp.corr_ns <- sp.correlogram(M1_ns.nb, TrdRast_model$log_chao.reds, order=8, method="I", zero.policy = TRUE)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(sp.corr_ns)

# So, we have autocorrelation in the data itself but not in the model residuals
# Test the reduced model as well:
GlobMT1.2_red <- moran.test(residuals(M1.2_red), listw=M1_red.listw, zero.policy = T)
GlobMT1.2_red

# No SAC in either of the models


##--- 2.9.2 Testing for SAC - Chao1_blacks             ---####
##--------------------------------------------------------####
xy_black <- coordinates(TrdRast_model_black)
M1_black.nb <- dnearneigh(as.matrix(xy_black[,1:2]), 0, 1500) # Find the neighbors - give lower and upper distance class here
M1_black.listw <- nb2listw(M1_black.nb, zero.policy = T)   


# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model_black$log_chao.blacks) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model_black, col=(TrdRast_model_black$log_chao.blacks)) 
par(mar=c(5,1,5,2.5))
image(y=0:6,z=t(0:6), col=rev(col.heat), axes=FALSE, main="log(alien\n+00.1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog2_black <- correlog(xy_black[,1], xy_black[,2], residuals(M2_black), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2_black$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_black[,1], xy_black[,2], col=c("blue", "red")[sign(resid(M2_black))/2+1.5], pch=19,
     cex=abs(resid(M2_black))/max(resid(M2_black))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and test for its significance:
GlobMT2_ns <- moran.test(residuals(M2_black), listw=M1_black.listw, zero.policy = T)
GlobMT2_ns

# We borderlien have SAC

### For the reduced model:
# Make a correlogram:
correlog2.2_black <- correlog(xy_black[,1], xy_black[,2], residuals(M2.2_black), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2.2_black$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_black[,1], xy_black[,2], col=c("blue", "red")[sign(resid(M2.2_black))/2+1.5], pch=19,
     cex=abs(resid(M2.2_black))/max(resid(M2.2_black))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT2.2_black <- moran.test(residuals(M2.2_black), listw=M1_black.listw, zero.policy = T)
GlobMT2.2_black


##--- 2.10 DEALING WITH SAC ---####
##--- 2.10.1 Chao1_reds     ---####
##-----------------------------####
summary(gls.exp_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                            delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                            delta_ofg_urban + delta_freshwater ,
                          data=TrdRast_model_red@data, correlation=corExp(form=~xy_red[,1]+xy_red[,2])))
summary(gls.gauss_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model_red@data, correlation=corGaus(form=~xy_red[,1]+xy_red[,2])))
summary(gls.spher_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model_red@data, correlation=corSpher(form=~xy_red[,1]+xy_red[,2])))
summary(gls.lin_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                            delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                            delta_ofg_urban + delta_freshwater ,
                          data=TrdRast_model_red@data, correlation=corLin(form=~xy_red[,1]+xy_red[,2])))
summary(gls.Ratio_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model_red@data, correlation=corRatio(form=~~xy_red[,1]+xy_red[,2])))

AIC(M1_red, gls.exp_red, gls.gauss_red, gls.spher_red, gls.lin_red, gls.Ratio_red)
# All spatial correlations makes a better model.
# Since the deltaAIC between the spatial models are less than 2, we go for the simplest one: gls.exp
# Remove the others (for space):
rm(gls.gauss_red)
rm(gls.spher_red)
rm(gls.lin_red)
rm(gls.Ratio_red)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp_ML_red <- gls(log_chao.reds ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                               delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                               delta_ofg_urban + delta_freshwater ,
                             data=TrdRast_model_red@data, correlation=corExp(form=~xy_red[,1]+xy_red[,2]), method = "ML"))

drop1(gls.exp_ML_red, test = "Chi")
library(MASS)
stepAIC(gls.exp_ML_red)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the optimal model is:
# gls(log_chao.reds ~ Developed + Agriculture + Forest + Marsh + delta_freshwater)
# This is similar to the optimal model for the non-spatial approach - the coefficients seem a little odd though

gls.exp_threat_red <- gls(log_chao.reds ~Developed + Agriculture + Forest + Marsh + delta_freshwater,
                      data=TrdRast_model_red@data, correlation=corExp(form=~xy_red[,1]+xy_red[,2]))
summary(gls.exp_threat)


##--- 2.10.2 Chao1_blacks      ---####
##--------------------------------####
summary(gls.exp.b_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model_black@data, correlation=corExp(form=~xy_black[,1]+xy_black[,2])))
summary(gls.gauss.b_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model_black@data, correlation=corGaus(form=~xy_black[,1]+xy_black[,2])))
summary(gls.spher.b_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model_black@data, correlation=corSpher(form=~xy_black[,1]+xy_black[,2])))
summary(gls.lin.b_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                              delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                              delta_ofg_urban + delta_freshwater ,
                            data=TrdRast_model_black@data, correlation=corLin(form=~xy_black[,1]+xy_black[,2])))
summary(gls.Ratio.b_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                delta_ofg_urban + delta_freshwater ,
                              data=TrdRast_model_black@data, correlation=corRatio(form=~~xy_black[,1]+xy_black[,2])))

AIC(M2_black, gls.exp.b_black, gls.gauss.b_black, gls.spher.b_black, gls.lin.b_black, gls.Ratio.b_black)
# All spatial correlations makes a better model.
# To evaluate which correlation structure is the best one, we can look at the AIC.
# Since the deltaAIC between the two best models are less than 2, we go for the simplest one: gls.exp
rm(gls.gauss.b_black, gls.spher.b_black, gls.lin.b_black, gls.Ratio.b_black)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp.b_ML_black <- gls(log_chao.blacks ~ Developed + Agriculture + Forest + Freshwater + Marsh + hfgl + ofg_imp + ofg_urban +
                                 delta_Developed + delta_Agriculture + delta_Forest + delta_Marsh + delta_hfgl + delta_ofg_imp + 
                                 delta_ofg_urban + delta_freshwater ,
                               data=TrdRast_model_black@data, correlation=corExp(form=~xy_black[,1]+xy_black[,2]), method = "ML"))

drop1(gls.exp.b_ML_black, test = "Chi")
stepAIC(gls.exp.b_ML_black)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the optimal model is:
# gls(log_chao.blacks ~ Agriculture + Marsh + ofg_urban + delta_Developed)
# This is somewhat similar to the optimal model for the non-spatial approach!

gls.exp_alien_black <- gls(log_chao.blacks ~ Agriculture + Marsh + ofg_urban + delta_Developed,
                     data=TrdRast_model_black@data, correlation=corExp(form=~xy_black[,1]+xy_black[,2]))
summary(gls.exp_alien_black)



##--- 2.10.4 Map showing richness estimatings based on the spatial models (threatened and alien species) ---####
##--- 2.10.4.1 Making predictions from models                                                            ---####
##----------------------------------------------------------------------------------------------------------####

# Now we want to try and make predictions on the number of either threatened or alien species based on
# the spatial models.
# For that, we need a dataset with the variables included in the model(s): the landcover data (percentage of cover)
# for all of Trondheim. 

# Creating a new dataframe with joined categories (first summing, then calculating relative area afterwards)
data_predict <- TrdRast_AR5[, c(1, 3:68)]
data_predict@data$total_area <- rowSums(data_predict@data[, 2:67])

# Sum the 'interesting' habitat types, and remove the uninteresting ones:
data_predict@data$Developed <- rowSums(data_predict@data[, c(2:3)])
data_predict@data$Agriculture <- rowSums(data_predict@data[, c(41:42,64:66)])
data_predict@data$Forest <- rowSums(data_predict@data[, c(4:39)])
data_predict@data$Marsh <- rowSums(data_predict@data[, c(47:55)])
data_predict@data$hfgl <- rowSums(data_predict@data[, c(43:46)])
data_predict@data$ofg_imp <- rowSums(data_predict@data[, c(59:61)])
data_predict@data$ofg_urban <- rowSums(data_predict@data[, c(57:58,62:63)])

data_predict <- data_predict[, -c(2:39, 41:55, 57:66)]
data_predict@data <- data_predict@data[,c(1, 5:12, 2:4)]    # Rearrange order of columns

# Recalculate all areas as percentage of total cell area:
for(i in 1:dim(data_predict@data)[1]) {
  for(j in 3:12) {
    data_predict@data[i,j] = (data_predict@data[i,j])/(data_predict@data[i,"total_area"])
  }
}

# Remove rows with NA (cells outside of Trondheim):
data_predict <- data_predict[!is.na(data_predict$total_area),]

# Merge the needed datasets:
data_predict@data <- merge(data_predict@data, TrdRast_2012@data[,c(1,4:12)], by="Pixelnr", all.x=TRUE)

# Calculate the change in the different landcover types in each cell. 
data_predict$delta_Developed <- data_predict@data$Developed - data_predict@data$Developed.2012
data_predict$delta_Agriculture <- data_predict@data$Agriculture - data_predict@data$Agriculture.2012
data_predict$delta_Forest <- data_predict@data$Forest - data_predict@data$Forest.2012
data_predict$delta_hfgl <- data_predict@data$hfgl - data_predict@data$hfgl.2012
data_predict$delta_ofg_imp <- data_predict@data$ofg_imp - data_predict@data$ofg_imp.2012
data_predict$delta_ofg_urban <- data_predict@data$ofg_urban - data_predict@data$ofg_urban.2012
data_predict$delta_freshwater <- data_predict@data$Freshwater - data_predict@data$Freshwater.2012
data_predict$delta_ocean <- data_predict@data$Ocean - data_predict@data$Ocean.2012
data_predict$delta_Marsh <- data_predict@data$Marsh - data_predict@data$Marsh.2012

# Remove grid cells covered only by ocean - these are useless in this model:
data_predict <- data_predict[!data_predict@data$Ocean==1,]

### Make the predictions for threatened and alien species:
data_predict$predict_reds <- predict(gls.exp_threat_red, newdata=data_predict)
data_predict$predict_blacks <- predict(gls.exp_alien_black, newdata=data_predict)

range(data_predict$predict_reds)
range(data_predict$predict_blacks)


# The map has not been made as it was completely out of bounds



##--- 4. ORDINATION OF THE LAND COVER ---####
##---------------------------------------####

# The idea here is to use some sort of ordination or clustering on the data to potentially identify the interesting habitat variables.
# Hopefully we can then use all of the habitat categories by reducing the dimensions.
# Exploratory analyses showed that neither PCA nor NMDS brings out anything useful (some of the code for the PCA is saved underneath).
# However, the clustering looks promising, and is what I'll continue with here, potentially coupled with some Indicator Species
# Analysis.

# The dataset I will use is the entire grid of Trondheim minus the cells only covered in ocean:

##--- 4.1 OUTCOMMENTED CODE FOR THE PCA ####
##--------------------------------------####
#pca_data3 <- subset(TrdRast_AR5@data, !Ocean==total_area)

# Clean up and remove unnecessary columns:
#pca_data3[is.na(pca_data3)] <- 0
#rownames(pca_data3) <- pca_data3[,1]                          # Make Pixelnr the rownames
#pca_data3[,1] <- NULL
#pca_data3 <- pca_data3[,-c(1,68:81)]                          # Only retain the habitat variables
#pr3 <- prcomp(pca_data3, scale=T)

#summary(pr3)
#pr3$rotation
#barplot(100*summary(pr3)$importance[2,], ylab="% variance", las=2, cex.names=0.5,
#        col=c(rep("skyblue", 9), rep("dodgerblue", 14), rep("blue3", 17), rep("navy", 26)))
#legend("topright", legend = c("Cumulative 26%", "Cumulative 50%", "Cumulative 75%", "Cumulative 100%"),
#       fill=c("skyblue", "dodgerblue", "blue3", "navy"))
#biplot(pr3, cex=0.6, scale=0, col=c("black", "red"))
#biplot(pr3, choices=c(1,3), cex=.6, scale=0, col=c("black", "red"))

# Function for seeing the PCA-loadings - specify the PCA-object and which dimension is to be shown,
# then the function will show it in decreasing order:
#PC <- function(principal_component, dimension){
#    principal_component_x <- principal_component$rotation[,dimension]
#    print(principal_component_x[order(abs(principal_component_x), decreasing = T)])
#}

#PC(pr, 1)

#PC1 <- as.data.frame(pr2$x[,1])
#PC1 <- data.frame(PC1 = pr2$x[,1])
#PC1$Pixelnr <- rownames(PC1)

#TrdRast_model@data <- merge(TrdRast_model@data, PC1, by="Pixelnr")
#names(TrdRast_model)

#plot(Trondheim)
#plot(TrdRast_model, col=(TrdRast_model$PC1 + abs(min(TrdRast_model$PC1))), add=T)


##--- 4.2 CLUSTER ANALYSIS ---####
##----------------------------####
library(vegan)
# Get the needed data: (a copy of the original dataframe, so that we do not mess up too much), then remove cells not within
# municipality borders or only covered by ocean:
TrdRast_clust <- TrdRast_AR5
TrdRast_clust <- TrdRast_clust[!is.na(TrdRast_clust@data$total_area),]
TrdRast_clust <- TrdRast_clust[!(TrdRast_clust@data$Ocean==TrdRast_clust@data$total_area),]

# Make the cluster-dendrogram based on a distance matrix with Bray-Curtis similarity 
clusters <- hclust(vegdist(TrdRast_clust@data[, 3:68], method="bray"))
par(mfrow=c(1,1))
par(mar=c(4.1,4.1,5.1,2.1))
plot(clusters, cex=0.5, main="", xlab="Grid cell number")
abline(h=0.99, col="red", lty=2)
#abline(h=0.9, col="red", lty=2)
#abline(h=0.85, col="red", lty=2)

##--- 4.2.1 ClusterCut 1 ---####
##--------------------------####
# Try and cot the dendrogram into clusters - the height of the cut is made solely with eye for the number of categories
# (thus, relatively arbitrary for now):
clusterCut <- cutree(clusters, h=0.99)
table(clusterCut)

clusterCut <- as.data.frame(clusterCut)
clusterCut$Pixelnr <- rownames(clusterCut)

TrdRast_clust <- merge(TrdRast_clust, clusterCut, by="Pixelnr")

# Make a column with colours according to the cluster:
TrdRast_clust$col.clust <- NA
# Make the vector with colour names (redo this multiple times until the colours are reasonable):
for(i in 1:length(TrdRast_clust@data$clusterCut)){
  TrdRast_clust@data$col.clust[i] <- ifelse(TrdRast_clust@data$clusterCut[i]==1, paste("blue"), ifelse(TrdRast_clust@data$clusterCut[i]==2, paste("hotpink"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==3, paste("red"), ifelse(TrdRast_clust@data$clusterCut[i]==4, paste("orange"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==5, paste("forestgreen"), ifelse(TrdRast_clust@data$clusterCut[i]==6, paste("green"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==7, paste("palegreen"), ifelse(TrdRast_clust@data$clusterCut[i]==8, paste("peachpuff"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==9, paste("goldenrod"), ifelse(TrdRast_clust@data$clusterCut[i]==10, paste("yellow"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==11, paste("maroon4"), ifelse(TrdRast_clust@data$clusterCut[i]==12, paste("cyan"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==13, paste("black"), ifelse(TrdRast_clust@data$clusterCut[i]==14, paste("gray30"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==15, paste("gray50"), ifelse(TrdRast_clust@data$clusterCut[i]==16, paste("gray70"),
                                   ifelse(TrdRast_clust@data$clusterCut[i]==17, paste("gray90"), "white")))))))))))))))))
}

TrdRast_clust@data$col.clust <- as.factor(TrdRast_clust@data$col.clust)

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters",
     col=as.character(TrdRast_clust@data$col.clust))  
par(mar=c(5,1,5,2.5))
image(y=1:18,z=t(1:18), col=c("blue", "hotpink", "red", "orange", "forestgreen", "green", "palegreen", "peachpuff",
                              "goldenrod", "yellow", "maroon4", "cyan", "black", "gray30", "gray50", "gray70",
                              "gray90", "white"), axes=FALSE, main="clusterCut", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# We here have multiple uninformative clusters (singletons) - these are unlikely to be useful in the further analysis.
# Lets "reclassify" those and/or leave them gray in the plot:
table(clusterCut$clusterCut)

# One way to reclassify, is to simply rename them in the dataframe - all groups with 3 or fewer cells::
TrdRast_clust@data[TrdRast_clust@data$clusterCut==9 |
                     TrdRast_clust@data$clusterCut==13 |
                     TrdRast_clust@data$clusterCut==14 |
                     TrdRast_clust@data$clusterCut==15|
                     TrdRast_clust@data$clusterCut==16 |
                     TrdRast_clust@data$clusterCut==17, "clusterCut"] <- 0

# Rename the colours:
TrdRast_clust@data$col.clust <- as.character(TrdRast_clust@data$col.clust)
TrdRast_clust@data[TrdRast_clust@data$clusterCut==0, "col.clust"] <- "gray"

table(TrdRast_clust@data$clusterCut)   # We here have 11 meaningfull clusters (obs! #9 does no longer exist)

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters (cut=0.99)",
     col=as.character(TrdRast_clust@data$col.clust))  
par(mar=c(5,1,5,2.5))
image(y=0:12,z=t(0:12), col=c("gray", "blue", "hotpink", "red", "orange", "forestgreen", "green", "palegreen", "peachpuff",
                              "gray", "yellow", "maroon4", "cyan"), axes=FALSE, main="clusterCut", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# This is potentially a reasonable number of clusters!

##--- 4.2.2 ClusterCut 2 ---####
##--------------------------####
# Try and cut the dendrogram into clusters - the height of the cut is made solely with eye for the number of categories
# (thus, relatively arbitrary for now):
clusterCut2 <- cutree(clusters, h=0.9)
table(clusterCut2)

clusterCut2 <- as.data.frame(clusterCut2)
clusterCut2$Pixelnr <- rownames(clusterCut2)

TrdRast_clust <- merge(TrdRast_clust, clusterCut2, by="Pixelnr")

# Make a column with colours according to the cluster:
TrdRast_clust$col.clust2 <- NA
pal.clust <- rainbow(28)
# Make the vector with colour names (redo this in a later step so that the colours are reasonable):
for(i in 1:length(TrdRast_clust@data$clusterCut2)){
  TrdRast_clust@data$col.clust2[i] <- ifelse(TrdRast_clust@data$clusterCut2[i]==1, paste(pal.clust[1]), ifelse(TrdRast_clust@data$clusterCut2[i]==2, paste(pal.clust[2]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==3, paste(pal.clust[3]), ifelse(TrdRast_clust@data$clusterCut2[i]==4, paste(pal.clust[4]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==5, paste(pal.clust[5]), ifelse(TrdRast_clust@data$clusterCut2[i]==6, paste(pal.clust[6]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==7, paste(pal.clust[7]), ifelse(TrdRast_clust@data$clusterCut2[i]==8, paste(pal.clust[8]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==9, paste(pal.clust[9]), ifelse(TrdRast_clust@data$clusterCut2[i]==10, paste(pal.clust[10]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==11, paste(pal.clust[11]), ifelse(TrdRast_clust@data$clusterCut2[i]==12, paste(pal.clust[12]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==13, paste(pal.clust[13]), ifelse(TrdRast_clust@data$clusterCut2[i]==14, paste(pal.clust[14]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==15, paste(pal.clust[15]), ifelse(TrdRast_clust@data$clusterCut2[i]==16, paste(pal.clust[16]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==17, paste(pal.clust[17]), ifelse(TrdRast_clust@data$clusterCut2[i]==18, paste(pal.clust[18]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==19, paste(pal.clust[19]), ifelse(TrdRast_clust@data$clusterCut2[i]==20, paste(pal.clust[20]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==21, paste(pal.clust[21]), ifelse(TrdRast_clust@data$clusterCut2[i]==22, paste(pal.clust[22]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==23, paste(pal.clust[23]), ifelse(TrdRast_clust@data$clusterCut2[i]==24, paste(pal.clust[24]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==25, paste(pal.clust[25]), ifelse(TrdRast_clust@data$clusterCut2[i]==26, paste(pal.clust[26]),
                                            ifelse(TrdRast_clust@data$clusterCut2[i]==27, paste(pal.clust[27]), ifelse(TrdRast_clust@data$clusterCut2[i]==28, paste(pal.clust[28]),
                                            "white"))))))))))))))))))))))))))))
}

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters (cut=0.9)",
     col=TrdRast_clust@data$col.clust2)  
par(mar=c(5,1,5,2.5))
image(y=1:28,z=t(1:28), col=pal.clust, axes=FALSE, main="clusterCut2", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# We here have multiple uninformative clusters (singletons) - these are unlikely to be useful in the further analysis.
# Lets "reclassify" those and/or leave them gray in the plot:
table(clusterCut2$clusterCut2)

# One way to reclassify, is to simply rename them in the dataframe - all groups with 3 or fewer cells:
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==9 |
                     TrdRast_clust@data$clusterCut2==21 |
                     TrdRast_clust@data$clusterCut2==23 |
                     TrdRast_clust@data$clusterCut2==24|
                     TrdRast_clust@data$clusterCut2==25 |
                     TrdRast_clust@data$clusterCut2==26 |
                     TrdRast_clust@data$clusterCut2==27 |
                     TrdRast_clust@data$clusterCut2==28, "clusterCut2"] <- 0

table(TrdRast_clust@data$clusterCut2)   # We here have 20 meaningful clusters (obs! #9 and #21 no longer exist)

# Rename the colours:
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==0, "col.clust2"] <- "gray"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==1, "col.clust2"] <- "blue"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==2, "col.clust2"] <- "hotpink"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==3, "col.clust2"] <- "firebrick4"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==4, "col.clust2"] <- "orange"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==5, "col.clust2"] <- "palegreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==6, "col.clust2"] <- "seagreen2"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==7, "col.clust2"] <- "forestgreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==8, "col.clust2"] <- "salmon"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==10, "col.clust2"] <- "mediumpurple"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==11, "col.clust2"] <- "limegreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==12, "col.clust2"] <- "yellowgreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==13, "col.clust2"] <- "maroon4"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==14, "col.clust2"] <- "khaki"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==15, "col.clust2"] <- "red"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==16, "col.clust2"] <- "yellow"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==17, "col.clust2"] <- "gold"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==18, "col.clust2"] <- "dodgerblue2"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==19, "col.clust2"] <- "cyan"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==20, "col.clust2"] <- "burlywood"
TrdRast_clust@data[TrdRast_clust@data$clusterCut2==22, "col.clust2"] <- "orange4"

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters (cut=0.9)",
     col=as.character(TrdRast_clust@data$col.clust2))  
par(mar=c(5,1,5,2.5))
image(y=0:22,z=t(0:22), col=c("gray", "blue", "hotpink", "firebrick4", "orange", "palegreen", "seagreen2", "forestgreen",
                              "salmon", "gray", "mediumpurple", "limegreen", "yellowgreen", "maroon4", "khaki", "red", 
                              "yellow", "gold", "dodgerblue2", "cyan", "burlywood", "gray", "orange4"),
      axes=FALSE, main="clusterCut2", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# This is probably too many clusters - potentially be even stricter with the number of cells required to qualify 
# as an important group?


##--- 4.2.3 ClusterCut 3 ---####
##--------------------------####
# Try and cut the dendrogram into clusters - the height of the cut is made solely with eye for the number of categories
# (thus, relatively arbitrary for now):
clusterCut3 <- cutree(clusters, h=0.85)
table(clusterCut3)

clusterCut3 <- as.data.frame(clusterCut3)
clusterCut3$Pixelnr <- rownames(clusterCut3)

TrdRast_clust <- merge(TrdRast_clust, clusterCut3, by="Pixelnr")

# Make a column with colours according to the cluster:
TrdRast_clust$col.clust3 <- NA
pal.clust <- rainbow(34)
# Make the vector with colour names (redo this in a later step so that the colours are reasonable):
for(i in 1:length(TrdRast_clust@data$clusterCut3)){
  TrdRast_clust@data$col.clust3[i] <- ifelse(TrdRast_clust@data$clusterCut3[i]==1, paste(pal.clust[1]), ifelse(TrdRast_clust@data$clusterCut3[i]==2, paste(pal.clust[2]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==3, paste(pal.clust[3]), ifelse(TrdRast_clust@data$clusterCut3[i]==4, paste(pal.clust[4]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==5, paste(pal.clust[5]), ifelse(TrdRast_clust@data$clusterCut3[i]==6, paste(pal.clust[6]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==7, paste(pal.clust[7]), ifelse(TrdRast_clust@data$clusterCut3[i]==8, paste(pal.clust[8]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==9, paste(pal.clust[9]), ifelse(TrdRast_clust@data$clusterCut3[i]==10, paste(pal.clust[10]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==11, paste(pal.clust[11]), ifelse(TrdRast_clust@data$clusterCut3[i]==12, paste(pal.clust[12]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==13, paste(pal.clust[13]), ifelse(TrdRast_clust@data$clusterCut3[i]==14, paste(pal.clust[14]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==15, paste(pal.clust[15]), ifelse(TrdRast_clust@data$clusterCut3[i]==16, paste(pal.clust[16]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==17, paste(pal.clust[17]), ifelse(TrdRast_clust@data$clusterCut3[i]==18, paste(pal.clust[18]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==19, paste(pal.clust[19]), ifelse(TrdRast_clust@data$clusterCut3[i]==20, paste(pal.clust[20]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==21, paste(pal.clust[21]), ifelse(TrdRast_clust@data$clusterCut3[i]==22, paste(pal.clust[22]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==23, paste(pal.clust[23]), ifelse(TrdRast_clust@data$clusterCut3[i]==24, paste(pal.clust[24]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==25, paste(pal.clust[25]), ifelse(TrdRast_clust@data$clusterCut3[i]==26, paste(pal.clust[26]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==27, paste(pal.clust[27]), ifelse(TrdRast_clust@data$clusterCut3[i]==28, paste(pal.clust[28]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==29, paste(pal.clust[29]), ifelse(TrdRast_clust@data$clusterCut3[i]==30, paste(pal.clust[30]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==31, paste(pal.clust[31]), ifelse(TrdRast_clust@data$clusterCut3[i]==32, paste(pal.clust[32]),
                                      ifelse(TrdRast_clust@data$clusterCut3[i]==33, paste(pal.clust[33]), ifelse(TrdRast_clust@data$clusterCut3[i]==34, paste(pal.clust[34]),
                                      "white"))))))))))))))))))))))))))))))))))
}

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters",
     col=TrdRast_clust@data$col.clust3)  
par(mar=c(5,1,5,2.5))
image(y=1:34,z=t(1:34), col=pal.clust, axes=FALSE, main="clusterCut3", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# We here have multiple uninformative clusters (singletons) - these are unlikely to be useful in the further analysis.
# Lets "reclassify" those and/or leave them gray in the plot:
table(clusterCut3$clusterCut3)

# One way to reclassify, is to simply rename them in the dataframe - all groups with 3 or fewer cells:
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==9 |
                     TrdRast_clust@data$clusterCut3==17 |
                     TrdRast_clust@data$clusterCut3==18 |
                     TrdRast_clust@data$clusterCut3==21 |
                     TrdRast_clust@data$clusterCut3==22 |
                     TrdRast_clust@data$clusterCut3==24|
                     TrdRast_clust@data$clusterCut3==25 |
                     TrdRast_clust@data$clusterCut3==27 |
                     TrdRast_clust@data$clusterCut3==28 |
                     TrdRast_clust@data$clusterCut3==29 |
                     TrdRast_clust@data$clusterCut3==31 |
                     TrdRast_clust@data$clusterCut3==32 |
                     TrdRast_clust@data$clusterCut3==33 |
                     TrdRast_clust@data$clusterCut3==34, "clusterCut3"] <- 0

table(TrdRast_clust@data$clusterCut3)   # We here have 20 meaningful clusters - the same as before. Potentially, they are similar?

# Rename the colours:
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==0, "col.clust3"] <- "gray"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==1, "col.clust3"] <- "blue"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==2, "col.clust3"] <- "hotpink"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==3, "col.clust3"] <- "firebrick4"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==4, "col.clust3"] <- "orange"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==5, "col.clust3"] <- "palegreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==6, "col.clust3"] <- "seagreen2"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==7, "col.clust3"] <- "forestgreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==8, "col.clust3"] <- "salmon"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==10, "col.clust3"] <- "mediumpurple"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==11, "col.clust3"] <- "limegreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==12, "col.clust3"] <- "yellowgreen"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==13, "col.clust3"] <- "sienna4"  
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==14, "col.clust3"] <- "maroon4"  
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==15, "col.clust3"] <- "khaki"  
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==16, "col.clust3"] <- "red"   
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==19, "col.clust3"] <- "gold"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==20, "col.clust3"] <- "dodgerblue2"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==23, "col.clust3"] <- "cyan"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==26, "col.clust3"] <- "burlywood"
TrdRast_clust@data[TrdRast_clust@data$clusterCut3==30, "col.clust3"] <- "orange4"

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters (cut=0.85)",
     col=as.character(TrdRast_clust@data$col.clust3))  
par(mar=c(5,1,5,2.5))
image(y=0:22,z=t(0:22), col=c("gray", "blue", "hotpink", "firebrick4", "orange", "palegreen", "seagreen2", "forestgreen",
                              "salmon", "gray", "mediumpurple", "limegreen", "yellowgreen", "sienna4", "maroon4", "khaki", 
                              "red", "gold", "dodgerblue2", "cyan", "burlywood", "gray", "orange4"),
      axes=FALSE, main="clusterCut3", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# This is probably too many clusters - potentially be even stricter with the number of cells required to qualify 
# as an important group?

# For now, I'll continue with the first groups



##--- 4.3 INDICATOR "SPECIES" ANALYSIS ---####
##----------------------------------------####
library(indicspecies)

# We will use this to analyse the groups from the cluster analysis - what habitats are characteristic for each group?
# Instead of "species" we'll use "land cover". Two objects are needed for the analysis: a "community matrix" and a
# vector with the groups. OBS! This takes A LONG TIME, so I started out doing for a test-set:
comm <- TrdRast_clust@data[, c(3:68)]
cluster <- TrdRast_clust@data$clusterCut

comm.test <- comm[seq(1, nrow(comm), 2), ]
cluster.test <- cluster[seq(1, length(cluster), 2)]

# Run the Indicator Value function:
indval <- multipatt(comm, cluster, control = how(nperm = 99))
summary(indval)
summary(indval, indvalcomp=TRUE)
summary(indval, alpha=1)
indval$sign

# The habitats with significant p-values (potential indicators):
indval$sign[indval$sign[,"p.value"]<=0.05,]
        # This brings us down to 30 categories

# Potentially, we can leave out those that are not characteristic in any way?
# The question then is whether those habitats are to be used in the analyses on theit own, or if they should be
# pooled in some way.
# One idea could be to include all of the habitats that "significant" in the models - but that is
# probably still way to many variables.
# Another idea could be to look at the results in more detail, and then potentially pool
# the habitats which are indicators of the same group (or something along those lines)

summary(indval)

# Export as a KML-file:
library(maptools)
library(rgdal)
kmlPolygons(obj=TrdRast_clust[,c(83)], kmlfile=NULL,  name="KML Polygons",
            description="", col=TrdRast_clust@data$col.clust, visibility=1, lwd=1,
            border="white", kmlname="", kmldescription="")

writeOGR(TrdRast_clust[,c(83)], layer = 'TRD_cluster', driver="ESRI Shapefile", dsn = "TRD_cluster")
write.table(TrdRast_clust@data$col.clust, "colour_clust.txt", sep="\t")
# Code for viewing in mapview:
### TRD_clust <- readOGR(dsn=path.expand("W:/Spatial_models/TRD_cluster"), layer="TRD_cluster")
### colour_vec <- as.character(colour_clust$x)
### mapview(TRD_clust, col.regions=as.character(colour_vec), alpha.regions=0.2)

##--- 4.4 SPINE PLOTS ---####
##-----------------------####

# To make spine plots of the the habitats in the categories (to get an overview what is actually important),
# I'll try and make some spineplots. For that, I need to make a dataframe with cluster as column and habitat as rows.
# In each entry is then the average of that habitat type for all cells within that cluster.
# We'll only use the ones that popped out as significant in the ISA:
spine <- matrix(nrow=12, ncol = 30)
colnames(spine) <- row.names(indval$sign[indval$sign[,"p.value"]<=0.05,])
rownames(spine) <- c(1:12)
# Calculate the mean of habitat in the grid cells included in each cluster:
for(i in 1:dim(spine)[1]) {
  for(j in 1:dim(spine)[2]) {
    spine[i,j] = mean(TrdRast_clust@data[TrdRast_clust@data$clusterCut==i, colnames(spine)[j]])
  }
}
spine <- spine[-9,]

layout(t(1:2),widths=c(2,1))
par(mar=c(5.1,1.1,4.1,2.1))
spineplot(spine, main="",
          col = c("hotpink", "lightpink",
                  "darkolivegreen1", "darkolivegreen2", "darkolivegreen3", "darkolivegreen4", "darkolivegreen", "darkgreen",
                  "olivedrab1", "olivedrab2", "olivedrab3", 
                  "chartreuse1", "chartreuse2", "chartreuse3", "chartreuse4", "dodgerblue2",
                  "orange", "orange2", "khaki1",
                  "lightcyan", "cyan", "cyan2", "cyan3", "navy",
                  "darkseagreen", "gray20", "gray90", "gray70", "sandybrown", "peru"),
          xlab="Cluster", ylab="Mean cover of habitat in grid cells",
        xaxlabels = c("1", "2", "3", "4", "5", "6", "7", "8", "10", "11", "12"), yaxlabels = "")

par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("Communications/traffic", "Developed area", "Forest, conif. high, soil",
                          "Forest, conif. imp, shallow soil", "Forest, conif. imp, soil",
                          "Forest, conif. low, organic soil", "Forest, conif. low, soil",
                          "Forest, conif. medium, soil", "Forest, dec. high, soil", "Forest, dec. imp, soil",
                          "Forest, dec. medium. soil", "Forest, mix, high, soil", "Forest, mix, imp, shallow soil",
                          "Forest, mix, imp, soil", "Forest, mix, medium, organic soil", "Freshwater",
                          "Fully cultivated organic soil", "Fully cultivated soil", "Hfgl, soil", "Marsh, conif. imp",
                          "Marsh, dec. imp", "Marsh, mix, low", "Marsh, open, imp", "Ocean",
                          "Ofg, high, soil", "Ofg, imp, artificial", "Ofg, imp, bedrock", "Ofg, imp, boulder",
                          "Ofg, imp, shallow soil", "Ofg, imp, soil"),
       fill=c("hotpink", "lightpink",
              "darkolivegreen1", "darkolivegreen2", "darkolivegreen3", "darkolivegreen4", "darkolivegreen", "darkgreen",
              "olivedrab1", "olivedrab2", "olivedrab3", 
              "chartreuse1", "chartreuse2", "chartreuse3", "chartreuse4", "dodgerblue2",
              "orange", "orange2", "khaki1",
              "lightcyan", "cyan", "cyan2", "cyan3", "navy",
              "darkseagreen", "gray20", "gray90", "gray70", "sandybrown", "peru"), cex=0.75)


# For the modelling: either do it with "cluster" as predictor variables (log(S.chao) ~ cluster) or use the ISA to pick out
# habitat variables - try out both, as I am not quite sure what the best approach would be

##--- 4.5 BETTER PLOTS FOR POTENTIAL PUBLICATION ---####
##--------------------------------------------------####

# Grid cells coloured accourding to cluster:
layout(t(1:2),widths=c(3,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust, main="Clusters (cut=0.99)",
     col=as.character(TrdRast_clust@data$col.clust))  
par(mar=c(5,1,5,10))
image(y=0:11, z=t(0:11), axes=FALSE, main="Cluster", cex.main=0.75,
      col=c("gray", "blue", "hotpink", "red", "orange", "forestgreen", "green", "palegreen", "peachpuff", "yellow", "maroon4", "cyan"))
axis(4,cex.axis=0.8, mgp=c(0,.5,0), at=seq(0, 11.5, by=1), las=2, cex.axis=0.6,
     labels=c("(0) Not grouped", "(1) Coastal", "(2) Urban", "(3) Urban/vegetated/riparian", "(4) Cultivated",
              "(5) Coniferous forest 1", "(6) Coniferous forest 2", "(7) Forest and marsh",
              "(8) Productive coniferous forest", "(10) Open frm ground", "(11) Open firm ground and cultivated land", "(12) Freshwater"))





##--- 4.6 MODELLING WITH CLUSTERS (categorical predictor) ---####
##-----------------------------------------------------------####
# First I'll try out building the models on a categorical predictor variabel: Cluster.
# Add the needed data to the dataframe, adn remove the Ringve outlier (if needed):
#TrdRast_model2 <- TrdRast_model[!TrdRast_model@data$S.chao1_blacks_2013>100,]   # For some od reason, this is removing too many cells?
TrdRast_model2 <- TrdRast_model[!TrdRast_model@data$Pixelnr==1140,]

TrdRast_model2 <- merge(TrdRast_model2, TrdRast_clust@data[,c("Pixelnr", "clusterCut")], by="Pixelnr")
# We can take out the grid cells with category 0 or no category:
TrdRast_model2@data[is.na(TrdRast_model2@data)] <- 0
TrdRast_model2 <- TrdRast_model2[!(TrdRast_model2@data$clusterCut==0),]

# Make the clustering a factor for modelling purposes:
TrdRast_model2@data$clusterCut <- as.factor(TrdRast_model2@data$clusterCut)
levels(TrdRast_model2@data$clusterCut)
# It is important to note here that category 10 (ofg_bedrock) is not present in the data set


##--- 4.6.1 DATA EXPLORATION                       ---####
##----------------------------------------------------####
source("HighstatLibV10.R")

### Outliers:
MyVar <- c("log_chao.reds", "log_chao.blacks", "clusterCut")

Mydotplot(TrdRast_model2@data[,MyVar])

# Make boxplots to assess the spread of the different variables:
par(mfrow=c(1,3))
par(mar=c(0.5,2,3,0.5))
for(i in c(83:85)){
  boxplot(TrdRast_model2@data[!is.na(TrdRast_model2@data[,i]),i], main=colnames(TrdRast_model2@data[i]),
          cex.main=0.7, cex.axis=0.6)
}
par(mfrow=c(1,1))
par(mar=c(4.1,4.1,5.1,2.1))
barplot(table(TrdRast_model2@data$clusterCut))


### Colinearity
pairs(TrdRast_model2@data[, MyVar], 
      lower.panel = panel.cor)

### Relationships
Myxyplot(TrdRast_model2@data, MyVar, "S.chao1_reds_2013", 
         MyYlab = "ESR of redlisted species")

Myxyplot(TrdRast_model2@data, MyVar, "S.chao1_blacks_2013", 
         MyYlab = "ESR of alien species")
# We cannot see any clear relationships from this, unfortunately



##--- 4.6.2 PRELIMINARY MODELLING (NON-SPATIAL)  ---####
##--- 4.6.2.1 Model 1 - threatened species       ---####
##--------------------------------------------------####
M1_clust <- glm(log_chao.reds ~  clusterCut ,
             family = "gaussian",
             data = TrdRast_model2@data)

summary(M1_clust)   # Obs! cluster1 is the inctercept here, so everything is compoared to this one. We need to analyse the potential differences


### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(2434.5 - 1787.1) / 2434.5    # Relatively large, actually

### Model validation 2: Is everything significant?
drop1(M1_clust, test = "Chi")
step(M1_clust) #Backwards selection using AIC - the model is better than no model

# Plot residuals vs fitted values (M1)
F1_clust <- fitted(M1_clust)
E1_clust <- resid(M1_clust, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_clust, 
     y = E1_clust,
     xlab = "Fitted values - M1",
     ylab = "Pearson residuals - M1",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_model2@data$E1_clust <- E1_clust
Myxyplot(TrdRast_model2@data, MyVar, "E1_clust")
TrdRast_model2@data$E1_clust <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E1_clust)

# Compare the predictor variable levels:
library(multcomp)
summary(glht(M1_clust, linfct=mcp(clusterCut="Tukey")))

library(emmeans)
library(multcompView)

M1_emm <- emmeans(M1_clust, "clusterCut")
pairs(M1_emm, type="response")
plot(M1_emm, comparisons=TRUE)
cld(M1_emm)
coef(pairs(M1_emm))

##--- 4.6.2.2 Model 2 - alien species ---####
##---------------------------------------####
M2_clust <- glm(log_chao.blacks ~  clusterCut,
             family = "gaussian",
             data = TrdRast_model2@data)

summary(M2_clust)

### Model validation 1:   (no overdispersion in Gaussian)
# Generalized R^2 = (Null deviance - residual deviance)/ Null deviance
(4677.4 - 3621.1) / 4677.4    # Relatively large, actually?

### Model validation 2: Is everything significant?
drop1(M2_clust, test = "Chi")
step(M2_clust) #Backwards selection using AIC - model is better than no model

# Plot residuals vs fitted values
F2_clust <- fitted(M2_clust)
E2_clust <- resid(M2_clust, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2_clust, 
     y = E2_clust,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)       

# Plot the residuals vs each covariate     
TrdRast_model2@data$E2_clust <- E2_clust
Myxyplot(TrdRast_model2@data, MyVar, "E2_clust")
TrdRast_model2@data$E2_clust <- NULL

# Histogram of the residuals to check is they are Gaussian:
hist(E2_clust)

# Compare the predictor variable levels:
summary(glht(M2_clust, linfct=mcp(clusterCut="Tukey")))

M2_emm <- emmeans(M2_clust, "clusterCut")
pairs(M2_emm, type="response")
plot(M2_emm, comparisons=TRUE)
cld(M2_emm)
coef(pairs(M2_emm))

##--- 4.6.3 SPATIAL AUTOCORRELATION- threatened species ---####
##--- 4.6.3.1 Testing for SAC - Chao1_reds              ---####
##---------------------------------------------------------####
library(spdep)
library(ncf)

# We have already subsetted the dataset, but we need the coordinates.
# OBS! We have to only use the cells for which have data on all variables - otherwise we get errors:
xy_clust <- coordinates(TrdRast_model2[TrdRast_model2@data$Pixelnr %in% M1_clust$data[,"Pixelnr"],]) 
                                          # OBS! Not all of the pixels are apparently included in the model - therefore, we have
                                          # to make this shorter to only include the pixels included in the model itself

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model2$log_chao.reds) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model2, col=(TrdRast_model2$log_chao.reds + 6.907755))   # The colours cannot be negative - add the numerical value of the lowest
par(mar=c(5,1,5,2.5))
image(y=(-7):5,z=t((-7):5), col=rev(col.heat), axes=FALSE, main="log(threatened\n+0.01)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog1_clust <- correlog(xy_clust[,1], xy_clust[,2], residuals(M1_clust), na.rm = T, increment = 1, resamp = 0)

correlog1_clust2 <- correlog(xy_clust[,1], xy_clust[,2], residuals(gls.exp_clust), na.rm = T, increment = 1, resamp = 0)


# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog1_clust2$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M1_clust))/2+1.5], pch=19,
     cex=abs(resid(M1_clust))/max(resid(M1_clust))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(gls.exp_clust))/2+1.5], pch=19,
     cex=abs(resid(gls.exp_clust))/max(resid(gls.exp_clust))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")


# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
M1_clust.nb <- dnearneigh(as.matrix(xy_clust[,1:2]), 0, 1500) # Find the neighbors - give lower and upper distance class here
# OBS! The classes are in euclidian distance (m), thus we need a reasonable distance to define
# a neighbouring grid cell. Here, I have chosen to use 1.5 km 
# to make it reasonable! Otherwise, use the following list:
# w_pp <- poly2nb(TrdRast_pp, row.names=TrdRast_pp$Pixelnr)     # Find the neighbors
# ww_pp <-  nb2listw(w_pp, style='B', zero.policy = TRUE)       # Make it a "listw" spatial object
M1_clust.listw <- nb2listw(M1_clust.nb, zero.policy = T)   
# Turns neighbourhood object into a weighted list
# this next step takes often several minutes to run:
GlobMT1_clust <- moran.test(residuals(M1_clust), listw=M1_clust.listw, zero.policy = T)
GlobMT1_clust

# We seemingly have SAC in these model residuals.
# Lets have a look at whether there is SAC in the data itself rather than only the residuals:
moran(TrdRast_model2$log_chao.reds, M1_clust.listw, n=length(M1_clust.listw$neighbours),
      S0=Szero(M1_clust.listw), zero.policy = TRUE)    # Calculate Moran's I

# Test for significance:
moran.test(TrdRast_model2$log_chao.reds, M1_clust.listw, randomisation=FALSE,
           alternative = "two.sided", zero.policy = TRUE)     # Using linear regression based logic and assumptions

MC_clust <- moran.mc(TrdRast_model2$log_chao.reds, M1_clust.listw,
                  zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_clust, main=NULL)     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_clust$statistic, lty=2, col="red")

# Make a correlogram
sp.corr_clust2 <- sp.correlogram(M1_clust.nb, TrdRast_model2$log_chao.reds, order=8, method="I", zero.policy = TRUE)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(sp.corr_clust)

# We thus have SAC in the data and in the models


##--- 4.6.3.2 Testing for SAC - Chao1_blacks             ---####
##----------------------------------------------------------####

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_model2$log_chao.blacks) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_model2, col=(TrdRast_model2$log_chao.blacks + 6.907755)) # The colours cannot be negative - add the numerical value of the lowest
par(mar=c(5,1,5,2.5))
image(y=-7:6,z=t(-7:6), col=rev(col.heat), axes=FALSE, main="log(alien\n+00.1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog2_clust <- correlog(xy_clust[,1], xy_clust[,2], residuals(M2_clust), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2_clust$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M2_clust))/2+1.5], pch=19,
     cex=abs(resid(M2_clust))/max(resid(M2_clust))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT2_clust <- moran.test(residuals(M2_clust), listw=M1_clust.listw, zero.policy = T)
GlobMT2_clust

# No SAC in this model either


##--- 4.6.4 DEALING WITH SAC ---####
##------------------------------####
##--- 4.6.4.1 Chao1_reds     ---####
##------------------------------####

# To deal with the spatial autocorrelation, we can use a GLS with an added correlation structure - we thus also
# need to figure out what kind of correlation structure is apppriate.
# The steps here are from the Zuur-book and the Dorman-paper:
require(nlme)
summary(gls.exp_clust <- gls(log_chao.reds ~ clusterCut,
                          data=TrdRast_model2@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))

AIC(M1_clust, gls.exp_clust)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp_ML_clust <- gls(log_chao.reds ~ clusterCut,
                             data=TrdRast_model2@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

drop1(gls.exp_ML_clust, test = "Chi")
library(MASS)
stepAIC(gls.exp_ML_clust)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the model is better than no model
# This is similar to the optimal model for the non-spatial approach

# Comparing the categories:
M1.sp_emm <- emmeans(gls.exp_clust, "clusterCut")
pairs(M1.sp_emm, type="")
plot(M1.sp_emm, comparisons=TRUE)
cld(M1.sp_emm)
coef(pairs(M1.sp_emm))

##--- 4.6.4.2 Chao1_blacks    ---####
##-------------------------------####

# To deal with the spatial autocorrelation, we can use a GLS with an added correlation structure - we thus also
# need to figure out what kind of correlation structure is apppriate.
# The steps here are from the Zuur-book and the Dorman-paper:
summary(gls.exp.b_clust <- gls(log_chao.blacks ~ clusterCut,
                            data=TrdRast_model2@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))

AIC(M2_clust, gls.exp.b_clust)
# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.exp.b_ML_clust <- gls(log_chao.blacks ~ clusterCut,
                               data=TrdRast_model2@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

drop1(gls.exp.b_ML_clust, test = "Chi")
stepAIC(gls.exp.b_ML_clust)              # For unknown reasons, the standard 'step()' doesn't work - this one does

# According to the SAC-function, the model is better than no model
# This is somewhat similar to the optimal model for the non-spatial approach!

# Comparing the categories:
M2.sp_emm <- emmeans(gls.exp.b_clust, "clusterCut")
pairs(M2.sp_emm, type="")
plot(M2.sp_emm, comparisons=TRUE)
cld(M2.sp_emm)


##--- 4.6.5 Boxplots of the model data and comparisons ---####
##--------------------------------------------------------####
# Threatened
par(mfrow=c(2,1))
par(mar=c(4.1,4.1,1,1))
boxplot(log_chao.reds~clusterCut, data=TrdRast_model2,
        col=c("blue", "hotpink", "red", "orange", "forestgreen", "green", "palegreen", "peachpuff", "maroon4", "cyan"),
        xlab="Cluster", ylab="log(ESR), threatened species")
text (c(1:10), y = 3.8, labels = c("A", "A", "AB", "A", "C", "BC", "ABC", "ABC", "ABC", "ABC"))

# Alien
par(mar=c(4.1,4.1,1,1))
boxplot(log_chao.blacks~clusterCut, data=TrdRast_model2,
        col=c("blue", "hotpink", "red", "orange", "forestgreen", "green", "palegreen", "peachpuff", "maroon4", "cyan"),
        xlab="Cluster", ylab="log(ESR), alien species")
text (c(1:10), y = 3.8, labels = c("AB", "A", "ABC", "BC", "C", "BC", "ABC", "ABC", "ABC", "ABC"))


##--- 4.6.6 Map showing richness estimatings based on the spatial models (threatened and alien species) ---####
##--- 4.6.6.1 Making predictions from models                                                            ---####
##---------------------------------------------------------------------------------------------------------####

# Now we want to try and make predictions on the number of either threatened or alien species based on
# the spatial models.
# For that, we need a dataset with the variables included in the model(s): the landcover data (percentage of cover)
# for all of Trondheim. 

# Creating a new dataframe with joined categories (first summing, then calculating relative area afterwards)
data_predict_clust <- TrdRast_clust[, c(1, 83)]
# Remove the grid cells with categories which cannot be used i the model (0 and 10)
data_predict_clust <- data_predict_clust[!data_predict_clust@data$clusterCut==0,]
data_predict_clust <- data_predict_clust[!data_predict_clust@data$clusterCut==10,]
data_predict_clust@data$clusterCut <- as.factor(data_predict_clust@data$clusterCut)

### Make the predictions for threatened and alien species:
data_predict_clust$predict_reds <- predict(gls.exp_clust, newdata=data_predict_clust)
data_predict_clust$predict_blacks <- predict(gls.exp.b_clust, newdata=data_predict_clust)

range(data_predict_clust$predict_reds)
range(data_predict_clust$predict_blacks)

# These predictions seem more reasonable than some of the previous ones (still not optimal though)


##--- 4.6.6.2 Make the vectors with colour names           ---####
##------------------------------------------------------------####
# Get the numbers to base the colour on:
col_ESR_red_vec <- c(TrdRast_model2@data$log_chao.reds)
col_ESR_black_vec <- c(TrdRast_model2@data$log_chao.blacks)
col_pred_red_vec <- c(data_predict@data$predict_reds)
col_pred_black_vec <- c(data_predict@data$predict_blacks)

# The vectors with colour names needs to have the same range, if the maps are to be comparable
# Chao1:
col_ESR_red <- rep(0, length(col_ESR_red_vec))
for(i in 1:length(col_ESR_red_vec)){
  col_ESR_red[i] <- ifelse(col_ESR_red_vec[i]>-7 & col_ESR_red_vec[i]<=-5, paste("#7F00FFFF"),
                               ifelse(col_ESR_red_vec[i]>-5 & col_ESR_red_vec[i]<=-4, paste("#001AFFFF"),
                               ifelse(col_ESR_red_vec[i]>-4 & col_ESR_red_vec[i]<=-3, paste("#00B3FFFF"),
                               ifelse(col_ESR_red_vec[i]>-3 & col_ESR_red_vec[i]<=-2, paste("#00FFFFFF"),
                               ifelse(col_ESR_red_vec[i]>-2 & col_ESR_red_vec[i]<=-1, paste("#00FF19FF"),
                               ifelse(col_ESR_red_vec[i]>-1 & col_ESR_red_vec[i]<=0, paste("#80FF00FF"),
                               ifelse(col_ESR_red_vec[i]>0 & col_ESR_red_vec[i]<=1, paste("#FFE500FF"),
                               ifelse(col_ESR_red_vec[i]>1 & col_ESR_red_vec[i]<=2, paste("#FF9900FF"),
                               ifelse(col_ESR_red_vec[i]>2 & col_ESR_red_vec[i]<=3, paste("#FF4D00FF"),
                               ifelse(col_ESR_red_vec[i]>3 & col_ESR_red_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

col_ESR_black <- rep(0, length(col_ESR_black_vec))
for(i in 1:length(col_ESR_black_vec)){
  col_ESR_black[i] <- ifelse(col_ESR_black_vec[i]>-7 & col_ESR_black_vec[i]<=-5, paste("#7F00FFFF"),
                             ifelse(col_ESR_black_vec[i]>-5 & col_ESR_black_vec[i]<=-4, paste("#001AFFFF"),
                             ifelse(col_ESR_black_vec[i]>-4 & col_ESR_black_vec[i]<=-3, paste("#00B3FFFF"),
                             ifelse(col_ESR_black_vec[i]>-3 & col_ESR_black_vec[i]<=-2, paste("#00FFFFFF"),
                             ifelse(col_ESR_black_vec[i]>-2 & col_ESR_black_vec[i]<=-1, paste("#00FF19FF"),
                             ifelse(col_ESR_black_vec[i]>-1 & col_ESR_black_vec[i]<=0, paste("#80FF00FF"),
                             ifelse(col_ESR_black_vec[i]>0 & col_ESR_black_vec[i]<=1, paste("#FFE500FF"),
                             ifelse(col_ESR_black_vec[i]>1 & col_ESR_black_vec[i]<=2, paste("#FF9900FF"),
                             ifelse(col_ESR_black_vec[i]>2 & col_ESR_black_vec[i]<=3, paste("#FF4D00FF"),
                             ifelse(col_ESR_black_vec[i]>3 & col_ESR_black_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

# Predicted:
col_pred_red <- rep(0, length(col_pred_red_vec))
for(i in 1:length(col_pred_red_vec)){
  col_pred_red[i] <- ifelse(col_pred_red_vec[i]>-7 & col_pred_red_vec[i]<=-5, paste("#7F00FFFF"),
                          ifelse(col_pred_red_vec[i]>-5 & col_pred_red_vec[i]<=-4, paste("#001AFFFF"),
                          ifelse(col_pred_red_vec[i]>-4 & col_pred_red_vec[i]<=-3, paste("#00B3FFFF"),
                          ifelse(col_pred_red_vec[i]>-3 & col_pred_red_vec[i]<=-2, paste("#00FFFFFF"),
                          ifelse(col_pred_red_vec[i]>-2 & col_pred_red_vec[i]<=-1, paste("#00FF19FF"),
                          ifelse(col_pred_red_vec[i]>-1 & col_pred_red_vec[i]<=0, paste("#80FF00FF"),
                          ifelse(col_pred_red_vec[i]>0 & col_pred_red_vec[i]<=1, paste("#FFE500FF"),
                          ifelse(col_pred_red_vec[i]>1 & col_pred_red_vec[i]<=2, paste("#FF9900FF"),
                          ifelse(col_pred_red_vec[i]>2 & col_pred_red_vec[i]<=3, paste("#FF4D00FF"),
                          ifelse(col_pred_red_vec[i]>3 & col_pred_red_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

col_pred_black <- rep(0, length(col_pred_black_vec))
for(i in 1:length(col_pred_black_vec)){
  col_pred_black[i] <- ifelse(col_pred_black_vec[i]>-7 & col_pred_black_vec[i]<=-5, paste("#7F00FFFF"),
                            ifelse(col_pred_black_vec[i]>-5 & col_pred_black_vec[i]<=-4, paste("#001AFFFF"),
                            ifelse(col_pred_black_vec[i]>-4 & col_pred_black_vec[i]<=-3, paste("#00B3FFFF"),
                            ifelse(col_pred_black_vec[i]>-3 & col_pred_black_vec[i]<=-2, paste("#00FFFFFF"),
                            ifelse(col_pred_black_vec[i]>-2 & col_pred_black_vec[i]<=-1, paste("#00FF19FF"),
                            ifelse(col_pred_black_vec[i]>-1 & col_pred_black_vec[i]<=0, paste("#80FF00FF"),
                            ifelse(col_pred_black_vec[i]>0 & col_pred_black_vec[i]<=1, paste("#FFE500FF"),
                            ifelse(col_pred_black_vec[i]>1 & col_pred_black_vec[i]<=2, paste("#FF9900FF"),
                            ifelse(col_pred_black_vec[i]>2 & col_pred_black_vec[i]<=3, paste("#FF4D00FF"),
                            ifelse(col_pred_black_vec[i]>3 & col_pred_black_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}


##--- 2.6.5.2 Make the maps                                ---####
##------------------------------------------------------------####
#par(mfrow=c(2,2))
#par(mar=c(0.5,0.5,6,1))

layout(rbind(c(1,2,3), c(4,5,3)), widths=c(4,4,1))
par(mar=c(0.5,0.5,6,0.5))

# Threatened species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of threatened species) \n in 500m x 500m cell")
plot(TrdRast_model2[, "log_chao.reds"],
     col=col_ESR_red, border=col_ESR_red, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell")
plot(data_predict,
     col=col_pred_red, border=col_pred_red, add=T, cex.main=0.75)

plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("-7-(-5)", "-5-(-4)", "-4-(-3)", "-3-(-2)", "-2-(-1)",
                       "-1-0", "0-1", "1-2", "2-3", "3-4.5"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=1)


# Alien species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of alien species) \nin 500m x 500m cell")
plot(TrdRast_model2[, "log_chao.blacks"],
     col=col_ESR_black, border=col_ESR_black, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell")
plot(data_predict,
     col=col_pred_black, border=col_pred_black, add=T, cex.main=0.75)

