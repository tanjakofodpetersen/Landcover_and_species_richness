##############################################################
###  COMMUNITY MATRICES FOR SPECIES RICHNESS CALCULATIONS  ###
##############################################################

# This is done for the data from after 2013 and a coordinate undertainty of less than 354 m

##--- 1.1 CREATE THE NEEDED OBJECTS, AND MAKE THE COMMUNITY MATRIX  ---####
##--- 1.1.1 ALL DATA                                                ---####
##---------------------------------------------------------------------####

## Here: made for the GBIF-data with an uncertainty <354 m collected from 2013 and later

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
GBIF.trd_2013@data$fPixelnr <- factor(GBIF.trd_2013@data$Pixelnr) 
GBIF.trd_2013@data <- droplevels(GBIF.trd_2013@data)

rar.data_2013 <- as.data.frame(GBIF.trd_2013@data[,c("species", "fPixelnr")])
rar.data_2013 <- droplevels(rar.data_2013)

# Create empty matrix
est_2013 <- matrix(data=NA, ncol=nlevels(rar.data_2013$species), nrow=nlevels(rar.data_2013$fPixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est_2013) <- levels(rar.data_2013$species)
rownames(est_2013) <- levels(rar.data_2013$fPixelnr)

# Make a tally of the rar.data
tallied_2013 <- rar.data_2013 %>%
  group_by(species, fPixelnr) %>%
  tally()

##--- 1.1.2 RED- AND BLACKLISTED ---####
##----------------------------------####
# Red and blacklisted observations only:
reds_2013@data$fPixelnr <- factor(reds_2013@data$Pixelnr) 
reds_2013@data <- droplevels(reds_2013@data)
blacks_2013@data$fPixelnr <- factor(blacks_2013@data$Pixelnr) 
blacks_2013@data <- droplevels(blacks_2013@data)

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
rar.reds_2013 <- as.data.frame(reds_2013@data[,c("species", "fPixelnr")])
rar.reds_2013 <- droplevels(rar.reds_2013)
rar.blacks_2013 <- as.data.frame(blacks_2013@data[,c("species", "fPixelnr")])
rar.blacks_2013 <- droplevels(rar.blacks_2013)

# Create empty matrix
est.reds_2013 <- matrix(data=NA, ncol=nlevels(rar.reds_2013$species), nrow=nlevels(rar.reds_2013$fPixelnr))
est.blacks_2013 <- matrix(data=NA, ncol=nlevels(rar.blacks_2013$species), nrow=nlevels(rar.blacks_2013$fPixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est.reds_2013) <- levels(rar.reds_2013$species)
rownames(est.reds_2013) <- levels(rar.reds_2013$fPixelnr)
colnames(est.blacks_2013) <- levels(rar.blacks_2013$species)
rownames(est.blacks_2013) <- levels(rar.blacks_2013$fPixelnr)

# Make a tally of the rar.data
tallied.reds_2013 <- rar.reds_2013 %>%
  group_by(species, fPixelnr) %>%
  tally()

tallied.blacks_2013 <- rar.blacks_2013 %>%
  group_by(species, fPixelnr) %>%
  tally()


##--- 1.2 RUN THE FUNCTION AND FILL IN THE COMMUNITY MATRIX  ---####
##--------------------------------------------------------------####

## ALL OBSERVATIONS:
## The full matrix and dataframe are too much to handle for the programme (it either gets stuck or takes way too long.
## Therefore, the est-matrix are split up)
est1_2013 <- est_2013[,c(1:600)]      
est2_2013 <- est_2013[,c(601:1200)]    
est3_2013 <- est_2013[,c(1201:1800)]    
est4_2013 <- est_2013[,c(1801:2400)]   
est5_2013 <- est_2013[,c(2401:3000)]  
est6_2013 <- est_2013[,c(3001:3097)]


# Run the function(s)
# 1.
for(r in 1:dim(est1_2013)[1]){
  for(c in 1:dim(est1_2013)[2]){
    est1_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est1_2013)}
}

# 2.
for(r in 1:dim(est2_2013)[1]){
  for(c in 1:dim(est2_2013)[2]){
    est2_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est2_2013)}
}

# 3.
for(r in 1:dim(est3_2013)[1]){
  for(c in 1:dim(est3_2013)[2]){
    est3_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est3_2013)}
}

# 4.
for(r in 1:dim(est4_2013)[1]){
  for(c in 1:dim(est4_2013)[2]){
    est4_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est4_2013)}
}

# 5.
for(r in 1:dim(est5_2013)[1]){
  for(c in 1:dim(est5_2013)[2]){
    est5_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est5_2013)}
}

# 6.
for(r in 1:dim(est6_2013)[1]){
  for(c in 1:dim(est6_2013)[2]){
    est6_2013[r,c]=ntally(i=r, j=c, Tally=tallied_2013, Com.matrix=est6_2013)}
}


# Combine all the small matrices to one large community matrix
est.all_2013 <- cbind(est1_2013, est2_2013, est3_2013, est4_2013, est5_2013, est6_2013) 


# Save the results, so you can reload them without running the function again
write.csv(est.all_2013, file="Comm_matrix_GBIF_2013_August2018.csv")
        ## Try to reload the dataframe to check that everything is okay:
        # est.all <- read.csv("Comm_matrix_GBIF_August2018.csv", row.names=1)

# Remove all the smaller est-files
#rm(est_2013)
#rm(est1_2013)
#rm(est2_2013)
#rm(est3_2013)
#rm(est4_2013)
#rm(est5_2013)
#rm(est6_2013)

### REDLISTED
for(r in 1:dim(est.reds_2013)[1]){
  for(c in 1:dim(est.reds_2013)[2]){
    est.reds_2013[r,c]=ntally(i=r, j=c, Tally=tallied.reds_2013, Com.matrix=est.reds_2013)}
}

# Save the results, so you can reload them without running the function again
write.csv(est.reds_2013, file="Comm_matrix_GBIF_2013_August2018_redlisted.csv")
        # est.reds <- read.csv("Comm_matrix_GBIF_August2018_redlisted.csv", header=TRUE, row.names = 1)

### BLACKLISTED
for(r in 1:dim(est.blacks_2013)[1]){
  for(c in 1:dim(est.blacks_2013)[2]){
    est.blacks_2013[r,c]=ntally(i=r, j=c, Tally=tallied.blacks_2013, Com.matrix=est.blacks_2013)}
}

# Save the results, so you can reload them without running the function again
write.csv(est.blacks_2013, file="Comm_matrix_GBIF_August2018_2013_blacklisted.csv")
        # est.blacks <- read.csv("Comm_matrix_GBIF_August2018_blacklisted.csv", header=TRUE, row.names = 1)

# Check that the community matrices seems reasonable:
dim(est.all_2013)
hist(rowSums(est.all_2013))
hist(log(rowSums(est.all_2013)))
View(as.data.frame(rowSums(est.all_2013)))

dim(est.reds_2013)
hist(log(rowSums(est.reds_2013)))
hist(rowSums(est.reds_2013))
View(as.data.frame(rowSums(est.reds_2013)))

dim(est.blacks_2013)
hist(log(rowSums(est.blacks_2013)))
hist(rowSums(est.blacks_2013))
View(as.data.frame(rowSums(est.blacks_2013)))


##--- 2. CALCULATE THE ESTIMATED SPECIES RICHNESS FOR EACH OF THE GRID CELLS ---####
##------------------------------------------------------------------------------####
# Estimate species richness for each site/grid cell, and save the results. For cells with only one
# observation, the returned value will  be NA
library(vegan)
est.sp_2013 <- estimateR(est.all_2013)
est.sp.reds_2013 <- estimateR(est.reds_2013)
est.sp.blacks_2013 <- estimateR(est.blacks_2013)

# Rotate it and make it a dataframe
est.sp_2013 <- as.data.frame(t(est.sp_2013))
est.sp.reds_2013 <- as.data.frame(t(est.sp.reds_2013))
est.sp.blacks_2013 <- as.data.frame(t(est.sp.blacks_2013))

# Add the rownames (Pixelnr) as a variable
est.sp_2013 <- tibble::rownames_to_column(est.sp_2013, var="Pixelnr")
est.sp.reds_2013 <- tibble::rownames_to_column(est.sp.reds_2013, var="Pixelnr")
est.sp.blacks_2013 <- tibble::rownames_to_column(est.sp.blacks_2013, var="Pixelnr")

# Add these calculations (Species richness) to the SpatialPointsDataframes (later: also to the TrdRast SpatialPolygonsDataFrame)
GBIF.trd_2013 <- merge(GBIF.trd_2013, est.sp_2013, all=TRUE)
reds_2013 <- merge(reds_2013, est.sp.reds_2013, all=TRUE)
blacks_2013 <- merge(blacks_2013, est.sp.blacks_2013, all=TRUE)

# Also add the the number of estimated redlisted species to the TrdRast-data for plotting
# First, retrieve the needed data for each of the raster cells 
GBIF_sp_2013 <- unique(GBIF.trd_2013@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(GBIF_sp_2013) <- c("Pixelnr", "S.obs_2013", "S.chao1_2013", "se.chao1_2013")

reds_sp_2013 <- unique(reds_2013@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(reds_sp_2013) <- c("Pixelnr", "S.obs_reds_2013", "S.chao1_reds_2013", "se.chao1_reds_2013")

blacks_sp_2013 <- unique(blacks_2013@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(blacks_sp_2013) <- c("Pixelnr", "S.obs_blacks_2013", "S.chao1_blacks_2013", "se.chao1_blacks_2013")

# Merge both into the TrdRast_AR5
TrdRast_AR5 <- merge(TrdRast_AR5, GBIF_sp_2013, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5)

TrdRast_AR5 <- merge(TrdRast_AR5, reds_sp_2013, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5)

TrdRast_AR5 <- merge(TrdRast_AR5, blacks_sp_2013, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5)

rm(GBIF_sp_2013)
rm(reds_sp_2013)
rm(blacks_sp_2013)



##--- 3. NUMBER OF SAMPLES/RECORDS IN EACH CELL ---####
##-------------------------------------------------####
# Add number of observations to the dataframes (these are picked up from the "Rasterization'-script).
# This needs to be done for all records, red- and blacklisted.
# First, add the pixelnumber as a column in TrdPointsRaster, and then merge them:
TrdPointsRaster@data <- tibble::rownames_to_column(TrdPointsRaster@data)
TrdRedRaster@data <- tibble::rownames_to_column(TrdRedRaster@data)
TrdBlackRaster@data <- tibble::rownames_to_column(TrdBlackRaster@data)

GBIF.trd_2013@data <- merge(GBIF.trd_2013@data, TrdPointsRaster@data, by.x="Pixelnr", by.y="rowname", all=TRUE)
names(GBIF.trd_2013)[names(GBIF.trd_2013) == 'Npoints'] <- 'Ntotal'
reds_2013@data <- merge(reds_2013@data, TrdRedRaster@data, by.x="Pixelnr", by.y="rowname", all=TRUE)
blacks_2013@data <- merge(blacks_2013@data, TrdBlackRaster@data, by.x="Pixelnr", by.y="rowname", all=TRUE)


# Also add these to the TrdRast_AR5 dataframe for plotting - first, retrieve the needed data, and remove
# cells not within Trondheim borders:
cells_in_trondheim <- TrdRast_AR5@data[!is.na(TrdRast_AR5@data$Communications_traffic), "Pixelnr"]

GBIF_sp_2013.2 <- unique(GBIF.trd_2013@data[,c("Pixelnr", "Ntotal")])
      GBIF_sp_2013.2 <- GBIF_sp_2013.2 %>%
        filter(Pixelnr %in% cells_in_trondheim)
reds_sp_2013.2 <- unique(reds_2013@data[,c("Pixelnr", "Nred")])
      reds_sp_2013.2 <- reds_sp_2013.2 %>%
        filter(Pixelnr %in% cells_in_trondheim)
blacks_sp_2013.2 <- unique(blacks_2013@data[,c("Pixelnr", "Nblack")])
      blacks_sp_2013.2 <- blacks_sp_2013.2 %>%
        filter(Pixelnr %in% cells_in_trondheim)

# Merge both into the TrdRast_AR5
TrdRast_AR5 <- merge(TrdRast_AR5, GBIF_sp_2013.2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, reds_sp_2013.2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, blacks_sp_2013.2, by="Pixelnr", all=TRUE)

rm(GBIF_sp_2013.2)
rm(reds_sp_2013.2)
rm(blacks_sp_2013.2)


##--- 4. HAVE A LOOK AT SOME MAPS ---####
##------------------------------------####
# All observations, reds and blacks - make rasters and maps
raster_2013 <- stack(rasterize(TrdRast_AR5, ras,
                               field = "S.obs_2013"),
                     rasterize(TrdRast_AR5, ras,
                               field = "S.chao1_2013"),
                     rasterize(TrdRast_AR5, ras,
                               fiel = "Ntotal"),
                     rasterize(TrdRast_AR5, ras,
                               field = "S.obs_reds_2013"),
                     rasterize(TrdRast_AR5, ras,
                               field = "S.chao1_reds_2013"),
                     rasterize(TrdRast_AR5, ras,
                               fiel = "Nred"),
                     rasterize(TrdRast_AR5, ras,
                               field = "S.obs_blacks_2013"),
                     rasterize(TrdRast_AR5, ras,
                               field = "S.chao1_blacks_2013"),
                     rasterize(TrdRast_AR5, ras,
                               fiel = "Nblack"))
names(raster_2013) <- c("S.obs_2013", "S.chao1_2013", "Ntotal",
                        "S.obs_reds_2013", "S.chao1_reds_2013", "Nred",
                        "S.obs_blacks_2013", "S.chao1_blacks_2013", "Nblack")

crs(raster_2013) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +vunits=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(raster_2013)
#plot(Trondheim, add=TRUE)

# We can plot it with more meaningfull/comparable axes
par(mfrow=c(3,3))
par(mar=c(0.5,0.5, 2, 3))
plot(raster_2013$S.obs_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed species richness", zlim=c(0,250), cex.main=0.75)
plot(raster_2013$S.chao1_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated species richness", zlim=c(0,5800), cex.main=0.75)
plot(raster_2013$Ntotal, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of records", zlim=c(0,29100), cex.main=0.75)

plot(raster_2013$S.obs_reds_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed threatened species richness", zlim=c(0,250), cex.main=0.75)
plot(raster_2013$S.chao1_reds_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated threatened species richness", zlim=c(0,250), cex.main=0.75)
plot(raster_2013$Nred, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of threatened records", zlim=c(0,6500), cex.main=0.75)

plot(raster_2013$S.obs_blacks_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Observed alien species richness", zlim=c(0,250), cex.main=0.75)
plot(raster_2013$S.chao1_blacks_2013, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Estimated alien species richness", zlim=c(0,250), cex.main=0.75)
plot(raster_2013$Nblack, bty="n", box=FALSE, xaxt="n", yaxt="n",
     main="Number of alien records", zlim=c(0,6500), cex.main=0.75)





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
barplot(table(TrdRast_AR5@data$Ntotal), ylim=c(0,50), xlab="Total number of records in cell", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nred), ylim=c(0,50), xlab="Number of redlisted records in cell", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nblack), ylim=c(0,50), xlab="Number of alien records in cell", ylab="Frequency")

# One rule for pruning could be only using cells with a minimum number of records, e.g. 10, 15, 20 etc.
# Using all cells in the grid gives us 2249 cells.
# Number of cells with any number of records in (thus, no zero or NA):
NROW(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>0 &
                        !is.na(TrdRast_AR5@data$Communications_traffic),])   # 1044

# Number of cells with 10 or more records:
NROW(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>=10 &
                        !is.na(TrdRast_AR5@data$Communications_traffic),])   # 713
# Number of cells with 15 or more records:
NROW(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>=15 &
                        !is.na(TrdRast_AR5@data$Communications_traffic),])   # 644
# Number of cells with 20 or more records:
NROW(TrdRast_AR5@data[TrdRast_AR5@data$Ntotal>=20 &
                        !is.na(TrdRast_AR5@data$Communications_traffic),])   # 580


# Another criterion for pruning is coefficient of variation (the SE of the estimated number of species/ESR)
# This should not be too large, e.g. less than 0.2 census Ballesteros-Meija et al. (2013).
# First, we add that calculation to the dataframe, then look at the results:
TrdRast_AR5@data$CoV_2013 <- TrdRast_AR5@data$se.chao1_2013/TrdRast_AR5@data$S.chao1_2013

range(TrdRast_AR5@data[!is.na(TrdRast_AR5@data$CoV_2013), "CoV_2013"])
hist(TrdRast_AR5@data$CoV_2013)
View(TrdRast_AR5@data[!is.na(TrdRast_AR5@data$CoV_2013), c(1, 69:71, 78, 81)])

# Number of cells with a CoV < 0.2 (less than or equal to):
NROW(TrdRast_AR5@data[TrdRast_AR5@data$CoV_2013<=0.2 &
                        !is.na(TrdRast_AR5@data$CoV_2013),])   # 349
# Number of cells with a CoV < 0.25 (less than or equal to):
NROW(TrdRast_AR5@data[TrdRast_AR5@data$CoV_2013<=0.25 &
                        !is.na(TrdRast_AR5@data$CoV_2013),])   # 451
# Number of cells with a CoV < 0.3 (less than or equal to):
NROW(TrdRast_AR5@data[TrdRast_AR5@data$CoV_2013<=0.3 &
                        !is.na(TrdRast_AR5@data$CoV_2013),])   # 529

### In the furster analyses, we will continue with some minorly "pruned" data - in this case,
# we'll go for cells with more than 10 records and a CoV <0.25. The total number of cells
# used for analysis are then:
NROW(TrdRast_AR5@data[TrdRast_AR5@data$CoV_2013<=0.25 &
                        TrdRast_AR5@data$Ntotal>=10 &
                        !is.na(TrdRast_AR5@data$Communications_traffic) &
                        !is.na(TrdRast_AR5@data$CoV_2013),])   # 308

# The Pixelnr's are: 
cells_analyses <- TrdRast_AR5@data[TrdRast_AR5@data$CoV_2013<=0.25 &
                                     TrdRast_AR5@data$Ntotal>=10 &
                                     !is.na(TrdRast_AR5@data$Communications_traffic) &
                                     !is.na(TrdRast_AR5@data$CoV_2013), "Pixelnr"]
TrdRast_analyses <- TrdRast_AR5[TrdRast_AR5@data$CoV_2013<=0.25 &
                                       TrdRast_AR5@data$Ntotal>=10 &
                                       !is.na(TrdRast_AR5@data$Communications_traffic) &
                                       !is.na(TrdRast_AR5@data$CoV_2013), ]

# Have a look at a map:
AR5map(Trondheim, AR5_crop, "")
plot(Trondheim, add=TRUE)

plot(TrdRast_AR5, border="gray", add=T)   # The entire grid
plot(TrdRast_AR5[TrdRast_AR5@data$Ntotal>0 &
                   !is.na(TrdRast_AR5@data$Ntotal),],
     col=rgb(190, 190, 190, alpha = 175, maxColorValue = 255),border=NA, add=T)  # Cells with records
plot(TrdRast_analyses,
     col=rgb(255, 0, 0, alpha = 125, maxColorValue = 255), border=NA, add=T) # Cells after pruning
