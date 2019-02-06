# This is the script containing the models which will be used for the future paper (the models themselves)
# The scripts for data download, cleaning, preparations and functions are:
# "Data download and cleaning.R", "Community matrices.R", "Functions.R" and "HighstatLibV10.R"s


##--- 1.1 CLUSTER ANALYSIS ---####
##----------------------------####
library(vegan)
# Get the needed data: (a copy of the original dataframe, so that we do not mess up too much),
# then remove cells not within  municipality borders or only covered by ocean:
TrdRast_clust <- TrdRast_AR5
TrdRast_clust <- TrdRast_clust[!is.na(TrdRast_clust@data$total_area),]
TrdRast_clust <- TrdRast_clust[!(TrdRast_clust@data$Ocean==TrdRast_clust@data$total_area),]

              # We use the raw area rather than relative area, as the raw area measure ensures that some of the border-cells are
              # not automatically included in other clusters- Using relative area gives the cells with only small areas "too much
              # weight", since we cannot be certain that the land cover within municipality border is representative of the entire
              # cell. By using area, these might be filtered out anyways.

# Make the cluster-dendrogram based on a distance matrix with Bray-Curtis similarity 
clusters <- hclust(vegdist(TrdRast_clust@data[, 3:68], method="bray"))
par(mfrow=c(1,1))
par(mar=c(4.1,4.1,5.1,2.1))
plot(clusters, cex=0.5, main="Raw area (m^2)", xlab="Grid cell number")
abline(h=0.99, col="red", lty=2)       # Other cut-off values can be used, this gives a reasonable number of clusters


##--- 1.2 ClusterCut  ---####
##-----------------------####
# Identify the optimal number of clusters:
library(NbClust)
nclust <- NbClust(TrdRast_clust@data[, 3:68], diss=vegdist(TrdRast_clust@data[, 3:68], method="bray"), distance=NULL,
                  min.nc = 2, max.nc = 25, method="complete")

nclust$Best.nc   # Best number of clusters: 11, according to the majority rule

# Have a look at the number of cluster by different cut-offs
clusterCut1 <- as.data.frame(cutree(clusters, h=0.999))
table(clusterCut1)
clusterCut2 <- as.data.frame(cutree(clusters, h=0.9999))
table(clusterCut2)
rm(clusterCut1, clusterCut2)
# In all cases, we end up with some clusters having very few grid cells. By placing the cutoff at 0.99, we get 11 clusters
# with >3 grid cells

# Try and cut the dendrogram into clusters - the height of the cut is made solely with eye for the number of categories
clusterCut <- cutree(clusters, h=0.99)
table(clusterCut)

clusterCut <- as.data.frame(clusterCut)
clusterCut$Pixelnr <- TrdRast_clust@data$Pixelnr

TrdRast_clust <- merge(TrdRast_clust, clusterCut, by="Pixelnr")
table(clusterCut$clusterCut)

# Make a column with colours according to the cluster:
TrdRast_clust$col.clust <- NA
# Make the vector with colour names (redo this multiple times until the colours are reasonable):
for(i in 1:length(TrdRast_clust@data$clusterCut)){
  TrdRast_clust@data$col.clust[i] <- ifelse(TrdRast_clust@data$clusterCut[i]==1, paste("blue"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==2, paste("hotpink"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==3, paste("red"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==4, paste("orange"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==5, paste("palegreen"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==6, paste("green"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==7, paste("lightcyan"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==8, paste("forestgreen"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==9, paste("goldenrod"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==10, paste("sandybrown"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==11, paste("yellow"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==12, paste("cyan"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==13, paste("black"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==14, paste("gray30"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==15, paste("gray50"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==16, paste("gray70"),
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
image(y=0:12,z=t(0:12), col=c("gray", "blue", "hotpink", "red", "orange", "palegreen", "green", "lightcyan", "forestgreen",
                              "gray", "sandybrown", "yellow", "cyan"), axes=FALSE, main="clusterCut", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# This is potentially a reasonable number of clusters!


##--- 1.3 SPINE PLOTS ---####
##-----------------------####

# Make a dataframe with cluster as column and habitat as rows.
# In each entry is then the average of that habitat type for all cells within that cluster.
spine_all <- matrix(nrow=12, ncol = 66)
colnames(spine_all) <- colnames(TrdRast_AR5@data[3:68])
rownames(spine_all) <- c(1:12)
# Calculate the mean of habitat in the grid cells included in each cluster:
for(i in 1:dim(spine_all)[1]) {
  for(j in 1:dim(spine_all)[2]) {
    spine_all[i,j] = mean(TrdRast_clust@data[TrdRast_clust@data$clusterCut==i, colnames(spine_all)[j]])
  }
}
spine_all <- spine_all[-9,]

layout(t(1:2),widths=c(2,1))
par(mar=c(11,.1,2,2))
par(las=2)
par(cex.axis=0.8)
spineplot(spine_all, main="",
          col = c("hotpink", "lightpink", rep("forestgreen", 15), rep("darkolivegreen1", 11),
                  rep("darkolivegreen3", 10), "dodgerblue", rep("darkorange",2), rep("khaki1", 4),
                  rep("cyan", 9), "navy", rep("sandybrown", 7),
                  rep("gold",3), "gray"),
          xlab="", ylab="Mean cover of habitat in grid cells",
          xaxlabels = c("(1) Coastal", "(2) Urban/developed", "(3) Urban/vegetated/riparian",
                        "(4) Cultivated", "(5) Conif. forest, \nlow production", "(6) Conif. forest, \nmedium production",
                        "(7) Open marsh and \nconif. forest", "(8) Conif. forest, \nhigh production",
                        "(10) Open firm ground \n and forest", "(11) Open firm ground \nand cultivated land",
                        "(12) Freshwater"), yaxlabels = "", border=NA)

par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("Communications/traffic", "Developed area",
                          "Forest, coniferous", "Forest, deciduous", "Forest, mix",
                          "Freshwater", "Fully cultivated land",
                          "Home fields grazing land", "Marsh,", "Ocean",
                          "Open firm ground", "Superficially cultivated land", "NA"),
       fill=c("hotpink", "lightpink", "forestgreen", "darkolivegreen1",
              "darkolivegreen3", "dodgerblue", "darkorange", "khaki1",
              "cyan", "navy", "sandybrown",
              "gold", "gray"), border=NA, cex=1)
par(las=1)
par(cex.axis=1)

##--- 1.4 BETTER PLOTS FOR POTENTIAL PUBLICATION ---####
##--------------------------------------------------####
# To investigate the dominating habitats in each of the clusters (adjust the cluster-number and the dataframes).
# This can give a hint to, what be a reasonable "name" and colour for the clusters:
sort(colMeans(TrdRast_clust@data[TrdRast_clust@data$clusterCut==12,c(3:68)]), decreasing = TRUE)

# Grid cells coloured accourding to cluster:
layout(t(1:2),widths=c(3,1))
par(mar=c(0.1,0.1,0.1,0.1))
plot(TrdRast_clust, main="",
     col=as.character(TrdRast_clust@data$col.clust))  
par(mar=c(5,1,5,7))
image(y=0:11, z=t(0:11), axes=FALSE, main="", cex.main=0.75,
      col=c("gray", "blue", "hotpink", "red", "orange", "palegreen", "green", "lightcyan", "forestgreen", "sandybrown", "yellow", "cyan"))
axis(4,cex.axis=0.8, mgp=c(0,.5,0), at=seq(0, 11.5, by=1), las=2, cex.axis=0.6,
     labels=c("(0) Not grouped", "(1) Coastal", "(2) Urban/developed", "(3) Urban/vegetated/riparian", "(4) Cultivated",
              "(5) Coniferous forest, \nlow production", "(6) Coniferous forest, \nmedium production", "(7) Open marsh and \nconif. forest",
              "(8) Coniferous forest, \nhigh production", "(10) Open firm ground \nand forest",
              "(11) Open firm ground \nand cultivated land", "(12) Freshwater"))

### Some new (and hopefully better) colours for the maps ###
library(RColorBrewer)
map.col <- brewer.pal(n=12, name="Paired")
map.col <- c(map.col[2], map.col[5], map.col[6],
             map.col[8], map.col[3], map.col[4],
             map.col[9], map.col[12], map.col[7],
             map.col[11], "#98F5FF")
# Make a column with colours according to the cluster:
TrdRast_clust$col.clust2 <- NA
# Make the vector with colour names (redo this multiple times until the colours are reasonable):
for(i in 1:length(TrdRast_clust@data$clusterCut)){
  TrdRast_clust@data$col.clust2[i] <- ifelse(TrdRast_clust@data$clusterCut[i]==0, paste("white"),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==1, paste(map.col[1]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==2, paste(map.col[2]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==3, paste(map.col[3]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==4, paste(map.col[4]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==5, paste(map.col[5]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==6, paste(map.col[6]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==7, paste(map.col[7]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==8, paste(map.col[8]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==10, paste(map.col[9]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==11, paste(map.col[10]),
                                            ifelse(TrdRast_clust@data$clusterCut[i]==12, paste(map.col[11]), "white"))))))))))))
}

TrdRast_clust@data$col.clust2 <- as.factor(TrdRast_clust@data$col.clust2)

# With a better legend and new colours:
par(mfrow=c(1,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(TrdRast_clust, main="",
     col=as.character(TrdRast_clust@data$col.clust2))  
legend("top", 
       c("Not grouped (0)", "Coastal (1)", "Urban/developed (2)", "Urban/vegetated/riparian (3)",
                   "Cultivated (4)", "Coniferous forest, \nlow production (5)",
                   "Coniferous forest, \nmedium production (6)", "Open marsh and \nconif. forest (7)",
                   "Coniferous forest, \nhigh production (8)", "Open firm ground \nand forest (10)",
                   "Open firm ground \nand cultivated land (11)", "Freshwater (12)"),
       fill=c("white", map.col[1], map.col[2], map.col[3], map.col[4], map.col[5], map.col[6],
              map.col[7], map.col[8], map.col[9], map.col[10], map.col[11]), 
       bty="n", cex=0.8, x.intersp = 2, y.intersp = 2, ncol = 4)

# Add a border around the cells used in the analyses:
plot(TrdRast_clust_model, border="black", lwd=4, col=NA, add=TRUE)

# plot(TrdRast_clust_model[TrdRast_clust_model$Pixelnr==1369,], border="red", add=TRUE) # To see the cell with a potential outlier

##--- 2. ADD MORE NEEDED VARIABLES FOR THE MODELS ---####
##--- 2.1 HABTAT HETEROGENEITY/EVENESS            ---####
##---------------------------------------------------####

# Here we might use either the number of habitats or a measure of evenness/diversity.
# For the the number of land cover types in each grid cell:
TrdRast_clust$nhabitat <- specnumber(TrdRast_clust@data[,c(3:68)])

# Get an overview of the distribution of number of habitats:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(TrdRast_clust@data$nhabitat)

# We can calculate the diversity of the habitats as Simpson's index of diversity (1-D)   ( D = sum((n/N)^2) ).
# This is used rather than Shannon's as it is more intuitive to interpret. I use the (1-D) to
# get a more intuitive number rather than the simple D (0=no diversity, 1=infinite diversity)
TrdRast_clust$Divers <- diversity(TrdRast_clust@data[,c(3:68)], index="simpson")

# Get an overview of the distribution of diversity indices:
hist(TrdRast_clust@data$Divers)

# We can calculate the "evenness" of the habitat as Simpson's evenness, which is calculated as the invsimpson / S
# (Taken from Morris et al. (2014))
TrdRast_clust$Evenness <- (diversity(TrdRast_clust@data[,c(3:68)],
                                     index="invsimpson"))/TrdRast_clust@data$nhabitat

# The interpretation of Simpsons's Evenness: E is constrained between 0 and 1. The less evenness in communities
# between the species (here: habitats) (and the presence of a dominant species/habitat),
# the lower E is. And vice versa."

# Compare maps of cluster, number of habitats, diversity and eveness:
col.habitat <- colorRampPalette(c("white", "red"))

par(mfrow=c(2,2))
plot(TrdRast_clust, main="Cluster (area m^2)",
     col=as.character(TrdRast_clust@data$col.clust))    # Cluster, area
palette(col.habitat(20))
plot(TrdRast_clust, main="# habitats",
     col=TrdRast_clust@data$nhabitat)    # Number of land cover types
palette(col.habitat(10))
plot(TrdRast_clust, main="Habitat diversity",
     col=TrdRast_clust@data$Divers*10, bg="forestgreen")    # Habitat diversity
palette(col.habitat(10))
plot(TrdRast_clust, main="Habitat evenness",
     col=TrdRast_clust@data$Evenness*10)    # Habitat evenness

# Have a look at if these might seem correlated:
pairs.default(cbind(TrdRast_clust@data$nhabitat, TrdRast_clust@data$Divers, TrdRast_clust@data$Evenness),
              labels=c("Richness", "Diversity", "Evenness"))
cor.test(TrdRast_clust@data$nhabitat, TrdRast_clust@data$Divers, method="pearson")     # Significant, 0.63
cor.test(TrdRast_clust@data$nhabitat, TrdRast_clust@data$Evenness, method="pearson")   # Significant, -0.42
cor.test(TrdRast_clust@data$Divers, TrdRast_clust@data$Evenness, method="pearson")     # Significant bot low, 0.17


##--- 2.3 ASPECT ---####
##------------------####

# Load the .tiff-file from Marc (25*25m solution, utm33-projection) (it needs to be unzipped first)
aspect <- raster("aspect_trondheim_25meter_utm33.tif")
str(aspect@data)
aspect@crs
par(mfrow=c(1,3))
plot(aspect)

# Make all flat rasters (-1) NA:
aspect2 <- subs(aspect, data.frame(id=c(-1), v=c(NA)), subsWithNA=FALSE)

# Make a new RasterLayer with "Northness/Southness" rather than aspect (to make it non-circular)
northness <- cos(aspect2*pi/180)
plot(northness)

# Extract the northness values by the grid-polygons  (obs: the polygons are CRS-transformed to match the raster):
Trd_northness <- raster::extract(northness, TrdRast_clust)

# Have a look at the data
View(as.data.frame(sapply(Trd_northness, mean)))

# Add mean "northness" of each grid cell to the data frames
TrdRast_clust$north.mean <- sapply(Trd_northness, mean, na.rm=TRUE)

# Compare maps
par(mfrow=c(2,2))
par(mar=c(2,5.5,3,5.5))
plot(aspect, main="Raw aspect data")
plot(aspect2, main="Aspect, flat cells as 'NA'")
plot(northness, main="Northness")
palette(rev(terrain.colors(20)))
plot(TrdRast_clust, col=(TrdRast_clust@data$north.mean+1)*10, main="Mean northness in cell")


##--- 3. MODELS WITH ADDTITIONAL VARIABLES ---####
##--------------------------------------------####
# Done for both the datasets with raw area and relative area
# First prepare the data to only include the cells adequate for modelling, and only
# including the columns needed (to save some memory):
TrdRast_clust_model <- TrdRast_clust[TrdRast_clust@data$CoV_2013<=0.25 &             # Only cells with low CoV
                                       TrdRast_clust@data$Ntotal>=10 &               # Only cells with >10 records
                                       !is.na(TrdRast_clust@data$CoV_2013), c(1,69:88)]        # Only cells with a valid CoV

# Transformation of the response variables (here we have to add a constant to make the calculations, as log(0) is meaningsless)
# I here use 1, as this gives the closest approximation of a Gaussian distribution (smaller make the data even more skewed):
TrdRast_clust_model$log_chao.reds <- NA
TrdRast_clust_model$log_chao.blacks <- NA
TrdRast_clust_model$log_chao.all <- NA
for(i in 1:NROW(TrdRast_clust_model@data)){
  TrdRast_clust_model@data[i,"log_chao.reds"] <- log(TrdRast_clust_model@data[i,"S.chao1_reds_2013"] + 1)
  TrdRast_clust_model@data[i,"log_chao.blacks"] <- log(TrdRast_clust_model@data[i,"S.chao1_blacks_2013"] + 1)
  TrdRast_clust_model@data[i,"log_chao.all"] <- log(TrdRast_clust_model@data[i,"S.chao1_2013"] + 1)
}

# Replace 'NA's with zeros - OBS! Important to do ONLY for the response variables!
TrdRast_clust_model@data$log_chao.blacks[is.na(TrdRast_clust_model@data$log_chao.blacks)] <- 0
TrdRast_clust_model@data$log_chao.reds[is.na(TrdRast_clust_model@data$log_chao.reds)] <- 0

# For future comparison between predicted values all across Trondheim and the observed values, we need to do this for the
# full dataset as well:
{
  # Transformation of the response variables (here we have to add a constant to make the calculations, as log(0) is meaningsless)
  # I here use 1, as this gives the closest approximation of a Gaussian distribution (smaller make the data even more skewed):
  TrdRast_clust$log_chao.reds <- NA
  TrdRast_clust$log_chao.blacks <- NA
  TrdRast_clust$log_chao.all <- NA
  for(i in 1:NROW(TrdRast_clust@data)){
    TrdRast_clust@data[i,"log_chao.reds"] <- log(TrdRast_clust@data[i,"S.chao1_reds_2013"] + 1)
    TrdRast_clust@data[i,"log_chao.blacks"] <- log(TrdRast_clust@data[i,"S.chao1_blacks_2013"] + 1)
    TrdRast_clust@data[i,"log_chao.all"] <- log(TrdRast_clust@data[i,"S.chao1_2013"] + 1)
  }
  
  # Replace 'NA's with zeros - OBS! Important to do ONLY for the response variables!
  TrdRast_clust@data$log_chao.blacks[is.na(TrdRast_clust@data$log_chao.blacks)] <- 0
  TrdRast_clust@data$log_chao.reds[is.na(TrdRast_clust@data$log_chao.reds)] <- 0
  TrdRast_clust@data$log_chao.all[is.na(TrdRast_clust@data$log_chao.all)] <- 0
}


##--- 3.1 DATA EXPLORATION ---####
##----------------------------####
source("HighstatLibV10.R")

# An important point for why we're using the Chao-estimation rather than the raw species numbers, is to take sampling
# effort into account - hence, we have to make see if we have a correlation between number of observed species and the
# estimated numbers within the cells we're using for the analyses
MyVar_numbers <- c("S.obs_2013", "S.obs_reds_2013", "S.obs_blacks_2013",
                   "S.chao1_2013", "S.chao1_reds_2013", "S.chao1_blacks_2013",
                   "log_chao.all", "log_chao.reds", "log_chao.blacks")
pairs(TrdRast_clust_model@data[, MyVar_numbers], 
      lower.panel = panel.cor)                     # Not too important yet, the important one is the model predictions

# Outliers:
MyVar <- c("log_chao.all", "log_chao.reds", "log_chao.blacks",
           "clusterCut", "nhabitat", "Divers", "Evenness", "north.mean")

### Dotplots
Mydotplot(TrdRast_clust_model@data[,MyVar])   

# We already know (from preliminary analyses) that we have at least one outlier due to the Ringve Botanical Garden.
# Remove the Ringve-outlier:
TrdRast_clust_model <- TrdRast_clust_model[!TrdRast_clust_model@data$log_chao.blacks>5,]

# It can be discussed wheter we have outliers in the number of total species

# We have two cells with Evenness and Diversity of zero - these are outliers in the Evenness-variable. These
# are both cells covered by freshwater. If we look at the number of observed species and total number of observations,
# they are seemingly not outliers there. However, those two points seem to be controlling some of the relationships completely.
# They could thus removed from the analyses - we choose not to do so, as we will only use one of the variables in the analyses -
# but now we know!
      # TrdRast_clust_model <- TrdRast_clust_model[!TrdRast_clust_model@data$Evenness==1,]

# Look again:
Mydotplot(TrdRast_clust_model@data[,MyVar])

          # It can be debated whether we need to remove more outliera. For now, I'll let it be as I have no
          # reason to remove (similar to the one above)

### Colinearity
pairs(TrdRast_clust_model@data[, MyVar], 
      lower.panel = panel.cor)                # As expected, we cannot include all measures of habitat heterogeneity
                                              # After removing the "Evenness-outliers", more of the indices are colinear.
                                              # In the models, we thus have to either use nhabitat+Evenness or only Diversity

corvif(TrdRast_clust_model@data[, MyVar])     # The highest GVIF is for Diversity
corvif(TrdRast_clust_model@data[, MyVar[-c(5,7)]]) # This would severely lower the GVIFs

### Relationships
MyVar2 <- c("log_chao.all", "log_chao.reds", "log_chao.blacks",
           "clusterCut", "Divers", "north.mean")

Myxyplot(TrdRast_clust_model@data, MyVar2, "log_chao.reds", 
         MyYlab = "ESR of redlisted species (m^2)")
Myxyplot(TrdRast_clust_model@data, MyVar2, "log_chao.blacks", 
         MyYlab = "ESR of alien species (m^2)")
Myxyplot(TrdRast_clust_model@data, MyVar2, "log_chao.all", 
         MyYlab = "ESR of all species (m^2)")


# Make sure the factor-classes are correct:
TrdRast_clust_model$clusterCut <- as.factor(TrdRast_clust_model$clusterCut)


##--- 3.2   PRELIMINARY MODELLING (NON-SPATIAL) ---####
##--- 3.2.1  Model 1 - threatened species       ---####
##-------------------------------------------------####

# Unfortunately, when we want to run the "step"-function, cells with missing values cannot be used - for this dataset,
# it is luckily only three grid cells (covered entirely by freshwater, north.mean = NaN). We have to omit that one,
# unless we find a reasonable value
TrdRast_clust_model <- TrdRast_clust_model[!is.na(TrdRast_clust_model@data$north.mean),]

global_M1 <- glm(log_chao.reds ~  clusterCut + Divers + north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M1)       # Backwards selection using AIC

# Define the "better" models:
M1 <- glm(log_chao.reds ~  clusterCut + Divers,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M1)

# Plot residuals vs fitted values (M1)
F1 <- fitted(M1)
E1 <- resid(M1, type = "pearson")      
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1,
     xlab = "Fitted values - M1",
     ylab = "Pearson residuals - M1",
     cex.lab = 1.5)
abline(h = 0, lty = 2)                        # The seen pattern is due to the categorical variable - no worries


# Plot the residuals vs each covariate     
TrdRast_clust_model@data$E1 <- E1
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "E1")
TrdRast_clust_model@data$E1 <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1)

# Compare the predictor variable levels:
library(multcomp)
summary(glht(M1, linfct=mcp(clusterCut="Tukey")))


##--- 3.2.2  Model 2 - alien species ---####
##---------------------------------------####
global_M2 <- glm(log_chao.blacks ~  clusterCut + Divers +  north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M2)       # Backwards selection using AIC

M2 <- glm(log_chao.blacks ~  clusterCut + Divers + north.mean,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M2)

# Plot residuals vs fitted values (M2)
F2 <- fitted(M2)
E2 <- resid(M2, type = "pearson")      
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F2, 
     y = E2,
     xlab = "Fitted values - M2",
     ylab = "Pearson residuals - M2",
     cex.lab = 1.5)
abline(h = 0, lty = 2)                       

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$E2 <- E2
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "E2")
TrdRast_clust_model@data$E2 <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mar=c(5.1,4.1,4.1,2.1))
hist(E2)

# Compare the predictor variable levels:
library(multcomp)
summary(glht(M2, linfct=mcp(clusterCut="Tukey")))
summary(glht(M2_rel, linfct=mcp(clusterCut="Tukey")))


##--- 3.2.3  Model 3 - all species ---####
##------------------------------------####
global_M3 <- glm(log_chao.all ~  clusterCut + Divers +  north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M3)       # Backwards selection using AIC

M3 <- glm(log_chao.all ~  clusterCut + Divers,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M3)

# Plot residuals vs fitted values (M2)
F3 <- fitted(M3)
E3 <- resid(M3, type = "pearson")      
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F3, 
     y = E3,
     xlab = "Fitted values - M3",
     ylab = "Pearson residuals - M3",
     cex.lab = 1.5)
abline(h = 0, lty = 2)                       

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$E3 <- E3
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "E3")
TrdRast_clust_model@data$E3 <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mar=c(5.1,4.1,4.1,2.1))
hist(E3)



##--- 3.3 SPATIAL AUTOCORRELATION- threatened species ---####
##--- 3.3.1 Testing for SAC - Chao1_reds              ---####
##-------------------------------------------------------####
library(spdep)
library(ncf)

xy_clust <- coordinates(TrdRast_clust_model) 

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_clust_model$log_chao.reds) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_clust_model, col=(TrdRast_clust_model$log_chao.reds))   
image(y=0:4,z=t(0:4), col=rev(col.heat), axes=FALSE, main="log(threatened\n+1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))
# From pure visual estimation, we seem to have some autocorrelation

# Make a correlogram:
correlog1 <- correlog(xy_clust[,1], xy_clust[,2], residuals(M1), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog1$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I, threatened species"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M1))/2+1.5], pch=19,
     cex=abs(resid(M1))/max(resid(M1))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
clust.nb <- dnearneigh(as.matrix(xy_clust[,1:2]), 0, 1500) # Find the neighbors - give lower and upper distance class here
          # OBS! The classes are in euclidian distance (m), thus we need a reasonable distance to define
          # a neighbouring grid cell. Here, I have chosen to use 1.5 km - that is the distance from center to center
          # to make it reasonable!
clust.listw <- nb2listw(clust.nb, zero.policy = T)       # Turns neighbourhood object into a weighted list

# this next step might take a few minutes to run:
GlobMT1_clust <- moran.test(residuals(M1), listw=clust.listw, zero.policy = T)
GlobMT1_clust    # Significant

# Look at it through Monte-Carlo simulation as well:
MC_res_clust <- moran.mc(residuals(M1), clust.listw,
                     zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
MC_res_clust
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res_clust, main="Threatened species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res_clust$statistic, lty=2, col="red")

# We seemingly have SAC in these model residuals.
# Lets have a look at whether there is SAC in the data itself rather than only the residuals (not strictly necessary):
moran(TrdRast_clust_model$log_chao.reds, clust.listw, n=length(clust.listw$neighbours),
      S0=Szero(clust.listw), zero.policy = TRUE)    # Calculate Moran's I

# Test for significance:
moran.test(TrdRast_clust_model$log_chao.reds, clust.listw, randomisation=FALSE,
           alternative = "two.sided", zero.policy = TRUE)     # Using linear regression based logic and assumptions

MC_clust <- moran.mc(TrdRast_clust_model$log_chao.reds, clust.listw,
                     zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_clust, main=NULL)     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_clust$statistic, lty=2, col="red")

# Make a correlogram
correlog1_data <- sp.correlogram(clust.nb, TrdRast_clust_model$log_chao.reds, order=8, method="I", zero.policy = TRUE)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(correlog1_data)

# We thus have SAC in the data and in the model residuals


##--- 3.3.2 Testing for SAC - Chao1_blacks             ---####
##--------------------------------------------------------####

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_clust_model$log_chao.blacks) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_clust_model, col=(TrdRast_clust_model$log_chao.blacks)) 
par(mar=c(5,1,5,2.5))
image(y=0:4,z=t(0:4), col=rev(col.heat), axes=FALSE, main="log(alien\n+1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog2 <- correlog(xy_clust[,1], xy_clust[,2], residuals(M2), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M2))/2+1.5], pch=19,
     cex=abs(resid(M2))/max(resid(M2))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT2 <- moran.test(residuals(M2), listw=clust.listw, zero.policy = T)
GlobMT2     # Significant SAC

# Look at it through Monte-Carlo simulation as well:
MC_res2_clust <- moran.mc(residuals(M2), clust.listw,
                         zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res2_clust, main="Alien species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res2_clust$statistic, lty=2, col="red")


# SAC in these model residuals

##--- 3.3.3 Testing for SAC - Chao1_all             ---####
##-----------------------------------------------------####

# Make a plot to visualize - some autocorrelation is detectable:
col.heat <- heat.colors(max(TrdRast_clust_model$log_chao.all) + 1)
palette(rev(col.heat))
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,1,1))
plot(TrdRast_clust_model, col=(TrdRast_clust_model$log_chao.all)) 
par(mar=c(5,1,5,2.5))
image(y=0:9,z=t(0:9), col=rev(col.heat), axes=FALSE, main="log(all\n+1)", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

# Make a correlogram:
correlog3 <- correlog(xy_clust[,1], xy_clust[,2], residuals(M3), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog3$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M3))/2+1.5], pch=19,
     cex=abs(resid(M3))/max(resid(M3))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
GlobMT3 <- moran.test(residuals(M3), listw=clust.listw, zero.policy = T)
GlobMT3     # Significant SAC

# Look at it through Monte-Carlo simulation as well:
MC_res3_clust <- moran.mc(residuals(M3), clust.listw,
                          zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res3_clust, main="Alien species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res3_clust$statistic, lty=2, col="red")


# SAC in these model residuals



##--- 3.4 DEALING WITH SAC ---####
##--- 3.4.1 Chao1_reds     ---####
##----------------------------####

# To deal with the spatial autocorrelation, we can use a GLS with an added correlation structure - we thus also
# need to figure out what kind of correlation structure is apppriate. As it takes extensive experience actually
# differentiating, I go with the simplest one. Running them all and comparing by AIC indicates that the
# ratio-structure might be appropriate as well.

# OBS! For the model selection, I wasn't qutie happy with just picking the "best" model based on AIC.
# Instead I decided to look at the bias-corrected AICc, and use model averaging, as both AIC and AICc were
# unable to differentiate between the best candidate models (deltaAIC <2 ).
# The two Information criteria give slightly different estimates of the best models, thus model averaging seems
# to be the best option
library(MuMIn)
require(nlme)
summary(gls_clust <- gls(log_chao.reds ~  clusterCut + Divers + north.mean,
                         data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))

AIC(global_M1, gls_clust)              # Not better (spatial model has higher AIC) - potentially, model selection will change this

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls_ML_clust <- gls(log_chao.reds ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

library(MASS)
#stepAIC(gls_ML_clust)              # For unknown reasons, the standard 'step()' doesn't work - this one does
d.red <- dredge(gls_ML_clust)
d.red2 <- subset(d.red, delta<2)
model.sel(d.red2)
output_red <- model.sel(d.red2)


# In this case, there was only one best candidate model, thus model averaging i unnecessary for this particular set.
# According to the SAC-function, the best model is: log_chao.reds ~ clusterCut + Divers
# This is similar to the optimal model for the non-spatial approach 

# Define the better model(s):
summary(gls_clust_reds <- gls(log_chao.reds ~  clusterCut + Divers,
                              data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                              method = "ML"))

# To keep consistency in naming of models:
gls_avg_reds <- gls(log_chao.reds ~  clusterCut + Divers,
                    data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                    method = "ML")   

AIC(M1, gls_clust_reds)              # The model is seemingly not better, according to AIC. This could potentially
                                     # be an artifact of the warning: "models are not all fitted to the same number
                                     # of observations" - this is probably as we introduce the neighbours, some grid
                                     # cells are "floating". The AIC might be worse, but we have shown that we have
                                     # SAC. We continue with the spatial models then

# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
#summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
#            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#            method = "ML"))
summary(update(gls_avg_reds, ~ . - clusterCut + relevel(clusterCut, ref="2")))


##--- 3.4.2 Chao1_blacks    ---####
##-----------------------------####
summary(gls.b_clust <- gls(log_chao.blacks ~  clusterCut + Divers + north.mean,
                           data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))

AIC(global_M2, gls.b_clust)               # Significantly better (dAIC > 2)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.b_ML_clust <- gls(log_chao.blacks ~  clusterCut + Divers + north.mean,
                              data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

#stepAIC(gls.b_ML_clust)        # log_chao.blacks ~ clusterCut + Divers
d.black <- dredge(gls.b_ML_clust)
d.black2 <- subset(d.black, delta<2)
model.sel(d.black2)
output_black <- model.sel(d.black2)
importance(output_black)
# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_blacks <- model.avg(output_black, fit=TRUE)   

gls_avg_blacks$msTable
gls_avg_blacks$coefficients
gls_avg_blacks$formula
gls_avg_blacks$call
summary(gls_avg_blacks)   

summary(gls_avg_blacks)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model) rather than the conditional
                                      # as this is better when we are interested in the importance of variables (Grueber et al. 2011)
summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))),
                  subset=delta<2, fit=TRUE))$coefmat.full

# This is somewhat similar to the optimal model for the non-spatial approach, minus the north.mean

# Define the better model and compare AIC:
#summary(gls_clust_blacks <- gls(log_chao.blacks ~  clusterCut + Divers,
#                                data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#                                method = "ML"))

AIC(M2, gls_clust_blacks)


# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
#summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
#            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#            method = "ML"))

model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)


##--- 3.4.3 Chao1_all    ---####
##--------------------------####
summary(gls.a_clust <- gls(log_chao.all ~  clusterCut + Divers + north.mean,
                           data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))

AIC(global_M3, gls.a_clust)               # Significantly better (dAIC > 2)

# As we now have a basal model, we can try and do some model selection similar to what we did for the uncorrelated model.
# We need to redefine the model to use "Maximum Likelihood" rather than the gls-default "REML" - the latter makes
# the backwards model selection impossible, as the AIC is undefined:
summary(gls.a_ML_clust <- gls(log_chao.all ~  clusterCut + Divers + north.mean,
                              data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

#stepAIC(gls.a_ML_clust)        # log_chao.all ~ clusterCut + Divers
d.all <- dredge(gls.a_ML_clust)
d.all2 <- subset(d.all, delta<2)
model.sel(d.all2)
output_all <- model.sel(d.all2)
importance(output_all)
# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_all <- model.avg(output_all, fit=TRUE)   

gls_avg_all$msTable
gls_avg_all$coefficients
gls_avg_all$formula
gls_avg_all$call
summary(gls_avg_all)   

summary(gls_avg_all)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model) rather than the conditional
                                   # as this is better when we are interested in the importance of variables (Grueber et al. 2011)
summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))),
                  subset=delta<2, fit=TRUE))$coefmat.full


# Define the better model and compare AIC:
#summary(gls_clust_all <- gls(log_chao.all ~  clusterCut + Divers,
#                                data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#                             method = "ML"))

AIC(M3, gls_clust_all)


# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
#summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
#            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#            method = "ML"))
model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)


##--- 3.4.3 Model validation ---####
##------------------------------####
summary(gls_avg_reds)      # summary(gls_clust_reds)
summary(gls_avg_blacks)    # summary(gls_clust_blacks)
summary(gls_avg_all)       # summary(gls_clust_all)

# Pseudo-R^2 calculated as correlation between observed and predicted values - this is what was done in
# in the Ballesteros-Meija paper - I am uncertain whether is is desirable for me.
# However, it seems to be my best option for a Goodness of Fit-test - I can compare them to the R^2 of the
# non-spatial models (calculated as (1-(Residual deviance/Null deviance))).
# Get the needed data:
pseudo <- merge(TrdRast_clust_model@data[,c(1,22:27)], data_predict_clust@data[,c(1,7:9,17:19,23:25)], by="Pixelnr")

# Make the calculations (and potentially compare):
cor(pseudo$log_chao.reds, pseudo$predict_reds.avg)       # 0.594   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.reds, pseudo$predict_reds.avg))^2   # 0.353

cor(pseudo$log_chao.blacks, pseudo$predict_blacks.avg)       # 0.473   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.blacks, pseudo$predict_blacks.avg))^2   # 0.224

cor(pseudo$log_chao.all, pseudo$predict_all.avg)       # 0.304   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.all, pseudo$predict_all.avg))^2   # 0.093


# Plot residuals vs fitted values (gls.exp_clust_reds)
F1_threat <- fitted(gls_avg_reds)
E1_threat <- resid(gls_avg_reds, type = "pearson")      
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_threat, 
     y = E1_threat,
     xlab = "Fitted values - threat.",
     ylab = "Pearson residuals - threat.",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_threat <- E1_threat
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "Resid_threat")
TrdRast_clust_model@data$Resid_threat <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_threat)


### THESE ARE NOT WORKING FOR MODEL-AVERAGING! ###
## Plot residuals vs fitted values (gls.exp_clust_blacks)
#F1_alien <- fitted(gls_avg_blacks, full=TRUE)
#E1_alien <- resid(gls_avg_blacks, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
#par(mfrow = c(1,1), mar = c(5,5,2,2))
#plot(x = F1_alien, 
#     y = E1_alien,
#     xlab = "Fitted values - alien",
#     ylab = "Pearson residuals - alien",
#     cex.lab = 1.5)
#abline(h = 0, lty = 2)

## Plot the residuals vs each covariate     
#TrdRast_clust_model@data$Resid_alien <- E1_alien
#Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "Resid_alien")
#TrdRast_clust_model@data$Resid_alien <- NULL

# Histogram of the residuals to check is they are Gaussian:
#par(mfrow=c(1,1))
#par(mar=c(5.1,4.1,4.1,2.1))
#hist(E1_alien)

## Plot residuals vs fitted values (gls.exp_clust_all)
#F1_all <- fitted(gls_clust_all)
#E1_all <- resid(gls_clust_all, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
#par(mfrow = c(1,1), mar = c(5,5,2,2))
#plot(x = F1_all, 
#     y = E1_all,
#     xlab = "Fitted values - all",
#     ylab = "Pearson residuals - all",
#     cex.lab = 1.5)
#abline(h = 0, lty = 2)

## Plot the residuals vs each covariate     
#TrdRast_clust_model@data$Resid_all <- E1_all
#Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "Resid_all")
#TrdRast_clust_model@data$Resid_all <- NULL

# Histogram of the residuals to check is they are Gaussian:
#par(mfrow=c(1,1))
#par(mar=c(5.1,4.1,4.1,2.1))
#hist(E1_all)

# QQ-plots - not quite working for model averaging either
qqnorm(gls_avg_reds, abline=c(0,1))
qqnorm(gls_avg_blacks$coefficients[1,])
qqnorm(gls_avg_all$coefficients[1,])
qqnorm(gls_clust_blacks, abline=c(0,1))
qqnorm(gls_clust_all, abline=c(0,1))





##--- 3.4.3.1 Plots of model coefficients ---####
##-------------------------------------------####
# Plot of Cluster coefficients and their std.error - the coefficients and their std.errors are picked up by
# refitting the models with a new reference level. The coefficient is the same across all models (baseline+correction),
# the std.errors are from the model in which that factor level is baseline
par(mfrow=c(1,1))
par(mar=c(10,4.1,0.5,2.1))
plot(1, type="n", xlab="", ylab="Model coefficient", xlim=c(1, 11), ylim=c(-1.5, 4.5), xaxt="n")
axis(1, at=c(1:11), labels=c("(1) Coastal", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
                             "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
                             "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
                             "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
                             "(12) Freshwater", "Habitat \nheterogeneity"), las=2)
abline(h=0, lty=2, col="gray")

### Threatened ####
# Cluster 1
segments( 0.9, coef(summary(gls_avg_reds))[1,1] - coef(summary(gls_avg_reds))[1,2],  
          x1=0.9, y1=coef(summary(gls_avg_reds))[1,1] + coef(summary(gls_avg_reds))[1,2], col="red")
      points(0.9, (coef(summary(gls_avg_reds))[1,1]), pch=20, col="red")
# Cluster 2
segments( 1.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=1.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(1.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")
# Cluster 3
segments( 2.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(2.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 4
segments( 3.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(3.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 5
segments( 4.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(4.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 6
segments( 5.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(5.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 7
segments( 6.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(6.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 8
segments( 7.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(7.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 11
segments( 8.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(8.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 12
segments( 9.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(9.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

segments(10.9, coef(summary(gls_clust_reds))[11,1] - coef(summary(gls_clust_reds))[11,2],  # Habitat diversity
         x1=10.9, y1=coef(summary(gls_clust_reds))[11,1] + coef(summary(gls_clust_reds))[11,2], col="red")
    points(10.9, (coef(summary(gls_clust_reds))[11,1]), pch=20, col="red")

    
## Alien ####
    # Cluster 1
    segments( 1.1, coef(summary(gls_clust_blacks))[1,1] - coef(summary(gls_clust_blacks))[1,2],  
              x1=1.1, y1=coef(summary(gls_clust_blacks))[1,1] + coef(summary(gls_clust_blacks))[1,2], col="black")
    points(1.1, (coef(summary(gls_clust_blacks))[1,1]), pch=20, col="black")
    
    # Cluster 2
    segments( 2.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=2.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(2.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    # Cluster 3
    segments( 3.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=3.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(3.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 4
    segments( 4.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=4.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(4.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 5
    segments( 5.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=5.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(5.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 6
    segments( 6.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=6.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(6.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 7
    segments( 7.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=7.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(7.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 8
    segments( 8.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=8.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(8.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 11
    segments( 9.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=9.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(9.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    
    # Cluster 12
    segments( 10.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=10.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
    points(10.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
    # Habitat heterogeneity    
    segments( 11.1, coef(summary(gls_clust_blacks))[11,1] - coef(summary(gls_clust_blacks))[11,2],
              x1=11.1, y1=coef(summary(gls_clust_blacks))[11,1] + coef(summary(gls_clust_blacks))[11,2])
    points(11.1, (coef(summary(gls_clust_blacks))[11,1]), pch=20)
    
### AVG.model
    
# Cluster 1
segments( 1.1, summary(gls_avg_blacks)$coefmat.full[1,1] - summary(gls_avg_blacks)$coefmat.full[1,3],  
          x1=1.1, y1=summary(gls_avg_blacks)$coefmat.full[1,1] + summary(gls_avg_blacks)$coefmat.full[1,3], col="black")
points(1.1, (summary(gls_avg_blacks)$coefmat.full[1,1]), pch=20, col="black")
    
# Cluster 2 
{cl2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
segments( 2.1, (summary(cl2_blacks)$coefmat.full[1,1] - summary(cl2_blacks)$coefmat.full[1,3]),  
          x1=2.1, y1=(summary(cl2_blacks)$coefmat.full[1,1] + summary(cl2_blacks)$coefmat.full[1,3]), col="black")
points(2.1, summary(cl2_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl2_blacks)}

# Cluster 3
{cl3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3.1, (summary(cl3_blacks)$coefmat.full[1,1] - summary(cl3_blacks)$coefmat.full[1,3]),  
            x1=3.1, y1=(summary(cl3_blacks)$coefmat.full[1,1] + summary(cl3_blacks)$coefmat.full[1,3]), col="black")
points(3.1, summary(cl3_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl3_blacks)}
    
    
# Cluster 4
{cl4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4.1, (summary(cl4_blacks)$coefmat.full[1,1] - summary(cl4_blacks)$coefmat.full[1,3]),  
            x1=4.1, y1=(summary(cl4_blacks)$coefmat.full[1,1] + summary(cl4_blacks)$coefmat.full[1,3]), col="black")
points(4.1, summary(cl4_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl4_blacks)}
    
        
# Cluster 5
{cl5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5.1, (summary(cl5_blacks)$coefmat.full[1,1] - summary(cl5_blacks)$coefmat.full[1,3]),  
            x1=5.1, y1=(summary(cl5_blacks)$coefmat.full[1,1] + summary(cl5_blacks)$coefmat.full[1,3]), col="black")
points(5.1, summary(cl5_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl5_blacks)}
    
    
# Cluster 6
{cl6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6.1, (summary(cl6_blacks)$coefmat.full[1,1] - summary(cl6_blacks)$coefmat.full[1,3]),  
            x1=6.1, y1=(summary(cl6_blacks)$coefmat.full[1,1] + summary(cl6_blacks)$coefmat.full[1,3]), col="black")
points(6.1, summary(cl6_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl6_blacks)}
    
    
# Cluster 7
{cl7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7.1, (summary(cl7_blacks)$coefmat.full[1,1] - summary(cl7_blacks)$coefmat.full[1,3]),  
          x1=7.1, y1=(summary(cl7_blacks)$coefmat.full[1,1] + summary(cl7_blacks)$coefmat.full[1,3]), col="black")
points(7.1, summary(cl7_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl7_blacks)}
    
    
# Cluster 8
{cl8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8.1, (summary(cl8_blacks)$coefmat.full[1,1] - summary(cl8_blacks)$coefmat.full[1,3]),  
            x1=8.1, y1=(summary(cl8_blacks)$coefmat.full[1,1] + summary(cl8_blacks)$coefmat.full[1,3]), col="black")
points(8.1, summary(cl8_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl8_blacks)}
    
    
# Cluster 11
{cl11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9.1, (summary(cl11_blacks)$coefmat.full[1,1] - summary(cl11_blacks)$coefmat.full[1,3]),  
            x1=9.1, y1=(summary(cl11_blacks)$coefmat.full[1,1] + summary(cl11_blacks)$coefmat.full[1,3]), col="black")
points(9.1, summary(cl11_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl11_blacks)}
    
    
# Cluster 12
{cl12_blacks <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10.1, (summary(cl12_blacks)$coefmat.full[1,1] - summary(cl12_blacks)$coefmat.full[1,3]),  
            x1=10.1, y1=(summary(cl12_blacks)$coefmat.full[1,1] + summary(cl12_blacks)$coefmat.full[1,3]), col="black")
points(10.1, summary(cl12_blacks)$coefmat.full[1,1], pch=20, col="black")
rm(cl12_blacks)}
    
    
# Habitat heterogeneity    
segments(11.1, summary(gls_avg_blacks)$coefmat.full[11,1] - summary(gls_avg_blacks)$coefmat.full[11,3],
          x1=11.1, y1=summary(gls_avg_blacks)$coefmat.full[11,1] + summary(gls_avg_blacks)$coefmat.full[11,3]) 
points(11.1, summary(gls_avg_blacks)$coefmat.full[11,1], pch=20)
    
    
## All ####
    # Cluster 1
    segments( 1, coef(summary(gls_clust_all))[1,1] - coef(summary(gls_clust_all))[1,2],  
              x1=1, y1=coef(summary(gls_clust_all))[1,1] + coef(summary(gls_clust_all))[1,2], col="blue")
    points(1, (coef(summary(gls_clust_all))[1,1]), pch=20, col="blue")
    
    # Cluster 2
    segments( 2, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=2, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(2, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    # Cluster 3
    segments( 3, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=3, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(3, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 4
    segments( 4, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=4, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(4, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 5
    segments( 5, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=5, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(5, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 6
    segments( 6, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=6, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(6, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 7
    segments( 7, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=7, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(7, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 8
    segments( 8, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=8, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(8, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 11
    segments( 9, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                      coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=9, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                            coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(9, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
    
    # Cluster 12
    segments( 10, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                      correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                       coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
              x1=10, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                             coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
    points(10, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

    # Hab. heterogenity    
segments( 11, coef(summary(gls_clust_all))[11,1] - coef(summary(gls_clust_all))[11,2],  
            x1=11, y1=coef(summary(gls_clust_all))[11,1] + coef(summary(gls_clust_all))[11,2], col="blue")
  points(11, (coef(summary(gls_clust_all))[11,1]), pch=20, col="blue")

  
### AVG.model

# Cluster 1
segments( 1, summary(gls_avg_all)$coefmat.full[1,1] - summary(gls_avg_all)$coefmat.full[1,3],  
          x1=1, y1=summary(gls_avg_all)$coefmat.full[1,1] + summary(gls_avg_all)$coefmat.full[1,3], col="blue")
points(1, (summary(gls_avg_all)$coefmat.full[1,1]), pch=20, col="blue")

# Cluster 2 
{cl2_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments(2, (summary(cl2_all)$coefmat.full[1,1] - summary(cl2_all)$coefmat.full[1,3]),  
            x1=2, y1=(summary(cl2_all)$coefmat.full[1,1] + summary(cl2_all)$coefmat.full[1,3]), col="blue")
  points(2, summary(cl2_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl2_all)}

# Cluster 3
{cl3_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3, (summary(cl3_all)$coefmat.full[1,1] - summary(cl3_all)$coefmat.full[1,3]),  
            x1=3, y1=(summary(cl3_all)$coefmat.full[1,1] + summary(cl3_all)$coefmat.full[1,3]), col="blue")
  points(3, summary(cl3_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl3_all)}


# Cluster 4
{cl4_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4, (summary(cl4_all)$coefmat.full[1,1] - summary(cl4_all)$coefmat.full[1,3]),  
            x1=4, y1=(summary(cl4_all)$coefmat.full[1,1] + summary(cl4_all)$coefmat.full[1,3]), col="blue")
  points(4, summary(cl4_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl4_all)}


# Cluster 5
{cl5_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5, (summary(cl5_all)$coefmat.full[1,1] - summary(cl5_all)$coefmat.full[1,3]),  
            x1=5, y1=(summary(cl5_all)$coefmat.full[1,1] + summary(cl5_all)$coefmat.full[1,3]), col="blue")
  points(5, summary(cl5_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl5_all)}


# Cluster 6
{cl6_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6, (summary(cl6_all)$coefmat.full[1,1] - summary(cl6_all)$coefmat.full[1,3]),  
            x1=6, y1=(summary(cl6_all)$coefmat.full[1,1] + summary(cl6_all)$coefmat.full[1,3]), col="blue")
  points(6, summary(cl6_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl6_all)}


# Cluster 7
{cl7_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7, (summary(cl7_all)$coefmat.full[1,1] - summary(cl7_all)$coefmat.full[1,3]),  
            x1=7, y1=(summary(cl7_all)$coefmat.full[1,1] + summary(cl7_all)$coefmat.full[1,3]), col="blue")
  points(7, summary(cl7_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl7_all)}


# Cluster 8
{cl8_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8, (summary(cl8_all)$coefmat.full[1,1] - summary(cl8_all)$coefmat.full[1,3]),  
            x1=8, y1=(summary(cl8_all)$coefmat.full[1,1] + summary(cl8_all)$coefmat.full[1,3]), col="blue")
  points(8, summary(cl8_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl8_all)}


# Cluster 11
{cl11_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9, (summary(cl11_all)$coefmat.full[1,1] - summary(cl11_all)$coefmat.full[1,3]),  
            x1=9, y1=(summary(cl11_all)$coefmat.full[1,1] + summary(cl11_all)$coefmat.full[1,3]), col="blue")
  points(9, summary(cl11_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl11_all)}


# Cluster 12
{cl12_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10, (summary(cl12_all)$coefmat.full[1,1] - summary(cl12_all)$coefmat.full[1,3]),  
            x1=10, y1=(summary(cl12_all)$coefmat.full[1,1] + summary(cl12_all)$coefmat.full[1,3]), col="blue")
  points(10, summary(cl12_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl12_all)}


# Habitat heterogeneity    
segments(11, summary(gls_avg_all)$coefmat.full[2,1] - summary(gls_avg_all)$coefmat.full[2,3],
         x1=11, y1=summary(gls_avg_all)$coefmat.full[2,1] + summary(gls_avg_all)$coefmat.full[2,3], col="blue") 
points(11, summary(gls_avg_all)$coefmat.full[2,1], pch=20, col="blue")

legend("topright", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)


##-------####
##--- 3.4.4 Comparison of factorial variable levels ---####
##-----------------------------------------------------####
# Unfortunately, I have not succeeded in finding a way to do post hoc comparisons of factor levels for an averaged model.
# The following works for standard gls, however.
# Seeminly, I will just have to look at the model coefficients in the plot

#library(emmeans)
#emm_reds <- emmeans(gls_avg_reds, specs = "clusterCut")
#    pairs(emm_reds)
#    plot(emm_reds, comparisons=TRUE)
#    cld(emm_reds)
    
#emm_blacks <- emmeans(gls_avg_blacks, specs = "clusterCut" )



##--- 3.5 Making predictions from models  ---####
##-------------------------------------------####

# Now we want to try and make predictions on the number of either threatened or alien species based on
# the spatial models.

data_predict_clust <- TrdRast_clust[, c(1, 83, 85, 86, 87, 88)]

# Remove the grid cells with categories which cannot be used i the model (0 and 10)
data_predict_clust <- data_predict_clust[!data_predict_clust@data$clusterCut==0,]
data_predict_clust <- data_predict_clust[!data_predict_clust@data$clusterCut==10,]
data_predict_clust@data$clusterCut <- as.factor(data_predict_clust@data$clusterCut)

### Make the predictions for threatened, alien and all species:
#data_predict_clust$predict_reds <- predict(gls_clust_reds, newdata=data_predict_clust)
#data_predict_clust$predict_blacks <- predict(gls_clust_blacks, newdata=data_predict_clust)
#data_predict_clust$predict_all <- predict(gls_clust_all, newdata=data_predict_clust)

### Make the predictions for threatened, alien and all species with the averaged models:
    # Potentiallt run this again, of the predictions fail: gls_avg_blacks <- model.avg(output_black, fit=TRUE)
data_predict_clust$predict_reds.avg <- predict(gls_avg_reds, newdata=data_predict_clust)                      # add 'se.fit=TRUE' if you want the standars errors returned
data_predict_clust$predict_blacks.avg <- predict(gls_avg_blacks, newdata=data_predict_clust, full=TRUE)
data_predict_clust$predict_all.avg <- predict(gls_avg_all, newdata=data_predict_clust, full=TRUE)


#range(data_predict_clust$predict_reds)        
#range(data_predict_clust$predict_blacks)
#range(data_predict_clust$predict_all)

range(data_predict_clust$predict_reds.avg)        
range(data_predict_clust$predict_blacks.avg)
range(data_predict_clust$predict_all.avg)

# Back-transform the predictions:
#data_predict_clust$predict_blacks_number <- (exp(data_predict_clust@data$predict_blacks))-1
#data_predict_clust$predict_reds_number <- (exp(data_predict_clust@data$predict_reds))-1
#data_predict_clust$predict_all_number <- (exp(data_predict_clust@data$predict_all))-1

data_predict_clust$predict_blacks_number.avg <- (exp(data_predict_clust@data$predict_blacks.avg))-1
data_predict_clust$predict_reds_number.avg <- (exp(data_predict_clust@data$predict_reds.avg))-1
data_predict_clust$predict_all_number.avg <- (exp(data_predict_clust@data$predict_all.avg))-1

#range(data_predict_clust$predict_reds_number)
#range(data_predict_clust$predict_blacks_number)
#range(data_predict_clust$predict_all_number)

range(data_predict_clust$predict_reds_number.avg)
range(data_predict_clust$predict_blacks_number.avg)
range(data_predict_clust$predict_all_number.avg)




##--- 3.5.1 BETTER MAPS ---####
##--- OBS! model.avg    ---####
##-------------------------####
library(RColorBrewer)
col.reds <- colorRampPalette(c("white","#FF0000"))
col.blacks <- colorRampPalette(c("white","black"))
col.all <- colorRampPalette(c("white", "blue"))

layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1))  # Use this, if you want to make a single figure with all maps
layout(t(1:2),widths=c(9,1))    # Use this if you want to make individual maps - the margins are optimized for this

# All species:
palette(col.all(3655/4 +1))  
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "") #ESR of all species \nin 500m x 500m cell   # Or use: DivMap_3(AR5, Trondheim, "")
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant
plot(TrdRast_clust_model[, "S.chao1_2013"],
     col=TrdRast_clust_model@data$S.chao1_2013,
     border=TrdRast_clust_model@data$S.chao1_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=0:max(TrdRast_clust_model@data$S.chao1_2013),
      z=t(0:max(TrdRast_clust_model@data$S.chao1_2013)),
      col=palette(col.all(3655/4 +1)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0),
     labels = c(4, 500, 1000, 1500, 2000, 2500, 3000, 3500),
     at=c(4, 500, 1000, 1500, 2000, 2500, 3000, 3500))

palette(col.all(102))  # max(data_predict_clust@data$predict_all_number.avg)  
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "") #Modelled richness of all \nspecies in 500m x 500m cell  #Or use DivMap_4(AR5, Trondheim, "")
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant - the green is to ensure correct backgrounds
plot(data_predict_clust,
     col= data_predict_clust@data$predict_all_number.avg,
     border=data_predict_clust@data$predict_all_number.avg,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=(0:(max(data_predict_clust@data$predict_all_number.avg))),
      z=t(0:(max(data_predict_clust@data$predict_all_number.avg))),
      col=palette(col.all(102)), axes=FALSE, main="# \nspecies", cex.main=1, bg="forestgreen")
axis(4,cex.axis=1,mgp=c(0,.5,0))

# Threatened species:
palette(col.reds(37+1))
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "")  # ESR of threatened species \n in 500m x 500m cell
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant
plot(TrdRast_clust_model[, "S.chao1_reds_2013"],
     col=TrdRast_clust_model@data$S.chao1_reds_2013,
     border=TrdRast_clust_model@data$S.chao1_reds_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=0:37,z=t(0:37), col=palette(col.reds(37+1)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0))


palette(col.reds(159))   # max(data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10 
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "") #Modelled richness of threatened \nspecies in 500m x 500m cell  #Or use DivMap_4(AR5, Trondheim, "")
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant
plot(data_predict_clust,
     col= (data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10,
     border=(data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=min((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10) : max((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10),
      z=t(min((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10) : max((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10)),
      col=palette(col.reds(159)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0),
     labels=c("0", "2", "5", "10", "15"),
     at=c(0, 20, 50, 100, 150))

# Alien species:
palette(col.blacks(66+1))
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "")  #ESR of alien species \nin 500m x 500m cell
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant
plot(TrdRast_clust_model[, "S.chao1_blacks_2013"],
     col=TrdRast_clust_model@data$S.chao1_blacks_2013,
     border=TrdRast_clust_model@data$S.chao1_blacks_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=0:66,z=t(0:66), col=palette(col.blacks(66+1)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0))

palette(col.blacks(30))   # max(data_predict_clust@data$predict_blacks_number.avg + abs(min(data_predict_clust@data$predict_blacks_number.avg))) *10
#par(mar=c(0.5,0.5,6,0.5))
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "")  # Modelled richness of alien \nspecies in 500m x 500m cell  #Or use DivMap_4(AR5, Trondheim, "")
# DivMap_2(AR5, Trondheim, "")   # If the land/ocean border is irrelevant
plot(data_predict_clust,
     col=(data_predict_clust@data$predict_blacks_number.avg + abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10,
     border=(data_predict_clust@data$predict_blacks_number.avg + abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
#par(mar=c(0.5,0.5,6,4))
par(mar=c(2,0.01,4,2))
image(y=((min(data_predict_clust@data$predict_blacks_number.avg)+abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10):((max(data_predict_clust@data$predict_blacks_number.avg)+abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10),
      z=t(((min(data_predict_clust@data$predict_blacks_number.avg))*10):((max(data_predict_clust@data$predict_blacks_number.avg))*10)),
      col=palette(col.blacks(30)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4, cex.axis=1, mgp=c(0,.5,0),
     labels=c("0", "0.5", "1", "1.5", "2", "2.5", "3"),
     at=c(0,5,10,15,20,25,30))


##--- 4. EXTRA MODEL VALIDATION  ---####
##----------------------------------####
# Get the useful data for comparing the Observed species richness and the predicted species richness - 
# there should not be a correlation here. Otherwise, we see an effect of sampling effort

val.plot <- TrdRast_clust_model@data[, c("Pixelnr", "S.obs_2013", "S.chao1_2013", "S.obs_reds_2013", "S.chao1_reds_2013",
                                         "S.obs_blacks_2013", "S.chao1_blacks_2013", "Ntotal", "Nred", "Nblack")]
val.plot <- merge(val.plot, data_predict_clust@data[,c("Pixelnr", "predict_all.avg", "predict_reds.avg", "predict_blacks.avg",
                                                       "predict_all_number.avg", "predict_reds_number.avg", "predict_blacks_number.avg")],
                  by="Pixelnr")
val.plot[is.na(val.plot)] <- 0


par(mar=c(5,4,4,2))
par(mfrow=c(3,4))
cor(log(val.plot$Ntotal +1), val.plot$predict_all_number.avg)  # Correlation coefficient
    plot(log(val.plot$Ntotal +1), val.plot$predict_all_number.avg, main="coor = 0.23",
     ylab="Predicted # species, all", xlab="log(# records, all sp. + 1)")
      lines(smooth.spline(x=log(val.plot$Ntotal +1), y=val.plot$predict_all_number.avg))   # Spline
cor(val.plot$S.obs_2013, val.plot$predict_all_number.avg)
      plot(val.plot$S.obs_2013, val.plot$predict_all_number.avg, main="coor = 0.29",
     ylab="Predicted # species, all", xlab="Observed # species, all")
      lines(smooth.spline(x=val.plot$S.obs_2013, y=val.plot$predict_all_number.avg))    # Spline
    plot(0,type='n',axes=FALSE,ann=FALSE)
    plot(0,type='n',axes=FALSE,ann=FALSE)

cor(log(val.plot$Ntotal +1), val.plot$predict_reds_number.avg)
    plot(log(val.plot$Ntotal +1), val.plot$predict_reds_number.avg, main="coor = 0.33",
     ylab="Predicted # species, treatened", xlab="log(# records, all + 1)")
      lines(smooth.spline(x=log(val.plot$Ntotal +1), y=val.plot$predict_reds_number.avg))   # Spline
cor(log(val.plot$Nred +1), val.plot$predict_reds_number.avg)
    plot(log(val.plot$Nred +1), val.plot$predict_reds_number.avg, main="coor = 0.51",
     ylab="Predicted # species, treatened", xlab="log(# records, threatened + 1)")
      lines(smooth.spline(x=log(val.plot$Nred +1), y=val.plot$predict_reds_number.avg))   # Spline
cor(val.plot$S.obs_2013, val.plot$predict_reds_number.avg)
    plot(val.plot$S.obs_2013, val.plot$predict_reds_number.avg, main="coor = 0.31",
     ylab="Predicted # species, treatened", xlab="Observed # species, all")
      lines(smooth.spline(x=val.plot$S.obs_2013, y=val.plot$predict_reds_number.avg))   # Spline
cor(val.plot$S.obs_reds_2013, val.plot$predict_reds_number.avg)
    plot(val.plot$S.obs_reds_2013, val.plot$predict_reds_number.avg, main="coor = 0.58",
     ylab="Predicted # species, threatened", xlab="Observed # species, threatened")
      lines(smooth.spline(x=val.plot$S.obs_reds_2013, y=val.plot$predict_reds_number.avg))   # Spline

cor(log(val.plot$Ntotal +1), val.plot$predict_blacks_number.avg)
    plot(log(val.plot$Ntotal +1), val.plot$predict_blacks_number.avg, main="coor = 0.28",
     ylab="Predicted # species, alien", xlab="log(# records, all + 1)")
      lines(smooth.spline(x=log(val.plot$Ntotal +1), y=val.plot$predict_blacks_number.avg))   # Spline
cor(log(val.plot$Nblack +1), val.plot$predict_blacks_number.avg)
    plot(log(val.plot$Nblack +1), val.plot$predict_blacks_number.avg, main="coor = 0.49",
     ylab="Predicted # species, alien", xlab="log(# records, alien + 1)")
      lines(smooth.spline(x=log(val.plot$Nblack +1), y=val.plot$predict_blacks_number.avg))   # Spline
cor(val.plot$S.obs_2013, val.plot$predict_blacks_number.avg)
    plot(val.plot$S.obs_2013, val.plot$predict_blacks_number.avg, main="coor = 0.18",
     ylab="Predicted # species, alien", xlab="Observed # species, all")
      lines(smooth.spline(x=val.plot$S.obs_2013, y=val.plot$predict_blacks_number.avg))   # Spline
cor(val.plot$S.obs_blacks_2013, val.plot$predict_blacks_number.avg)
    plot(val.plot$S.obs_blacks_2013, val.plot$predict_blacks_number.avg, main="coor = 0.42",
     ylab="Predicted # species, alien", xlab="Observed # species, alien")
      lines(smooth.spline(x=val.plot$S.obs_blacks_2013, y=val.plot$predict_blacks_number.avg))   # Spline

###
par(mfrow=c(1,4))
plot(log(val.plot$Ntotal), val.plot$predict_blacks.avg, ylab="Predicted # species, alien", xlab="log(# records, all)")
plot(log(val.plot$Nblack), val.plot$predict_blacks.avg, ylab="Predicted # species, alien", xlab="log(# records, alien)")
plot(val.plot$S.obs_2013, val.plot$predict_blacks.avg, ylab="Predicted # species, alien", xlab="Observed # species, all")
plot(val.plot$S.obs_blacks_2013, val.plot$predict_blacks.avg, ylab="Predicted # species, alien", xlab="Observed # species, alien")


data_predict_clust$num.clusterCut <- as.numeric(data_predict_clust$clusterCut)  # For a numerical version of the cluster variable
MyVar_pred <- c("num.clusterCut", "Divers")
par(mfrow=c(1,1))
pairs(data_predict_clust@data[, MyVar_pred], 
      lower.panel = panel.cor) 

# Plot of sampling effort - maps coloured by the number of observed species (and potentially number of records?)
layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1))

# All species:
palette(col.all(232+1))
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# observed species, all")
plot(TrdRast_clust, add=TRUE,
     col=TrdRast_clust@data$S.obs_2013,
     border=TrdRast_clust@data$S.obs_2013)
plot(Trondheim, add=TRUE, lty=2)
par(mar=c(0.5,0.5,6,4))
image(y=0:232,z=t(0:232), col=palette(col.all(232+1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

palette(col.all(29089/30+1))   
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# records, all")
plot(TrdRast_clust, 
     col=TrdRast_clust@data$Ntotal,
     border=TrdRast_clust@data$Ntotal,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:max(TrdRast_clust@data$Ntotal),
      z=t(0:max(TrdRast_clust@data$Ntotal)),
      col=palette(col.all(29089/30+1)), axes=FALSE, main="# \nrecords", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels = c(0, 1000, 5000, 10000, 15000, 20000, 25000, 29000),
     at=c(0, 1000, 5000, 10000, 15000, 20000, 25000, 29000))

# Threatened species:
palette(col.reds(35+1))
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# observed species, threatened")
plot(TrdRast_clust, add=TRUE,
     col=TrdRast_clust@data$S.obs_reds_2013,
     border=TrdRast_clust@data$S.obs_reds_2013)
plot(Trondheim, add=TRUE, lty=2)
par(mar=c(0.5,0.5,6,4))
image(y=0:35,z=t(0:35), col=palette(col.reds(35+1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

palette(col.reds(6495/10+1))   
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# records, threatened")
plot(TrdRast_clust, 
     col=TrdRast_clust@data$Nred,
     border=TrdRast_clust@data$Nred,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:max(TrdRast_clust@data$Nred),
      z=t(0:max(TrdRast_clust@data$Nred)),
      col=palette(col.reds(6495/10+1)), axes=FALSE, main="# \nrecords", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels = c(0, 500, 1000, 2000, 3000, 4000, 5000, 6000),
     at=c(0, 500, 1000, 2000, 3000, 4000, 5000, 6000))

# Alien species:
palette(col.blacks(38+1))
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# observed species, alien")
plot(TrdRast_clust, add=TRUE,
     col=TrdRast_clust@data$S.obs_blacks_2013,
     border=TrdRast_clust@data$S.obs_blacks_2013)
plot(Trondheim, add=TRUE, lty=2)
par(mar=c(0.5,0.5,6,4))
image(y=0:38,z=t(0:38), col=palette(col.blacks(38+1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

palette(col.blacks(394+1))   
par(mar=c(0.5,0.5,6,0.5))
plot(Trondheim, lty=2, lwd=0.5, main="# records, alien")
plot(TrdRast_clust, 
     col=TrdRast_clust@data$Nblack,
     border=TrdRast_clust@data$Nblack,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:max(TrdRast_clust@data$Nblack),
      z=t(0:max(TrdRast_clust@data$Nblack)),
      col=palette(col.blacks(394+1)), axes=FALSE, main="# \nrecords", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))


### More formal testing of relative importance of the factor level variable:





##--- 5. MODELS WITH STANDARDIZED RESPONSE VARIABLES ---####
##--- 5.1 DATA EXPLORATION ---####
##----------------------------####
# To be able to compare the models even more, we can try to standardize the response variable - we do this by
# dividing all the y-values (log_chao) by the mean of the group. By doing this, we center everything around
# zero, get comparable values, and we can thus easier assess the differences and similarityes in coefficient
# values when the models are done.

# Standardize the response values - we do that by both scaling and centering:
TrdRast_clust_model$log_chao.all.std <- scale(TrdRast_clust_model$log_chao.all, center = TRUE, scale = TRUE)
TrdRast_clust_model$log_chao.reds.std <- scale(TrdRast_clust_model$log_chao.reds, center = TRUE, scale = TRUE) 
TrdRast_clust_model$log_chao.blacks.std <- scale(TrdRast_clust_model$log_chao.blacks, center = TRUE, scale = TRUE)
        # OBS! This needs to be done INCLUDING the "artificial zeros"! Otherwise we get outliers
        # destroying the appropriate scaling 

range(TrdRast_clust_model$log_chao.all.std)
range(TrdRast_clust_model$log_chao.reds.std)
range(TrdRast_clust_model$log_chao.blacks.std)

par(mfrow=c(1,3))
boxplot(TrdRast_clust_model@data$log_chao.all.std, main="All")
boxplot(TrdRast_clust_model@data$log_chao.reds.std, main="Threatened")
boxplot(TrdRast_clust_model@data$log_chao.blacks.std, main="Alien")

plot(x=TrdRast_clust_model@data$log_chao.all, y=TrdRast_clust_model@data$log_chao.all.std)
plot(x=TrdRast_clust_model@data$log_chao.reds, y=TrdRast_clust_model@data$log_chao.reds.std)
plot(x=TrdRast_clust_model@data$log_chao.blacks, y=TrdRast_clust_model@data$log_chao.blacks.std)

# An important point for why we're using the Chao-estimation rather than the raw species numbers, is to take sampling
# effort into account - hence, we have to make see if we have a correlation between number of observed species and the
# estimated numbers within the cells we're using for the analyses

# Outliers (just to double-check):
MyVar_std <- c("log_chao.all.std", "log_chao.reds.std", "log_chao.blacks.std",
           "clusterCut", "nhabitat", "Divers", "Evenness", "north.mean")

### Dotplots
Mydotplot(TrdRast_clust_model@data[,MyVar_std[-4]])   

### Colinearity
pairs(TrdRast_clust_model@data[, MyVar_std], 
      lower.panel = panel.cor)              

corvif(TrdRast_clust_model@data[, MyVar_std[-c(5,7)]]) # This would severely lower the GVIFs

### Relationships
MyVar_std2 <- c("log_chao.all.std", "log_chao.reds.std", "log_chao.blacks.std",
            "clusterCut", "Divers", "north.mean")

Myxyplot(TrdRast_clust_model@data, MyVar_std2, "log_chao.reds", 
         MyYlab = "ESR of redlisted species (m^2)")
Myxyplot(TrdRast_clust_model@data, MyVar_std2, "log_chao.blacks", 
         MyYlab = "ESR of alien species (m^2)")
Myxyplot(TrdRast_clust_model@data, MyVar_std2, "log_chao.all", 
         MyYlab = "ESR of all species (m^2)")


##--- 5.2   PRELIMINARY MODELLING (NON-SPATIAL) ---####
##--- 5.2.1  Model 1 - threatened species       ---####
##-------------------------------------------------####

global_M1.std <- glm(log_chao.reds.std ~  clusterCut + Divers + north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M1.std)       # Backwards selection using AIC
rm(global_M1.std)

# Define the "better" models:
M1.std <- glm(log_chao.reds.std ~  clusterCut + Divers,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M1.std)


##--- 5.2.2  Model 2 - alien species ---####
##---------------------------------------####
global_M2.std <- glm(log_chao.blacks.std ~  clusterCut + Divers +  north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M2.std)       # Backwards selection using AIC
rm(global_M2.std)

M2.std <- glm(log_chao.blacks.std ~  clusterCut + Divers + north.mean,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M2.std)


##--- 5.2.3  Model 3 - all species ---####
##------------------------------------####
global_M3.std <- glm(log_chao.all.std ~  clusterCut + Divers +  north.mean,
                 family = "gaussian",
                 data = TrdRast_clust_model@data)

### Model validation: Is everything significant?
step(global_M3.std)       # Backwards selection using AIC
rm(global_M3.std)

M3.std <- glm(log_chao.all.std ~  clusterCut + Divers,
          family = "gaussian",
          data = TrdRast_clust_model@data)

summary(M3.std)


##--- 5.3 SPATIAL AUTOCORRELATION- threatened species ---####
##--- 5.3.1 Testing for SAC - Chao1_reds              ---####
##-------------------------------------------------------####

# Make a correlogram:
correlog1.std <- correlog(xy_clust[,1], xy_clust[,2], residuals(M1.std), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog1.std$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I, threatened species"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M1.std))/2+1.5], pch=19,
     cex=abs(resid(M1.std))/max(resid(M1.std))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# Look at it through Monte-Carlo simulation as well:
MC_res_clust.std <- moran.mc(residuals(M1.std), clust.listw,
                         zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)
MC_res_clust.std
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res_clust.std, main="Threatened species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res_clust.std$statistic, lty=2, col="red")

##--- 5.3.2 Testing for SAC - Chao1_blacks             ---####
##--------------------------------------------------------####

# Make a correlogram:
correlog2.std <- correlog(xy_clust[,1], xy_clust[,2], residuals(M2.std), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog2.std$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M2.std))/2+1.5], pch=19,
     cex=abs(resid(M2.std))/max(resid(M2.std))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# Look at it through Monte-Carlo simulation as well:
MC_res2_clust.std <- moran.mc(residuals(M2.std), clust.listw,
                          zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res2_clust.std, main="Alien species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res2_clust.std$statistic, lty=2, col="red")

# SAC in these model residuals

##--- 5.3.3 Testing for SAC - Chao1_all             ---####
##-----------------------------------------------------####

# Make a correlogram:
correlog3.std <- correlog(xy_clust[,1], xy_clust[,2], residuals(M3.std), na.rm = T, increment = 1, resamp = 0)

# Plot the first 20 distance classes
par(mfrow=c(1,1))
par(mar=c(5,5,0.1, 0.1))
plot(correlog3.std$correlation[1:20], type="b", pch=16, lwd=1.5,
     xlab="distance", ylab="Moran's I"); abline(h=0)

# Make a map of the residuals:
plot(xy_clust[,1], xy_clust[,2], col=c("blue", "red")[sign(resid(M3.std))/2+1.5], pch=19,
     cex=abs(resid(M3.std))/max(resid(M3.std))*2, xlab="geographical x- coordinates", ylab="geographical y-coordinates")

# Look at it through Monte-Carlo simulation as well:
MC_res3_clust.std <- moran.mc(residuals(M3.std), clust.listw,
                          zero.policy = TRUE, nsim=999)    # Using a Monce Carlo simulation (better!) (obs on the value of nsim)

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(MC_res3_clust.std, main="All species")     # Our value is way beyond the curve - high levels of SAC in the data!
abline(v=MC_res3_clust.std$statistic, lty=2, col="red")

# SAC in these model residuals


##--- 5.4 DEALING WITH SAC ---####
##--- 5.4.1 Chao1_reds     ---####
##----------------------------####
summary(gls_ML_clust.std <- gls(log_chao.reds.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

#stepAIC(gls_ML_clust.std)              # For unknown reasons, the standard 'step()' doesn't work - this one does
d.red.std <- dredge(gls_ML_clust.std)
d.red2.std <- subset(d.red.std, delta<2)
model.sel(d.red2.std)
output_red.std <- model.sel(d.red2.std)


# According to the SAC-function, the best model is: log_chao.reds ~ clusterCut + Divers
# This is similar to the optimal model for the non-spatial approach 

# Define the better model(s):
summary(gls_clust_reds.std <- gls(log_chao.reds.std ~  clusterCut + Divers,
                              data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                              method = "ML"))

# To keep consistency in naming of models:
gls_avg_reds.std <- gls(log_chao.reds.std ~  clusterCut + Divers,
                    data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                    method = "ML")   


AIC(M1.std, gls_clust_reds.std)              # The model is seemingly slightly better, according to AIC. 

# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
            method = "ML"))


##--- 5.4.2 Chao1_blacks    ---####
##-----------------------------####

summary(gls.b_ML_clust.std <- gls(log_chao.blacks.std ~  clusterCut + Divers + north.mean,
                              data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

#stepAIC(gls.b_ML_clust.std)        # log_chao.blacks ~ clusterCut + Divers
d.black.std <- dredge(gls.b_ML_clust.std)
d.black2.std <- subset(d.black.std, delta<2)
model.sel(d.black2.std)
output_black.std <- model.sel(d.black2.std)
# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_blacks.std <- model.avg(output_black.std, fit=TRUE)   

gls_avg_blacks.std$coefficients
summary(gls_avg_blacks.std)   

summary(gls_avg_blacks.std)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model) rather than the conditional
                                          # as this is better when we are interested in the importance of variables (Grueber et al. 2011)
summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))),
                  subset=delta<2, fit=TRUE))$coefmat.full


# This is somewhat similar to the optimal model for the non-spatial approach, minus the north.mean

# Define the better model and compare AIC:
#summary(gls_clust_blacks.std <- gls(log_chao.blacks.std ~  clusterCut + Divers,
#                                data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#                                method = "ML"))

#AIC(M2.std, gls_clust_blacks.std)


# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
#summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
#            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#            method = "ML"))
model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)


##--- 5.4.3 Chao1_all    ---####
##--------------------------####

summary(gls.a_ML_clust.std <- gls(log_chao.all.std ~  clusterCut + Divers + north.mean,
                              data=TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML"))

#stepAIC(gls.a_ML_clust.std)        # log_chao.all ~ clusterCut + Divers
d.all.std <- dredge(gls.a_ML_clust.std)
d.all2.std <- subset(d.all.std, delta<2)
model.sel(d.all2.std)
output_all.std <- model.sel(d.all2.std)
# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_all.std <- model.avg(output_all.std, fit=TRUE)   

gls_avg_all.std$coefficients
summary(gls_avg_all.std)   

summary(gls_avg_all.std)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model) rather than the conditional
                                       # as this is better when we are interested in the importance of variables (Grueber et al. 2011)
summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))),
                  subset=delta<2, fit=TRUE))$coefmat.full

# Define the better model and compare AIC:
#summary(gls_clust_all.std <- gls(log_chao.all.std ~  clusterCut + Divers,
#                             data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#                             method = "ML"))

#AIC(M3.std, gls_clust_all.std)

# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
#summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
#            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
#            method = "ML"))
model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)


##--- 5.4.3 Model validation ---####
##------------------------------####
summary(gls_clust_reds.std)
summary(gls_clust_blacks.std)
summary(gls_clust_all.std)

# Pseudo-R^2 calculated as correlation between observed and predicted values - this is what was done in
# in the Ballesteros-Meija paper - I am uncertain whether is is desirable for me.
# Make the calculations (and potentially compare):
cor(pseudo$log_chao.reds.std, pseudo$predict_reds.std.avg)       # 0.594   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.reds.std, pseudo$predict_reds.std.avg))^2   # 0.353

cor(pseudo$log_chao.blacks.std, pseudo$predict_blacks.std.avg)       # 0.473   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.blacks.std, pseudo$predict_blacks.std.avg))^2   # 0.224

cor(pseudo$log_chao.all.std, pseudo$predict_all.std.avg)       # 0.304   # As in Ballesteros-Meija et al.
(cor(pseudo$log_chao.all.std, pseudo$predict_all.std.avg))^2   # 0.093


# Plot residuals vs fitted values (gls.exp_clust_reds)
F1_threat.std <- fitted(gls_clust_reds.std)
E1_threat.std <- resid(gls_clust_reds.std, type = "pearson")      
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_threat.std, 
     y = E1_threat.std,
     xlab = "Fitted values - threat.",
     ylab = "Pearson residuals - threat.",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_threat.std <- E1_threat.std
Myxyplot(TrdRast_clust_model@data, MyVar_std2[-4], "Resid_threat.std")
TrdRast_clust_model@data$Resid_threat.std <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_threat.std)


# Plot residuals vs fitted values (gls.exp_clust_blacks)
F1_alien.std <- fitted(gls_clust_blacks.std)
E1_alien.std <- resid(gls_clust_blacks.std, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_alien.std, 
     y = E1_alien.std,
     xlab = "Fitted values - alien",
     ylab = "Pearson residuals - alien",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_alien.std <- E1_alien.std
Myxyplot(TrdRast_clust_model@data, MyVar_std2[-4], "Resid_alien.std")
TrdRast_clust_model@data$Resid_alien.std <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_alien.std)

# Plot residuals vs fitted values (gls.exp_clust_all)
F1_all.std <- fitted(gls_clust_all.std)
E1_all.std <- resid(gls_clust_all.std, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_all.std, 
     y = E1_all.std,
     xlab = "Fitted values - all",
     ylab = "Pearson residuals - all",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_all.std <- E1_all.std
Myxyplot(TrdRast_clust_model@data, MyVar_std2[-4], "Resid_all.std")
TrdRast_clust_model@data$Resid_all.std <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_all.std)

# QQ-plots
qqnorm(gls_clust_reds.std, abline=c(0,1))
qqnorm(gls_clust_blacks.std, abline=c(0,1))
qqnorm(gls_clust_all.std, abline=c(0,1))


##--- 5.4.3.1 Plots of model coefficients ---####
##-------------------------------------------####
# Plot of Cluster coefficients and their std.error - the coefficients and their std.errors are picked up by
# refitting the models with a new reference level. The coefficient is the same across all models (baseline+correction),
# the std.errors are from the model in which that factor level is baseline
par(mfrow=c(1,1))
par(mar=c(10,4.1,0.5,2.1))
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(1, 11), ylim=c(-3.5, 2), xaxt="n")
axis(1, at=c(1:11), labels=c("(1) Coastal", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
                             "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
                             "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
                             "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
                             "(12) Freshwater", "Habitat \nheterogeneity"), las=2)
abline(h=0, lty=2, col="gray")
legend("top", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)


### Threatened ####
# Cluster 1
segments( 0.9, coef(summary(gls_clust_reds.std))[1,1] - coef(summary(gls_clust_reds.std))[1,2],  
          x1=0.9, y1=coef(summary(gls_clust_reds.std))[1,1] + coef(summary(gls_clust_reds.std))[1,2], col="red")
points(0.9, (coef(summary(gls_clust_reds.std))[1,1]), pch=20, col="red")
# Cluster 2
segments( 1.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=1.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(1.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")
# Cluster 3
segments( 2.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(2.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 4
segments( 3.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(3.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 5
segments( 4.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(4.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 6
segments( 5.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(5.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 7
segments( 6.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(6.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 8
segments( 7.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(7.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 11
segments( 8.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(8.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 12
segments( 9.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(9.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

segments(10.9, coef(summary(gls_clust_reds.std))[11,1] - coef(summary(gls_clust_reds.std))[11,2],  # Habitat diversity
         x1=10.9, y1=coef(summary(gls_clust_reds.std))[11,1] + coef(summary(gls_clust_reds.std))[11,2], col="red")
points(10.9, (coef(summary(gls_clust_reds.std))[11,1]), pch=20, col="red")


## Alien ####
# Cluster 1
segments( 1.1, coef(summary(gls_clust_blacks.std))[1,1] - coef(summary(gls_clust_blacks.std))[1,2],  
          x1=1.1, y1=coef(summary(gls_clust_blacks.std))[1,1] + coef(summary(gls_clust_blacks.std))[1,2], col="black")
points(1.1, (coef(summary(gls_clust_blacks.std))[1,1]), pch=20, col="black")

# Cluster 2
segments( 2.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(2.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
# Cluster 3
segments( 3.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(3.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 4
segments( 4.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(4.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 5
segments( 5.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(5.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 6
segments( 6.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(6.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 7
segments( 7.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(7.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 8
segments( 8.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(8.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 11
segments( 9.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(9.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")

# Cluster 12
segments( 10.1, (coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=10.1, y1=(coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(10.1, coef(summary(gls(log_chao.blacks.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="black")
# Habitat heterogeneity    
segments( 11.1, coef(summary(gls_clust_blacks.std))[11,1] - coef(summary(gls_clust_blacks.std))[11,2],
          x1=11.1, y1=coef(summary(gls_clust_blacks.std))[11,1] + coef(summary(gls_clust_blacks.std))[11,2])
points(11.1, (coef(summary(gls_clust_blacks.std))[11,1]), pch=20)


### AVG.model

# Cluster 1
segments( 1.1, summary(gls_avg_blacks.std)$coefmat.full[1,1] - summary(gls_avg_blacks.std)$coefmat.full[1,3],  
          x1=1.1, y1=summary(gls_avg_blacks.std)$coefmat.full[1,1] + summary(gls_avg_blacks.std)$coefmat.full[1,3], col="black")
points(1.1, (summary(gls_avg_blacks.std)$coefmat.full[1,1]), pch=20, col="black")

# Cluster 2 
{cl2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments( 2.1, (summary(cl2_blacks)$coefmat.full[1,1] - summary(cl2_blacks)$coefmat.full[1,3]),  
            x1=2.1, y1=(summary(cl2_blacks)$coefmat.full[1,1] + summary(cl2_blacks)$coefmat.full[1,3]), col="black")
  points(2.1, summary(cl2_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl2_blacks)}

# Cluster 3
{cl3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3.1, (summary(cl3_blacks)$coefmat.full[1,1] - summary(cl3_blacks)$coefmat.full[1,3]),  
            x1=3.1, y1=(summary(cl3_blacks)$coefmat.full[1,1] + summary(cl3_blacks)$coefmat.full[1,3]), col="black")
  points(3.1, summary(cl3_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl3_blacks)}


# Cluster 4
{cl4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4.1, (summary(cl4_blacks)$coefmat.full[1,1] - summary(cl4_blacks)$coefmat.full[1,3]),  
            x1=4.1, y1=(summary(cl4_blacks)$coefmat.full[1,1] + summary(cl4_blacks)$coefmat.full[1,3]), col="black")
  points(4.1, summary(cl4_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl4_blacks)}


# Cluster 5
{cl5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5.1, (summary(cl5_blacks)$coefmat.full[1,1] - summary(cl5_blacks)$coefmat.full[1,3]),  
            x1=5.1, y1=(summary(cl5_blacks)$coefmat.full[1,1] + summary(cl5_blacks)$coefmat.full[1,3]), col="black")
  points(5.1, summary(cl5_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl5_blacks)}


# Cluster 6
{cl6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6.1, (summary(cl6_blacks)$coefmat.full[1,1] - summary(cl6_blacks)$coefmat.full[1,3]),  
            x1=6.1, y1=(summary(cl6_blacks)$coefmat.full[1,1] + summary(cl6_blacks)$coefmat.full[1,3]), col="black")
  points(6.1, summary(cl6_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl6_blacks)}


# Cluster 7
{cl7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7.1, (summary(cl7_blacks)$coefmat.full[1,1] - summary(cl7_blacks)$coefmat.full[1,3]),  
            x1=7.1, y1=(summary(cl7_blacks)$coefmat.full[1,1] + summary(cl7_blacks)$coefmat.full[1,3]), col="black")
  points(7.1, summary(cl7_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl7_blacks)}


# Cluster 8
{cl8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8.1, (summary(cl8_blacks)$coefmat.full[1,1] - summary(cl8_blacks)$coefmat.full[1,3]),  
            x1=8.1, y1=(summary(cl8_blacks)$coefmat.full[1,1] + summary(cl8_blacks)$coefmat.full[1,3]), col="black")
  points(8.1, summary(cl8_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl8_blacks)}


# Cluster 11
{cl11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9.1, (summary(cl11_blacks)$coefmat.full[1,1] - summary(cl11_blacks)$coefmat.full[1,3]),  
            x1=9.1, y1=(summary(cl11_blacks)$coefmat.full[1,1] + summary(cl11_blacks)$coefmat.full[1,3]), col="black")
  points(9.1, summary(cl11_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl11_blacks)}


# Cluster 12
{cl12_blacks <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10.1, (summary(cl12_blacks)$coefmat.full[1,1] - summary(cl12_blacks)$coefmat.full[1,3]),  
            x1=10.1, y1=(summary(cl12_blacks)$coefmat.full[1,1] + summary(cl12_blacks)$coefmat.full[1,3]), col="black")
  points(10.1, summary(cl12_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl12_blacks)}


# Habitat heterogeneity    
segments(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1] - summary(gls_avg_blacks.std)$coefmat.full[11,3],
         x1=11.1, y1=summary(gls_avg_blacks.std)$coefmat.full[11,1] + summary(gls_avg_blacks.std)$coefmat.full[11,3]) 
points(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1], pch=20)


## All ####
# Cluster 1
segments( 1, coef(summary(gls_clust_all.std))[1,1] - coef(summary(gls_clust_all.std))[1,2],  
          x1=1, y1=coef(summary(gls_clust_all.std))[1,1] + coef(summary(gls_clust_all.std))[1,2], col="blue")
points(1, (coef(summary(gls_clust_all.std))[1,1]), pch=20, col="blue")

# Cluster 2
segments( 2, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(2, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")
# Cluster 3
segments( 3, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(3, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 4
segments( 4, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(4, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 5
segments( 5, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(5, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 6
segments( 6, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(6, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 7
segments( 7, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(7, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 8
segments( 8, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(8, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 11
segments( 9, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                      coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(9, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Cluster 12
segments( 10, (coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=10, y1=(coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                      correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(10, coef(summary(gls(log_chao.all.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="blue")

# Hab. heterogenity    
segments( 11, coef(summary(gls_clust_all.std))[11,1] - coef(summary(gls_clust_all.std))[11,2],  
          x1=11, y1=coef(summary(gls_clust_all.std))[11,1] + coef(summary(gls_clust_all.std))[11,2], col="blue")
points(11, (coef(summary(gls_clust_all.std))[11,1]), pch=20, col="blue")


### AVG.model

# Cluster 1
segments( 1, summary(gls_avg_all.std)$coefmat.full[1,1] - summary(gls_avg_all.std)$coefmat.full[1,3],  
          x1=1, y1=summary(gls_avg_all.std)$coefmat.full[1,1] + summary(gls_avg_all.std)$coefmat.full[1,3], col="blue")
points(1, (summary(gls_avg_all.std)$coefmat.full[1,1]), pch=20, col="blue")

# Cluster 2 
{cl2_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments( 2, (summary(cl2_all)$coefmat.full[1,1] - summary(cl2_all)$coefmat.full[1,3]),  
            x1=2, y1=(summary(cl2_all)$coefmat.full[1,1] + summary(cl2_all)$coefmat.full[1,3]), col="blue")
  points(2, summary(cl2_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl2_all)}

# Cluster 3
{cl3_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3, (summary(cl3_all)$coefmat.full[1,1] - summary(cl3_all)$coefmat.full[1,3]),  
            x1=3, y1=(summary(cl3_all)$coefmat.full[1,1] + summary(cl3_all)$coefmat.full[1,3]), col="blue")
  points(3, summary(cl3_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl3_all)}


# Cluster 4
{cl4_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4, (summary(cl4_all)$coefmat.full[1,1] - summary(cl4_all)$coefmat.full[1,3]),  
            x1=4, y1=(summary(cl4_all)$coefmat.full[1,1] + summary(cl4_all)$coefmat.full[1,3]), col="blue")
  points(4, summary(cl4_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl4_all)}


# Cluster 5
{cl5_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5, (summary(cl5_all)$coefmat.full[1,1] - summary(cl5_all)$coefmat.full[1,3]),  
            x1=5, y1=(summary(cl5_all)$coefmat.full[1,1] + summary(cl5_all)$coefmat.full[1,3]), col="blue")
  points(5, summary(cl5_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl5_all)}


# Cluster 6
{cl6_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6, (summary(cl6_all)$coefmat.full[1,1] - summary(cl6_all)$coefmat.full[1,3]),  
            x1=6, y1=(summary(cl6_all)$coefmat.full[1,1] + summary(cl6_all)$coefmat.full[1,3]), col="blue")
  points(6, summary(cl6_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl6_all)}


# Cluster 7
{cl7_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7, (summary(cl7_all)$coefmat.full[1,1] - summary(cl7_all)$coefmat.full[1,3]),  
            x1=7, y1=(summary(cl7_all)$coefmat.full[1,1] + summary(cl7_all)$coefmat.full[1,3]), col="blue")
  points(7, summary(cl7_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl7_all)}


# Cluster 8
{cl8_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8, (summary(cl8_all)$coefmat.full[1,1] - summary(cl8_all)$coefmat.full[1,3]),  
            x1=8, y1=(summary(cl8_all)$coefmat.full[1,1] + summary(cl8_all)$coefmat.full[1,3]), col="blue")
  points(8, summary(cl8_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl8_all)}


# Cluster 11
{cl11_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9, (summary(cl11_all)$coefmat.full[1,1] - summary(cl11_all)$coefmat.full[1,3]),  
            x1=9, y1=(summary(cl11_all)$coefmat.full[1,1] + summary(cl11_all)$coefmat.full[1,3]), col="blue")
  points(9, summary(cl11_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl11_all)}


# Cluster 12
{cl12_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10, (summary(cl12_all)$coefmat.full[1,1] - summary(cl12_all)$coefmat.full[1,3]),  
            x1=10, y1=(summary(cl12_all)$coefmat.full[1,1] + summary(cl12_all)$coefmat.full[1,3]), col="blue")
  points(10, summary(cl12_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl12_all)}


# Habitat heterogeneity    
segments(11, summary(gls_avg_all.std)$coefmat.full[2,1] - summary(gls_avg_all.std)$coefmat.full[2,3],
         x1=11, y1=summary(gls_avg_all.std)$coefmat.full[2,1] + summary(gls_avg_all.std)$coefmat.full[2,3], col="blue") 
points(11, summary(gls_avg_all.std)$coefmat.full[2,1], pch=20, col="blue")

legend("topright", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)


##-------####

##--- 5.5 Making predictions from models  ---####
##-------------------------------------------####
### Make the predictions for threatened, alien and all species:
#data_predict_clust$predict_reds.std <- predict(gls_clust_reds.std, newdata=data_predict_clust)
#data_predict_clust$predict_blacks.std <- predict(gls_clust_blacks.std, newdata=data_predict_clust)
#data_predict_clust$predict_all.std <- predict(gls_clust_all.std, newdata=data_predict_clust)

#range(data_predict_clust$predict_reds.std)        
#range(data_predict_clust$predict_blacks.std)
#range(data_predict_clust$predict_all.std)

# Avg. models
data_predict_clust$predict_reds.std.avg <- predict(gls_avg_reds.std, newdata=data_predict_clust, full=TRUE)
data_predict_clust$predict_blacks.std.avg <- predict(gls_avg_blacks.std, newdata=data_predict_clust, full=TRUE)
data_predict_clust$predict_all.std.avg <- predict(gls_avg_all.std, newdata=data_predict_clust, full=TRUE)

range(data_predict_clust$predict_reds.std.avg)        
range(data_predict_clust$predict_blacks.std.avg)
range(data_predict_clust$predict_all.std.avg)


# Since we now have standardized number, it doesn't really make sense to try and back-transform the numbers anymore

##--- 5.5.3 BETTER MAPS ---####
##-------------------------####

layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1)) 

# All species:
palette(col.all(3655/4 +1))  
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "ESR of all species \nin 500m x 500m cell")
plot(TrdRast_clust_model[, "S.chao1_2013"],
     col=TrdRast_clust_model@data$S.chao1_2013,
     border=TrdRast_clust_model@data$S.chao1_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:max(TrdRast_clust_model@data$S.chao1_2013),
      z=t(0:max(TrdRast_clust_model@data$S.chao1_2013)),
      col=palette(col.all(3655/4 +1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels = c(4, 500, 1000, 1500, 2000, 2500, 3000, 3500),
     at=c(4, 500, 1000, 1500, 2000, 2500, 3000, 3500))

palette(col.all(137))   # max(data_predict_clust@data$predict_all.std.avg - min(data_predict_clust@data$predict_all.std.avg)) *100
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of all \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col= (data_predict_clust@data$predict_all.std.avg - min(data_predict_clust@data$predict_all.std.avg))*100 ,
     border=(data_predict_clust@data$predict_all.std.avg - min(data_predict_clust@data$predict_all.std.avg))*100 ,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=( (min(data_predict_clust@data$predict_all.std.avg)*100) : (max(data_predict_clust@data$predict_all.std.avg)*100) ),
      z=t( (min(data_predict_clust@data$predict_all.std.avg)*100) : (max(data_predict_clust@data$predict_all.std.avg)*100) ),
      col=palette(col.all(137)), axes=FALSE, main="Standardized. \nlog(chao + 1)", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels=c("-1.5", "-1", "-0.5", "0", "0.5"),
     at=c(-150,-100, -50,0,50 ))

# Threatened species:
palette(col.reds(37+1))
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "ESR of threatened species \n in 500m x 500m cell")
plot(TrdRast_clust_model[, "S.chao1_reds_2013"],
     col=TrdRast_clust_model@data$S.chao1_reds_2013,
     border=TrdRast_clust_model@data$S.chao1_reds_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:37,z=t(0:37), col=palette(col.reds(37+1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))


palette(col.reds(312))    # max(data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg))) *100
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col= (data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
     border=(data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=min((data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100) : max((data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100),
      z=t(min((data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100) : max((data_predict_clust@data$predict_reds.std.avg + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100)),
      col=palette(col.reds(312)), axes=FALSE, main="Standardized \nlog(chao + 1)", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels=c("-1.5", "-1", "-0.5", "0", "0.5", "1", "1.5"),
     at=c((-1.5 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (-1 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (-0.5 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (0 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (0.5 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (1 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100,
          (1.5 + abs(min(data_predict_clust@data$predict_reds.std.avg)))*100))

# Alien species:
palette(col.blacks(66+1))   
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "ESR of alien species \nin 500m x 500m cell")
plot(TrdRast_clust_model[, "S.chao1_blacks_2013"],
     col=TrdRast_clust_model@data$S.chao1_blacks_2013,
     border=TrdRast_clust_model@data$S.chao1_blacks_2013,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:66,z=t(0:66), col=palette(col.blacks(66+1)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

palette(col.blacks(141))   # max(data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg))) *100
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col= (data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
     border=(data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2)
par(mar=c(0.5,0.5,6,4))
image(y=min((data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100) : max((data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100),
      z=t(min((data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100) : max((data_predict_clust@data$predict_blacks.std.avg + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100)),
      col=palette(col.reds(141)), axes=FALSE, main="Standardized \nlog(chao + 1)", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels=c("-1.5", "-1", "-0.5", "0", "0.5", "1"),
     at=c((-1.5 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
          (-1 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
          (-0.5 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
          (0 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
          (0.5 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100,
          (1 + abs(min(data_predict_clust@data$predict_blacks.std.avg)))*100))


##--- 6. COMPARISON OF RELATIVE IMPORTANCE ---####
##---------------------------------------------####
# I have yet to find a way to calculate this for a gls


layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(5,1))

palette(col.all(29089/100))  
par(mar=c(0.5,0.5,6,0.5))
plot(TrdRast_AR5[, "S.chao1_2013"], main="# of records",
     col=TrdRast_AR5@data$Ntotal/100,
     border=TrdRast_AR5@data$Ntotal/100,
     cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=0:max(TrdRast_AR5@data$Ntotal[!is.na(TrdRast_AR5@data$Ntotal)]/100),
      z=t(0:max(TrdRast_AR5@data$Ntotal[!is.na(TrdRast_AR5@data$Ntotal)]/100)),
      col=palette(col.all(29089/100)), axes=FALSE, main="# \nrecords", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0),
     labels = c(5000, 10000, 15000, 20000, 25000),
     at=c(50, 100, 150, 200, 250))



##--- 7. FIGURES FOR POSTER AND PUBLICATION ---####
##--- 7.1 Prediction maps                   ---####
##---------------------------------------------####
library(RColorBrewer)
col.reds <- colorRampPalette(c("white","#FF0000"))
col.blacks <- colorRampPalette(c("white","black"))
col.all <- colorRampPalette(c("white", "blue"))

# All species:
pdf("Prediction_all.pdf", width = 23/2.54)  # Create the pdf, cm/2.54 (R calculates in inches) #
palette(col.all(102))  # max(data_predict_clust@data$predict_all_number.avg)  
#layout(t(1:2),widths=c(9,1))
par(mar=c(0.01,0.01,0.01,0.01))
par(ps = 32, cex = 1, cex.main = 1)         # Font sizes
DivMap(AR5, Trondheim, TrdRast_AR5, "") #Modelled richness of all \nspecies in 500m x 500m cell
plot(data_predict_clust,
     col= data_predict_clust@data$predict_all_number.avg,
     border=data_predict_clust@data$predict_all_number.avg,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
dev.off()

# The scalebar:
png("Prediction_all_scalebar_backgroundgreen.png", height = 16, width = 3.5, units = "cm",
    pointsize = 18, res=500)  # Create the png, 
par(mar=c(0.5,0.5,4,2))
par(bg="forestgreen")
image(y=(0:(max(data_predict_clust@data$predict_all_number.avg))),
      z=t(0:(max(data_predict_clust@data$predict_all_number.avg))),
      col=palette(col.all(102)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0))
dev.off()  


# Threatened species:
pdf("Prediction_threat.pdf", height = 16/2.54)  # Create the pdf 
palette(col.reds(159))
par(mar=c(0.01,0.01,0.01,0.01))
par(ps = 32, cex = 1, cex.main = 1)         # Font sizes
DivMap(AR5, Trondheim, TrdRast_AR5, "") #Modelled richness of threatened \nspecies in 500m x 500m cell
plot(data_predict_clust,
     col= (data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10,
     border=(data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2)
dev.off()

# The scalebar:
png("Prediction_threat_scale_backgroundgreen.png", height = 16, width = 3.5, units = "cm",
    pointsize = 18, res=500)  # Create the png, 
par(mar=c(0.5,0.5,4,2))
par(bg="forestgreen")
image(y=min((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10) : max((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10),
      z=t(min((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10) : max((data_predict_clust@data$predict_reds_number.avg + abs(min(data_predict_clust@data$predict_reds_number.avg)))*10)),
      col=palette(col.reds(159)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4,cex.axis=1,mgp=c(0,.5,0),
     labels=c("0", "2", "5", "10", "15"),
     at=c(0, 20, 50, 100, 150))
dev.off()

# Alien species:
pdf("Prediction_alien.pdf", height = 16/2.54)  # Create the pdf 
palette(col.blacks(30)) 
par(mar=c(0.01,0.01,0.01,0.01))
DivMap(AR5, Trondheim, TrdRast_AR5, "")  # Modelled richness of alien \nspecies in 500m x 500m cell
plot(data_predict_clust,
     col=(data_predict_clust@data$predict_blacks_number.avg + abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10,
     border=(data_predict_clust@data$predict_blacks_number.avg + abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
dev.off()

# The scalebar
png("Prediction_alien_scale_backgroundgreen.png", height = 16, width = 3.5, units = "cm",
    pointsize = 18, res=500)  # Create the png, 
par(mar=c(0.5,0.5,4,2))
par(bg="forestgreen")
image(y=((min(data_predict_clust@data$predict_blacks_number.avg)+abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10):((max(data_predict_clust@data$predict_blacks_number.avg)+abs(min(data_predict_clust@data$predict_blacks_number.avg)))*10),
      z=t(((min(data_predict_clust@data$predict_blacks_number.avg))*10):((max(data_predict_clust@data$predict_blacks_number.avg))*10)),
      col=palette(col.blacks(30)), axes=FALSE, main="# \nspecies", cex.main=1)
axis(4, cex.axis=1, mgp=c(0,.5,0),
     labels=c("0", "0.5", "1", "1.5", "2", "2.5", "3"),
     at=c(0,5,10,15,20,25,30))
dev.off()


##--- 7.2 Model coefficients (scaled) ---####
##---------------------------------------####

par(mfrow=c(1,1))
par(mar=c(9,4.1,0.5,2.1))
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(1, 11), ylim=c(-3, 2), xaxt="n")
axis(1, at=c(1:11), labels=FALSE)
text(x=c(1:11), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
     labels=c("(1) Coastal", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
              "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
              "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
              "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
              "(12) Freshwater", "Habitat \nheterogeneity"),
     srt=55, adj=1, xpd=TRUE)
#legend("top", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)
legend("top", legend=c("Threatened", "Alien", "All", "Mean", "Standard error"),
       lty=c(1,1,1,NA,1,3), pch=c(NA,NA,NA,20,NA),
       col=c("red", "black", "blue","gray40","gray40"), cex=0.75, ncol=2, y.intersp = 2)

## All ####
{# Cluster 1
segments( 1, summary(gls_avg_all.std)$coefmat.full[1,1] - summary(gls_avg_all.std)$coefmat.full[1,3],  
          x1=1, y1=summary(gls_avg_all.std)$coefmat.full[1,1] + summary(gls_avg_all.std)$coefmat.full[1,3], col="blue")
points(1, (summary(gls_avg_all.std)$coefmat.full[1,1]), pch=20, col="blue")

# Cluster 2 
{cl2_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments( 2, (summary(cl2_all)$coefmat.full[1,1] - summary(cl2_all)$coefmat.full[1,3]),  
            x1=2, y1=(summary(cl2_all)$coefmat.full[1,1] + summary(cl2_all)$coefmat.full[1,3]), col="blue")
  points(2, summary(cl2_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl2_all)}

# Cluster 3
{cl3_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3, (summary(cl3_all)$coefmat.full[1,1] - summary(cl3_all)$coefmat.full[1,3]),  
            x1=3, y1=(summary(cl3_all)$coefmat.full[1,1] + summary(cl3_all)$coefmat.full[1,3]), col="blue")
  points(3, summary(cl3_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl3_all)}


# Cluster 4
{cl4_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4, (summary(cl4_all)$coefmat.full[1,1] - summary(cl4_all)$coefmat.full[1,3]),  
            x1=4, y1=(summary(cl4_all)$coefmat.full[1,1] + summary(cl4_all)$coefmat.full[1,3]), col="blue")
  points(4, summary(cl4_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl4_all)}


# Cluster 5
{cl5_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5, (summary(cl5_all)$coefmat.full[1,1] - summary(cl5_all)$coefmat.full[1,3]),  
            x1=5, y1=(summary(cl5_all)$coefmat.full[1,1] + summary(cl5_all)$coefmat.full[1,3]), col="blue")
  points(5, summary(cl5_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl5_all)}


# Cluster 6
{cl6_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6, (summary(cl6_all)$coefmat.full[1,1] - summary(cl6_all)$coefmat.full[1,3]),  
            x1=6, y1=(summary(cl6_all)$coefmat.full[1,1] + summary(cl6_all)$coefmat.full[1,3]), col="blue")
  points(6, summary(cl6_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl6_all)}


# Cluster 7
{cl7_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7, (summary(cl7_all)$coefmat.full[1,1] - summary(cl7_all)$coefmat.full[1,3]),  
            x1=7, y1=(summary(cl7_all)$coefmat.full[1,1] + summary(cl7_all)$coefmat.full[1,3]), col="blue")
  points(7, summary(cl7_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl7_all)}


# Cluster 8
{cl8_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8, (summary(cl8_all)$coefmat.full[1,1] - summary(cl8_all)$coefmat.full[1,3]),  
            x1=8, y1=(summary(cl8_all)$coefmat.full[1,1] + summary(cl8_all)$coefmat.full[1,3]), col="blue")
  points(8, summary(cl8_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl8_all)}


# Cluster 11
{cl11_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9, (summary(cl11_all)$coefmat.full[1,1] - summary(cl11_all)$coefmat.full[1,3]),  
            x1=9, y1=(summary(cl11_all)$coefmat.full[1,1] + summary(cl11_all)$coefmat.full[1,3]), col="blue")
  points(9, summary(cl11_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl11_all)}


# Cluster 12
{cl12_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10, (summary(cl12_all)$coefmat.full[1,1] - summary(cl12_all)$coefmat.full[1,3]),  
            x1=10, y1=(summary(cl12_all)$coefmat.full[1,1] + summary(cl12_all)$coefmat.full[1,3]), col="blue")
  points(10, summary(cl12_all)$coefmat.full[1,1], pch=20, col="blue")
  rm(cl12_all)}


# Habitat heterogeneity    
segments(11, summary(gls_avg_all.std)$coefmat.full[2,1] - summary(gls_avg_all.std)$coefmat.full[2,3],
         x1=11, y1=summary(gls_avg_all.std)$coefmat.full[2,1] + summary(gls_avg_all.std)$coefmat.full[2,3], col="blue") 
points(11, summary(gls_avg_all.std)$coefmat.full[2,1], pch=20, col="blue")}
### Threatened ####
{# Cluster 1
segments( 0.9, coef(summary(gls_clust_reds.std))[1,1] - coef(summary(gls_clust_reds.std))[1,2],  
          x1=0.9, y1=coef(summary(gls_clust_reds.std))[1,1] + coef(summary(gls_clust_reds.std))[1,2], col="red")
points(0.9, (coef(summary(gls_clust_reds.std))[1,1]), pch=20, col="red")
# Cluster 2
segments( 1.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=1.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(1.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")
# Cluster 3
segments( 2.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(2.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 4
segments( 3.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(3.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 5
segments( 4.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(4.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 6
segments( 5.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(5.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 7
segments( 6.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(6.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 8
segments( 7.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(7.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 11
segments( 8.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(8.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Cluster 12
segments( 9.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                  coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                        coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(9.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=20, col="red")

# Habitat diversity
segments(10.9, coef(summary(gls_clust_reds.std))[11,1] - coef(summary(gls_clust_reds.std))[11,2],  
         x1=10.9, y1=coef(summary(gls_clust_reds.std))[11,1] + coef(summary(gls_clust_reds.std))[11,2], col="red")
points(10.9, (coef(summary(gls_clust_reds.std))[11,1]), pch=20, col="red")}
## Alien ####
{# Cluster 1
segments( 1.1, summary(gls_avg_blacks.std)$coefmat.full[1,1] - summary(gls_avg_blacks.std)$coefmat.full[1,3],  
          x1=1.1, y1=summary(gls_avg_blacks.std)$coefmat.full[1,1] + summary(gls_avg_blacks.std)$coefmat.full[1,3], col="black")
points(1.1, (summary(gls_avg_blacks.std)$coefmat.full[1,1]), pch=20, col="black")

# Cluster 2 
{cl2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments( 2.1, (summary(cl2_blacks)$coefmat.full[1,1] - summary(cl2_blacks)$coefmat.full[1,3]),  
            x1=2.1, y1=(summary(cl2_blacks)$coefmat.full[1,1] + summary(cl2_blacks)$coefmat.full[1,3]), col="black")
  points(2.1, summary(cl2_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl2_blacks)}

# Cluster 3
{cl3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 3.1, (summary(cl3_blacks)$coefmat.full[1,1] - summary(cl3_blacks)$coefmat.full[1,3]),  
            x1=3.1, y1=(summary(cl3_blacks)$coefmat.full[1,1] + summary(cl3_blacks)$coefmat.full[1,3]), col="black")
  points(3.1, summary(cl3_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl3_blacks)}


# Cluster 4
{cl4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 4.1, (summary(cl4_blacks)$coefmat.full[1,1] - summary(cl4_blacks)$coefmat.full[1,3]),  
            x1=4.1, y1=(summary(cl4_blacks)$coefmat.full[1,1] + summary(cl4_blacks)$coefmat.full[1,3]), col="black")
  points(4.1, summary(cl4_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl4_blacks)}


# Cluster 5
{cl5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 5.1, (summary(cl5_blacks)$coefmat.full[1,1] - summary(cl5_blacks)$coefmat.full[1,3]),  
            x1=5.1, y1=(summary(cl5_blacks)$coefmat.full[1,1] + summary(cl5_blacks)$coefmat.full[1,3]), col="black")
  points(5.1, summary(cl5_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl5_blacks)}


# Cluster 6
{cl6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 6.1, (summary(cl6_blacks)$coefmat.full[1,1] - summary(cl6_blacks)$coefmat.full[1,3]),  
            x1=6.1, y1=(summary(cl6_blacks)$coefmat.full[1,1] + summary(cl6_blacks)$coefmat.full[1,3]), col="black")
  points(6.1, summary(cl6_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl6_blacks)}


# Cluster 7
{cl7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 7.1, (summary(cl7_blacks)$coefmat.full[1,1] - summary(cl7_blacks)$coefmat.full[1,3]),  
            x1=7.1, y1=(summary(cl7_blacks)$coefmat.full[1,1] + summary(cl7_blacks)$coefmat.full[1,3]), col="black")
  points(7.1, summary(cl7_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl7_blacks)}


# Cluster 8
{cl8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 8.1, (summary(cl8_blacks)$coefmat.full[1,1] - summary(cl8_blacks)$coefmat.full[1,3]),  
            x1=8.1, y1=(summary(cl8_blacks)$coefmat.full[1,1] + summary(cl8_blacks)$coefmat.full[1,3]), col="black")
  points(8.1, summary(cl8_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl8_blacks)}


# Cluster 11
{cl11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 9.1, (summary(cl11_blacks)$coefmat.full[1,1] - summary(cl11_blacks)$coefmat.full[1,3]),  
            x1=9.1, y1=(summary(cl11_blacks)$coefmat.full[1,1] + summary(cl11_blacks)$coefmat.full[1,3]), col="black")
  points(9.1, summary(cl11_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl11_blacks)}


# Cluster 12
{cl12_blacks <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 10.1, (summary(cl12_blacks)$coefmat.full[1,1] - summary(cl12_blacks)$coefmat.full[1,3]),  
            x1=10.1, y1=(summary(cl12_blacks)$coefmat.full[1,1] + summary(cl12_blacks)$coefmat.full[1,3]), col="black")
  points(10.1, summary(cl12_blacks)$coefmat.full[1,1], pch=20, col="black")
  rm(cl12_blacks)}


# Habitat heterogeneity    
segments(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1] - summary(gls_avg_blacks.std)$coefmat.full[11,3],
         x1=11.1, y1=summary(gls_avg_blacks.std)$coefmat.full[11,1] + summary(gls_avg_blacks.std)$coefmat.full[11,3]) 
points(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1], pch=20)
}
##-------####


##--- 7.3 Model coefficients (scaled and unscaled) ---####
##----------------------------------------------------####

# Layout doesn't work for this - instead, I need to make two figures!
# For now, only do the scaled one - if I need to do the inscaled as well, redefine the margins

# If a .png with fixed sizes are needed:
png("coefficients_poster2.png", height = 20, width = 25.5, units = "cm",
    pointsize = 18, res=500)  # Create the .png, 

## Unscaled ####
par(mar=c(0.5,4.1,0.5,2.1))
plot(1, type="n", xlab="", ylab="Model coefficient, unscaled response", xlim=c(1, 11), ylim=c(-1.5, 4), xaxt="n")
axis(1, at=c(1:11), labels=FALSE)
abline(h=0, lty=2, col="gray60")
text(x=11, y=3.5, labels=c("(a)"))

## All ####
{# Cluster 1
  segments( 1 , summary(gls_avg_all)$coefmat.full[1,1] - summary(gls_avg_all)$coefmat.full[1,3],  
            x1=1 , y1=summary(gls_avg_all)$coefmat.full[1,1] + summary(gls_avg_all)$coefmat.full[1,3], col="blue")
  points(1 , (summary(gls_avg_all)$coefmat.full[1,1]), pch=15, col="blue")
  
  # Cluster 2 
  {cl2_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
    segments( 2 , (summary(cl2_all)$coefmat.full[1,1] - summary(cl2_all)$coefmat.full[1,3]),  
              x1=2 , y1=(summary(cl2_all)$coefmat.full[1,1] + summary(cl2_all)$coefmat.full[1,3]), col="blue" )
    points(2 , summary(cl2_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl2_all)}
  
  # Cluster 3
  {cl3_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
    segments( 3 , (summary(cl3_all)$coefmat.full[1,1] - summary(cl3_all)$coefmat.full[1,3]),  
              x1=3 , y1=(summary(cl3_all)$coefmat.full[1,1] + summary(cl3_all)$coefmat.full[1,3]), col="blue" )
    points(3 , summary(cl3_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl3_all)}
  
  
  # Cluster 4
  {cl4_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
    segments( 4 , (summary(cl4_all)$coefmat.full[1,1] - summary(cl4_all)$coefmat.full[1,3]),  
              x1=4 , y1=(summary(cl4_all)$coefmat.full[1,1] + summary(cl4_all)$coefmat.full[1,3]), col="blue" )
    points(4 , summary(cl4_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl4_all)}
  
  
  # Cluster 5
  {cl5_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
    segments( 5 , (summary(cl5_all)$coefmat.full[1,1] - summary(cl5_all)$coefmat.full[1,3]),  
              x1=5 , y1=(summary(cl5_all)$coefmat.full[1,1] + summary(cl5_all)$coefmat.full[1,3]), col="blue" )
    points(5 , summary(cl5_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl5_all)}
  
  
  # Cluster 6
  {cl6_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
    segments( 6 , (summary(cl6_all)$coefmat.full[1,1] - summary(cl6_all)$coefmat.full[1,3]),  
              x1=6 , y1=(summary(cl6_all)$coefmat.full[1,1] + summary(cl6_all)$coefmat.full[1,3]), col="blue" )
    points(6 , summary(cl6_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl6_all)}
  
  
  # Cluster 7
  {cl7_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
    segments( 7 , (summary(cl7_all)$coefmat.full[1,1] - summary(cl7_all)$coefmat.full[1,3]),  
              x1=7 , y1=(summary(cl7_all)$coefmat.full[1,1] + summary(cl7_all)$coefmat.full[1,3]), col="blue" )
    points(7 , summary(cl7_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl7_all)}
  
  
  # Cluster 8
  {cl8_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
    segments( 8 , (summary(cl8_all)$coefmat.full[1,1] - summary(cl8_all)$coefmat.full[1,3]),  
              x1=8 , y1=(summary(cl8_all)$coefmat.full[1,1] + summary(cl8_all)$coefmat.full[1,3]), col="blue" )
    points(8 , summary(cl8_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl8_all)}
  
  
  # Cluster 11
  {cl11_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
    segments( 9 , (summary(cl11_all)$coefmat.full[1,1] - summary(cl11_all)$coefmat.full[1,3]),  
              x1=9 , y1=(summary(cl11_all)$coefmat.full[1,1] + summary(cl11_all)$coefmat.full[1,3]), col="blue" )
    points(9 , summary(cl11_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl11_all)}
  
  
  # Cluster 12
  {cl12_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
    segments( 10 , (summary(cl12_all)$coefmat.full[1,1] - summary(cl12_all)$coefmat.full[1,3]),  
              x1=10 , y1=(summary(cl12_all)$coefmat.full[1,1] + summary(cl12_all)$coefmat.full[1,3]), col="blue" )
    points(10 , summary(cl12_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl12_all)}
  
  # Habitat heterogeneity    
  segments(11 , summary(gls_avg_all)$coefmat.full[2,1] - summary(gls_avg_all)$coefmat.full[2,3],
           x1=11 , y1=summary(gls_avg_all)$coefmat.full[2,1] + summary(gls_avg_all)$coefmat.full[2,3], col="blue" ) 
  points(11, summary(gls_avg_all.std)$coefmat.full[2,1], pch=15, col="blue")} 

### Threatened ####
{# Cluster 1
  segments( 0.9, coef(summary(gls_avg_reds))[1,1] - coef(summary(gls_clust_reds))[1,2],  
            x1=0.9, y1=coef(summary(gls_clust_reds))[1,1] + coef(summary(gls_clust_reds))[1,2], col="red" )
  points(0.9, (coef(summary(gls_clust_reds))[1,1]), pch=16, col="red")
  
  # Cluster 2
  segments( 1.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=1.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(1.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  # Cluster 3
  segments( 2.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=2.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(2.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 4
  segments( 3.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=3.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(3.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 5
  segments( 4.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=4.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(4.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 6
  segments( 5.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=5.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(5.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 7
  segments( 6.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=6.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(6.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 8
  segments( 7.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=7.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(7.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 11
  segments( 8.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=8.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(8.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 12
  segments( 9.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=9.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red" )
  points(9.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Habitat diversity
  segments(10.9, coef(summary(gls_clust_reds))[11,1] - coef(summary(gls_clust_reds))[11,2],  
           x1=10.9, y1=coef(summary(gls_clust_reds))[11,1] + coef(summary(gls_clust_reds))[11,2], col="red" )
  points(10.9, (coef(summary(gls_clust_reds))[11,1]), pch=16, col="red")}

## Alien ####
{# Cluster 1
  segments( 1.1, summary(gls_avg_blacks)$coefmat.full[1,1] - summary(gls_avg_blacks)$coefmat.full[1,3],  
            x1=1.1, y1=summary(gls_avg_blacks)$coefmat.full[1,1] + summary(gls_avg_blacks)$coefmat.full[1,3], col="black" )
  points(1.1, (summary(gls_avg_blacks)$coefmat.full[1,1]), pch=17, col="black")
  
  # Cluster 2 
  {cl2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
    segments( 2.1, (summary(cl2_blacks)$coefmat.full[1,1] - summary(cl2_blacks)$coefmat.full[1,3]),  
              x1=2.1, y1=(summary(cl2_blacks)$coefmat.full[1,1] + summary(cl2_blacks)$coefmat.full[1,3]), col="black" )
    points(2.1, summary(cl2_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl2_blacks)}
  
  # Cluster 3
  {cl3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
    segments( 3.1, (summary(cl3_blacks)$coefmat.full[1,1] - summary(cl3_blacks)$coefmat.full[1,3]),  
              x1=3.1, y1=(summary(cl3_blacks)$coefmat.full[1,1] + summary(cl3_blacks)$coefmat.full[1,3]), col="black" )
    points(3.1, summary(cl3_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl3_blacks)}
  
  
  # Cluster 4
  {cl4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
    segments( 4.1, (summary(cl4_blacks)$coefmat.full[1,1] - summary(cl4_blacks)$coefmat.full[1,3]),  
              x1=4.1, y1=(summary(cl4_blacks)$coefmat.full[1,1] + summary(cl4_blacks)$coefmat.full[1,3]), col="black" )
    points(4.1, summary(cl4_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl4_blacks)}
  
  
  # Cluster 5
  {cl5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
    segments( 5.1, (summary(cl5_blacks)$coefmat.full[1,1] - summary(cl5_blacks)$coefmat.full[1,3]),  
              x1=5.1, y1=(summary(cl5_blacks)$coefmat.full[1,1] + summary(cl5_blacks)$coefmat.full[1,3]), col="black" )
    points(5.1, summary(cl5_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl5_blacks)}
  
  
  # Cluster 6
  {cl6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
    segments( 6.1, (summary(cl6_blacks)$coefmat.full[1,1] - summary(cl6_blacks)$coefmat.full[1,3]),  
              x1=6.1, y1=(summary(cl6_blacks)$coefmat.full[1,1] + summary(cl6_blacks)$coefmat.full[1,3]), col="black" )
    points(6.1, summary(cl6_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl6_blacks)}
  
  
  # Cluster 7
  {cl7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
    segments( 7.1, (summary(cl7_blacks)$coefmat.full[1,1] - summary(cl7_blacks)$coefmat.full[1,3]),  
              x1=7.1, y1=(summary(cl7_blacks)$coefmat.full[1,1] + summary(cl7_blacks)$coefmat.full[1,3]), col="black" )
    points(7.1, summary(cl7_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl7_blacks)}
  
  
  # Cluster 8
  {cl8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
    segments( 8.1, (summary(cl8_blacks)$coefmat.full[1,1] - summary(cl8_blacks)$coefmat.full[1,3]),  
              x1=8.1, y1=(summary(cl8_blacks)$coefmat.full[1,1] + summary(cl8_blacks)$coefmat.full[1,3]), col="black" )
    points(8.1, summary(cl8_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl8_blacks)}
  
  
  # Cluster 11
  {cl11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
    segments( 9.1, (summary(cl11_blacks)$coefmat.full[1,1] - summary(cl11_blacks)$coefmat.full[1,3]),  
              x1=9.1, y1=(summary(cl11_blacks)$coefmat.full[1,1] + summary(cl11_blacks)$coefmat.full[1,3]), col="black" )
    points(9.1, summary(cl11_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl11_blacks)}
  
  
  # Cluster 12
  {cl12_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
    segments( 10.1, (summary(cl12_blacks)$coefmat.full[1,1] - summary(cl12_blacks)$coefmat.full[1,3]),  
              x1=10.1, y1=(summary(cl12_blacks)$coefmat.full[1,1] + summary(cl12_blacks)$coefmat.full[1,3]), col="black" )
    points(10.1, summary(cl12_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl12_blacks)}
  
  
  # Habitat heterogeneity    
  segments(11.1, summary(gls_avg_blacks)$coefmat.full[11,1] - summary(gls_avg_blacks)$coefmat.full[11,3],
           x1=11.1, y1=summary(gls_avg_blacks)$coefmat.full[11,1] + summary(gls_avg_blacks)$coefmat.full[11,3], col="black") 
  points(11.1, summary(gls_avg_blacks)$coefmat.full[11,1], pch=17)
}

## Scaled ####
bg.col <- rgb(221, 231, 238, alpha = 255, maxColorValue = 255)

par(mar=c(8,4,0.5,0.5))
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response",
     xlim=c(1, 11), ylim=c(-3, 2), xaxt="n", bg=bg.col)
axis(1, at=c(1:11), labels=FALSE)
text(x=c(1:11), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
     labels=c("(1) Coastal", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
              "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
              "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
              "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
              "(12) Freshwater", "Habitat \nheterogeneity"),
     srt=55, adj=1, xpd=TRUE)
text(x=11, y=1.75, labels=c("(b)"))

## All ####
{# Cluster 1
  segments( 1, summary(gls_avg_all.std)$coefmat.full[1,1] - summary(gls_avg_all.std)$coefmat.full[1,3],  
            x1=1, y1=summary(gls_avg_all.std)$coefmat.full[1,1] + summary(gls_avg_all.std)$coefmat.full[1,3], col="blue")
  points(1, (summary(gls_avg_all.std)$coefmat.full[1,1]), pch=15, col="blue")
  
  # Cluster 2 
  {cl2_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
    segments( 2, (summary(cl2_all)$coefmat.full[1,1] - summary(cl2_all)$coefmat.full[1,3]),  
              x1=2, y1=(summary(cl2_all)$coefmat.full[1,1] + summary(cl2_all)$coefmat.full[1,3]), col="blue")
    points(2, summary(cl2_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl2_all)}
  
  # Cluster 3
  {cl3_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
    segments( 3, (summary(cl3_all)$coefmat.full[1,1] - summary(cl3_all)$coefmat.full[1,3]),  
              x1=3, y1=(summary(cl3_all)$coefmat.full[1,1] + summary(cl3_all)$coefmat.full[1,3]), col="blue")
    points(3, summary(cl3_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl3_all)}
  
  
  # Cluster 4
  {cl4_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
    segments( 4, (summary(cl4_all)$coefmat.full[1,1] - summary(cl4_all)$coefmat.full[1,3]),  
              x1=4, y1=(summary(cl4_all)$coefmat.full[1,1] + summary(cl4_all)$coefmat.full[1,3]), col="blue")
    points(4, summary(cl4_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl4_all)}
  
  
  # Cluster 5
  {cl5_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
    segments( 5, (summary(cl5_all)$coefmat.full[1,1] - summary(cl5_all)$coefmat.full[1,3]),  
              x1=5, y1=(summary(cl5_all)$coefmat.full[1,1] + summary(cl5_all)$coefmat.full[1,3]), col="blue")
    points(5, summary(cl5_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl5_all)}
  
  
  # Cluster 6
  {cl6_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
    segments( 6, (summary(cl6_all)$coefmat.full[1,1] - summary(cl6_all)$coefmat.full[1,3]),  
              x1=6, y1=(summary(cl6_all)$coefmat.full[1,1] + summary(cl6_all)$coefmat.full[1,3]), col="blue")
    points(6, summary(cl6_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl6_all)}
  
  
  # Cluster 7
  {cl7_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
    segments( 7, (summary(cl7_all)$coefmat.full[1,1] - summary(cl7_all)$coefmat.full[1,3]),  
              x1=7, y1=(summary(cl7_all)$coefmat.full[1,1] + summary(cl7_all)$coefmat.full[1,3]), col="blue")
    points(7, summary(cl7_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl7_all)}
  
  
  # Cluster 8
  {cl8_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
    segments( 8, (summary(cl8_all)$coefmat.full[1,1] - summary(cl8_all)$coefmat.full[1,3]),  
              x1=8, y1=(summary(cl8_all)$coefmat.full[1,1] + summary(cl8_all)$coefmat.full[1,3]), col="blue")
    points(8, summary(cl8_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl8_all)}
  
  
  # Cluster 11
  {cl11_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
    segments(9, (summary(cl11_all)$coefmat.full[1,1] - summary(cl11_all)$coefmat.full[1,3]),  
              x1=9, y1=(summary(cl11_all)$coefmat.full[1,1] + summary(cl11_all)$coefmat.full[1,3]), col="blue")
    points(9, summary(cl11_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl11_all)}
  
  
  # Cluster 12
  {cl12_all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
    segments( 10, (summary(cl12_all)$coefmat.full[1,1] - summary(cl12_all)$coefmat.full[1,3]),  
              x1=10, y1=(summary(cl12_all)$coefmat.full[1,1] + summary(cl12_all)$coefmat.full[1,3]), col="blue")
    points(10, summary(cl12_all)$coefmat.full[1,1], pch=15, col="blue")
    rm(cl12_all)}
  
  
  # Habitat heterogeneity    
  segments(11, summary(gls_avg_all.std)$coefmat.full[2,1] - summary(gls_avg_all.std)$coefmat.full[2,3],
           x1=11, y1=summary(gls_avg_all.std)$coefmat.full[2,1] + summary(gls_avg_all.std)$coefmat.full[2,3], col="blue") 
  points(11, summary(gls_avg_all.std)$coefmat.full[2,1], pch=15, col="blue")}

### Threatened ####
{# Cluster 1
  segments( 0.9, coef(summary(gls_clust_reds.std))[1,1] - coef(summary(gls_clust_reds.std))[1,2],  
            x1=0.9, y1=coef(summary(gls_clust_reds.std))[1,1] + coef(summary(gls_clust_reds.std))[1,2], col="red")
  points(0.9, (coef(summary(gls_clust_reds.std))[1,1]), pch=16, col="red")
  # Cluster 2
  segments( 1.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=1.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(1.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  # Cluster 3
  segments( 2.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=2.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(2.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 4
  segments( 3.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=3.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(3.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 5
  segments( 4.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=4.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(4.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 6
  segments( 5.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=5.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(5.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 7
  segments( 6.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=6.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(6.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 8
  segments( 7.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=7.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(7.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 11
  segments( 8.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=8.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(8.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Cluster 12
  segments( 9.9, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                    coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                     correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
            x1=9.9, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                          coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                           correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(9.9, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
  
  # Habitat diversity
  segments(10.9, coef(summary(gls_clust_reds.std))[11,1] - coef(summary(gls_clust_reds.std))[11,2],  
           x1=10.9, y1=coef(summary(gls_clust_reds.std))[11,1] + coef(summary(gls_clust_reds.std))[11,2], col="red")
  points(10.9, (coef(summary(gls_clust_reds.std))[11,1]), pch=16, col="red")}

## Alien ####
{# Cluster 1
  segments( 1.1, summary(gls_avg_blacks.std)$coefmat.full[1,1] - summary(gls_avg_blacks.std)$coefmat.full[1,3],  
            x1=1.1, y1=summary(gls_avg_blacks.std)$coefmat.full[1,1] + summary(gls_avg_blacks.std)$coefmat.full[1,3], col="black")
  points(1.1, (summary(gls_avg_blacks.std)$coefmat.full[1,1]), pch=17, col="black")
  
  # Cluster 2 
  {cl2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
    segments( 2.1, (summary(cl2_blacks)$coefmat.full[1,1] - summary(cl2_blacks)$coefmat.full[1,3]),  
              x1=2.1, y1=(summary(cl2_blacks)$coefmat.full[1,1] + summary(cl2_blacks)$coefmat.full[1,3]), col="black")
    points(2.1, summary(cl2_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl2_blacks)}
  
  # Cluster 3
  {cl3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
    segments( 3.1, (summary(cl3_blacks)$coefmat.full[1,1] - summary(cl3_blacks)$coefmat.full[1,3]),  
              x1=3.1, y1=(summary(cl3_blacks)$coefmat.full[1,1] + summary(cl3_blacks)$coefmat.full[1,3]), col="black")
    points(3.1, summary(cl3_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl3_blacks)}
  
  
  # Cluster 4
  {cl4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
    segments( 4.1, (summary(cl4_blacks)$coefmat.full[1,1] - summary(cl4_blacks)$coefmat.full[1,3]),  
              x1=4.1, y1=(summary(cl4_blacks)$coefmat.full[1,1] + summary(cl4_blacks)$coefmat.full[1,3]), col="black")
    points(4.1, summary(cl4_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl4_blacks)}
  
  
  # Cluster 5
  {cl5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
    segments( 5.1, (summary(cl5_blacks)$coefmat.full[1,1] - summary(cl5_blacks)$coefmat.full[1,3]),  
              x1=5.1, y1=(summary(cl5_blacks)$coefmat.full[1,1] + summary(cl5_blacks)$coefmat.full[1,3]), col="black")
    points(5.1, summary(cl5_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl5_blacks)}
  
  
  # Cluster 6
  {cl6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
    segments( 6.1, (summary(cl6_blacks)$coefmat.full[1,1] - summary(cl6_blacks)$coefmat.full[1,3]),  
              x1=6.1, y1=(summary(cl6_blacks)$coefmat.full[1,1] + summary(cl6_blacks)$coefmat.full[1,3]), col="black")
    points(6.1, summary(cl6_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl6_blacks)}
  
  
  # Cluster 7
  {cl7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
    segments( 7.1, (summary(cl7_blacks)$coefmat.full[1,1] - summary(cl7_blacks)$coefmat.full[1,3]),  
              x1=7.1, y1=(summary(cl7_blacks)$coefmat.full[1,1] + summary(cl7_blacks)$coefmat.full[1,3]), col="black")
    points(7.1, summary(cl7_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl7_blacks)}
  
  
  # Cluster 8
  {cl8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
    segments( 8.1, (summary(cl8_blacks)$coefmat.full[1,1] - summary(cl8_blacks)$coefmat.full[1,3]),  
              x1=8.1, y1=(summary(cl8_blacks)$coefmat.full[1,1] + summary(cl8_blacks)$coefmat.full[1,3]), col="black")
    points(8.1, summary(cl8_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl8_blacks)}
  
  
  # Cluster 11
  {cl11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
    segments( 9.1, (summary(cl11_blacks)$coefmat.full[1,1] - summary(cl11_blacks)$coefmat.full[1,3]),  
              x1=9.1, y1=(summary(cl11_blacks)$coefmat.full[1,1] + summary(cl11_blacks)$coefmat.full[1,3]), col="black")
    points(9.1, summary(cl11_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl11_blacks)}
  
  
  # Cluster 12
  {cl12_blacks <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
    segments( 10.1, (summary(cl12_blacks)$coefmat.full[1,1] - summary(cl12_blacks)$coefmat.full[1,3]),  
              x1=10.1, y1=(summary(cl12_blacks)$coefmat.full[1,1] + summary(cl12_blacks)$coefmat.full[1,3]), col="black")
    points(10.1, summary(cl12_blacks)$coefmat.full[1,1], pch=17, col="black")
    rm(cl12_blacks)}
  
  
  # Habitat heterogeneity    
  segments(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1] - summary(gls_avg_blacks.std)$coefmat.full[11,3],
           x1=11.1, y1=summary(gls_avg_blacks.std)$coefmat.full[11,1] + summary(gls_avg_blacks.std)$coefmat.full[11,3]) 
  points(11.1, summary(gls_avg_blacks.std)$coefmat.full[11,1], pch=17)
}

## Legend ####
legend("top", legend=c("Threatened species", "Alien species", "All species"),
       pch=c(16, 17, 15),
       col=c("red", "black", "blue"), cex=1)





dev.off()

##--- 7.4 Legend for cluster map ---####
##----------------------------------####

png("cluster_legend.png", height = 7, width = 25.75, units = "cm",
    pointsize = 18, res=500)  # Create the .png,

par(mar=c(0.01,0.5,0.01,0.5))
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("top", 
       c("Not grouped (0)", "Coastal (1)", "Urban/\ndeveloped (2)", "Urban/vegetated/\nriparian (3)",
         "Cultivated (4)", "Coniferous forest, \nlow production (5)",
         "Coniferous forest, \nmedium production (6)", "Open marsh and \nconif. forest (7)",
         "Coniferous forest, \nhigh production (8)", "Open firm ground \nand forest (10)",
         "Open firm ground \nand cultivated land (11)", "Freshwater (12)"),
       fill=c("white", map.col[1], map.col[2], map.col[3], map.col[4], map.col[5], map.col[6],
              map.col[7], map.col[8], map.col[9], map.col[10], map.col[11]), 
       bty="n", x.intersp = 1, y.intersp = 1.3, ncol = 3)
dev.off()



##--- 7.5 Model coefficient plot made with ggplot ---####
##--- (stick to the original plot for now)        ---####
##---------------------------------------------------####
#packages - not sure these are all necessary
libs<-c("lattice","Matrix","devtools","tidyr","tidyr","dplyr","ggplot2","grid","gridExtra","ggpubr","ggExtra","egg",
        "sp","rgdal","raster","rasterVis","maptools", "rgeos","lubridate","vegan","reshape","reshape2","cluster", "lme4","nlme") 
lapply(libs, require, character.only = TRUE)

# Make a dataframe with the model coefficients and their standard errors:
coef.models <- data.frame("coeff"={c(summary(gls_avg_all.std)$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(gls_avg_all.std)$coefmat.full[2,1],
                            # Threatened:
                            coef(summary(gls_avg_reds.std))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                            coef(summary(gls_avg_reds.std))[11,1],
                            # Alien:
                            summary(gls_avg_blacks.std)$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                            summary(gls_avg_blacks.std)$coefmat.full[2,1])},
                   "SE"={c(summary(gls_avg_all.std)$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                              summary(gls_avg_all.std)$coefmat.full[2,3],
                           # Threatened:
                           coef(summary(gls_avg_reds.std))[1,2],
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                   
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],             
                           coef(summary(gls_avg_reds.std))[11,2],
                           # ALien:
                           summary(gls_avg_blacks.std)$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                           summary(gls_avg_blacks.std)$coefmat.full[2,3])})

# Add the variables as factors  (the last 'levels' is necessary to make R recognize that I want that specific order)
coef.models$Variable <- factor(rep(c("Coastal", "Urban/developed", "Urban/vegetated/\nriparian", "Cultivated",
                             "Conif. forest,\nlow productivity", "Conif. forest,\nmedium productivity",
                             "Open marsh and\nconiferous forest", "Conif. forest,\nhigh productivity",
                             "Open firm ground\nand forest", "Freshwater", "Habitat heterogeneity"),3),
                             levels = c("Coastal", "Urban/developed", "Urban/vegetated/\nriparian", "Cultivated",
                                        "Conif. forest,\nlow productivity", "Conif. forest,\nmedium productivity",
                                        "Open marsh and\nconiferous forest", "Conif. forest,\nhigh productivity",
                                        "Open firm ground\nand forest", "Freshwater", "Habitat heterogeneity"))

# Add the group factor
coef.models$Species <- factor(c(rep("All",11), rep("Threatened", 11), rep("Alien", 11)), levels=c("All", "Threatened", "Alien"))

# Simple ggplot (for my talk in Tromsø)
# Basic geom_point + error bar - this works sequentially - do one model at a time
Coef.scaled<-ggplot(coef.models, aes(y=coeff,x=Variable, colour=Species, shape=Species))
Coef.scaled<-Coef.scaled+geom_errorbar(aes(ymin=coeff-SE, ymax=coeff+SE),width=.2,lwd=1.1,position=position_dodge(width=.65), alpha=.6,show.legend=F) # if legend = T - line in the legend
Coef.scaled<-Coef.scaled+scale_colour_manual(values=c("blue","red","black"))
Coef.scaled<-Coef.scaled+geom_point(position=position_dodge(width=.65),size=3)
Coef.scaled<-Coef.scaled+ylab("Model coefficients (scaled response)")+xlab(element_blank())
Coef.scaled<-Coef.scaled+theme_minimal() +
  theme(axis.text.x=element_text(angle=55, hjust=1))
Coef.scaled


# ggplot with multiple facets (to make the bars more separated)
CS2<-ggplot(coef.models,aes(y=coeff,x="", colour=Species,fill=Species, shape=Species))
#CS2<-CS2+geom_point(data=coef.models, position=position_dodge(width=.65),colour="grey50",alpha=.6,size=1)
CS2<-CS2+geom_errorbar(aes(ymin=coeff-SE, ymax=coeff+SE),width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
CS2<-CS2+scale_colour_manual(values=c("blue","red","black"))
CS2<-CS2+geom_point(position=position_dodge(width=.65),size=4)
CS2<-CS2+facet_wrap(~Variable, ncol=11)                        # This is making two panels based on a factor 
CS2<-CS2+scale_y_continuous(limits=c(-3,2), expand=c(0,0))     # This is controlling the scales/axes - here: the y-axis. 'labels' lets you adjust the margins, if the original numbers are not corrects. The 'expand' command lets you adjust the spacing between the first break (her: 0) and the axes (normally, R likes to give you a little space between 0 and where the graph axes starts - you might not want that)
CS2<-CS2+ylab("Model coefficients (scaled response)")+xlab(element_blank())
CS2<-CS2+ theme_bw() +                      # Theme controls the asethetics of the graph - all components can be manipulated...
  theme(plot.background = element_blank(),    # This means that the plot.background does not exist ('blank')
        #plot.background = element_rect(colour = "gray60"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill="gray99", colour="gray90"),
        axis.title=element_text(size=24,color="black"),
        strip.text.x = element_text(size=24, angle=90, hjust=0.1,colour = "black"),
        strip.background = element_rect(colour = "gray"),
        legend.title = element_text(size=24),
        legend.text=element_text(size=22))   
CS2

#ggsave("coef_scaled.png", plot=CS2, width=14, height=10, units = "cm", dpi=500)
#dev.off()

# Lets try and add the unscaled data as well:
coef.uns <- data.frame("coeff"={c(summary(gls_avg_all)$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(gls_avg_all)$coefmat.full[2,1],
                                     # Threatened:
                                     coef(summary(gls_avg_reds))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                                     coef(summary(gls_avg_reds))[11,1],
                                     # Alien:
                                     summary(gls_avg_blacks)$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,1],
                                     summary(gls_avg_blacks)$coefmat.full[2,1])},
                          "SE"={c(summary(gls_avg_all)$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.a_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(gls_avg_all)$coefmat.full[2,3],
                                  # Threatened:
                                  coef(summary(gls_avg_reds))[1,2],
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                   
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                      
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],                                     
                                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2],             
                                  coef(summary(gls_avg_reds))[11,2],
                                  # ALien:
                                  summary(gls_avg_blacks)$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(model.avg(model.sel(dredge(update(gls.b_ML_clust, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE))$coefmat.full[1,3],
                                  summary(gls_avg_blacks)$coefmat.full[2,3])})

coef.sc.usc <- rbind(coef.models[,c(1,2)], coef.uns[,c(1,2)])

# Add the variables as factors  (the last 'levels' is necessary to make R recognize that I want that specific order)
coef.sc.usc$Variable <- factor(rep(c("Coastal", "Urban_dev", "Urban_veg_rip", "Cultivated",
                                     "Conif.low", "Conif.med", "Marsh", "Conif.high",
                                     "Open_ground", "Freshwater", "Hab.het"),6), levels = c("Coastal", "Urban_dev", "Urban_veg_rip", "Cultivated",
                                                                                            "Conif.low", "Conif.med", "Marsh", "Conif.high",
                                                                                            "Open_ground", "Freshwater", "Hab.het"))
# Add the group factor
coef.sc.usc$group <- factor(c(rep("All",11), rep("Threatened", 11), rep("Alien", 11),rep("All",11), rep("Threatened", 11), rep("Alien", 11)),
                            levels=c("All", "Threatened", "Alien"))


# Add scaling/centering as a factor
coef.sc.usc$Scaling <- factor(c(rep("Scaled",33), rep("Unscaled", 33)), levels=c("Scaled", "Unscaled"))

# Make the ggplot
CS3<-ggplot(coef.sc.usc,aes(y=coeff,x="", colour=group, fill=group, shape=group))
CS3<-CS3+geom_errorbar(aes(ymin=coeff-SE, ymax=coeff+SE),width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
CS3<-CS3+scale_colour_manual(values=c("blue","red","black"))
CS3<-CS3+geom_point(position=position_dodge(width=.65),size=4)
CS3<-CS3+facet_wrap(~Scaling+Variable, ncol=11, scales="free")                      # This is making two panels based on a factor 
#CS3<-CS3+scale_y_continuous(limits=c(-3,4.5), expand=c(0,0))     # This is controlling the scales/axes - here: the y-axis. 'labels' lets you adjust the margins, if the original numbers are not corrects. The 'expand' command lets you adjust the spacing between the first break (her: 0) and the axes (normally, R likes to give you a little space between 0 and where the graph axes starts - you might not want that)
CS3<-CS3+ylab("Model coefficients")+xlab(element_blank())
CS3<-CS3+ theme_gray() +                      
  theme(plot.background = element_blank(),    
        panel.grid.major.x = element_blank())   
CS3

# TRY AGAIN!
CS4<-ggplot(coef.sc.usc,aes(y=coeff,x=Variable, colour=group, fill=group, shape=group))
CS4<-CS4+geom_errorbar(aes(ymin=coeff-SE, ymax=coeff+SE),width=.2,lwd=1.1,position=position_dodge(width=.65),show.legend=F)
CS4<-CS4+scale_colour_manual(values=c("blue","red","black"))
CS4<-CS4+geom_point(position=position_dodge(width=.65),size=4)
CS4<-CS4+facet_wrap(~Scaling, ncol=1, scales="free")         # This is making two panels based on a factor 
#CS3<-CS3+scale_y_continuous(limits=c(-3,4.5), expand=c(0,0))     # This is controlling the scales/axes - here: the y-axis. 'labels' lets you adjust the margins, if the original numbers are not corrects. The 'expand' command lets you adjust the spacing between the first break (her: 0) and the axes (normally, R likes to give you a little space between 0 and where the graph axes starts - you might not want that)
CS4<-CS4+ylab("Model coefficients")+xlab("Habitat")
CS4<-CS4+ theme_gray() +                      
  theme(plot.background = element_blank(),    
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(angle=35, hjust=1))   
CS4

# Try to fix the x-axis to only be at the bottom
library(gtable)
plot_tab <- ggplotGrob(CS4)
plot_tab$layout$name
#print(plot_tab)
#plot_filtered <- gtable_filter(plot_tab, 
#                               "(background|panel|strip_t|axis_l|xlab|ylab|guide-box|title|axis_t-[1234567891011])",
#                               trim=FALSE)
#grid.newpage()
#grid.draw(plot_filtered)

gtable_filter_remove <- function (x, name, trim = TRUE){
  matches <- !(x$layout$name %in% name)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  if (trim) 
    x <- gtable_trim(x)
  x
}

p_filtered <- gtable_filter_remove(plot_tab, name = paste0("axis-b-", c(1), c("-1")),
                                   trim = FALSE)
p_filtered <- gtable_filter_remove(p_filtered, name = paste0("xlab-t"),
                                   trim = FALSE)
library(grid)
grid.newpage()
grid.draw(p_filtered)



##--- 7.6 Prediction plot with ggplot ---####
##---------------------------------------####
# Make the needed predictions (in the right way)
#A:Specify covariate values for predictions
{MyData <- TrdRast_clust[, c(1, 83, 86)]
      # Remove the grid cells with categories which cannot be used i the model (0 and 10)
      MyData <- MyData[!MyData@data$clusterCut==0,]
      MyData <- MyData[!MyData@data$clusterCut==10,]
      MyData@data$clusterCut <- as.factor(MyData@data$clusterCut)}

#B. Create X matrix with expand.grid
X <- model.matrix(~ clusterCut + Divers, data = MyData)
head(X)

#C. Calculate predicted values
#NewData$Pred <- predict(M4, NewData, level = 0)
#The level = 0 ensure that we fit the fixed effects
#Or:
MyData$log_chao.all <- X %*% (gls_avg_all$coefficients[1,])  # = X * beta   # We have to make sure that it has the same names as the other dataset - otherwise er cannot plot them together

#D. Calculate standard errors (SE) for predicted values
#   SE of fitted values are given by the square root of
#   the diagonal elements of: X * cov(betas) * t(X)  
#   Take this for granted!

MyData$SE <- sqrt(  diag(X %*% vcov(gls_avg_all) %*% t(X))  )

#And using the Pred and SE values, we can calculate
#a 95% confidence interval
MyData$SeUp <- MyData$log_chao.all + 1.96 * MyData$SE
MyData$SeLo <- MyData$log_chao.all - 1.96 * MyData$SE

#E. Plot predicted values
names(MyData)

# I am not sure, if this is doing exactly what I want, so instead I use the predict-function (the same as was used for)
# making the mapped predictions). But now I have to save them separately, and add the standard errors:
#C. Calculate predicted values and #D. Calculate standard error
MyData2 <- MyData[,1:3]
    MyData2$log_chao.all <- predict(gls_avg_all, newdata=MyData2, se.fit=TRUE)$fit
        MyData2$se.log_chao.all <- predict(gls_avg_all, newdata=MyData2, se.fit=TRUE)$se.fit
    MyData2$log_chao.reds <- predict(gls_avg_reds, newdata=MyData2, se.fit=TRUE)$fit
        MyData2$se.log_chao.reds <- predict(gls_avg_reds, newdata=MyData2, se.fit=TRUE)$se.fit
    MyData2$log_chao.blacks <- predict(gls_avg_blacks, newdata=MyData2, se.fit=TRUE)$fit
        MyData2$se.log_chao.blacks <- predict(gls_avg_blacks, newdata=MyData2, se.fit=TRUE)$se.fit

# Calculate 0.95 confidence interval
{MyData2$SeUp_all <- MyData2$log_chao.all + 1.96 * MyData2$se.log_chao.all
MyData2$SeLo_all <- MyData2$log_chao.all - 1.96 * MyData2$se.log_chao.all
MyData2$SeUp_reds <- MyData2$log_chao.reds + 1.96 * MyData2$se.log_chao.reds
MyData2$SeLo_reds <- MyData2$log_chao.reds - 1.96 * MyData2$se.log_chao.reds
MyData2$SeUp_blacks <- MyData2$log_chao.blacks + 1.96 * MyData2$se.log_chao.blacks
MyData2$SeLo_blacks <- MyData2$log_chao.blacks - 1.96 * MyData2$se.log_chao.blacks}

## Plot observed data versus prediction (MyData) #####
P.all <- ggplot(TrdRast_clust@data,aes(x=Divers, y=log_chao.all))
P.all <- P.all + geom_ribbon(data=MyData@data,aes(ymin=SeUp, ymax=SeLo),fill="blue",colour="blue",alpha=.65,lwd=NA,show.legend=F)  # This is the same as an errorbar, just along the entire line rather than a single point
P.all <- P.all +geom_line(data=MyData@data,#aes(ymin=SeUp, ymax=SeLo),
                          colour="blue",alpha=.9,lwd=2,show.legend=F)
P.all <- P.all +geom_point(data=TrdRast_clust_model@data, stat="identity",colour="grey50",fill="grey50",size=2.5)   # 'identity' takes you back to the original dataset
P.all <- P.all +facet_wrap(~clusterCut, scale="fixed")
P.all <- P.all+scale_x_continuous(limits=c(0,1), breaks = c(0,0.25,0.5,0.75), labels = c(0,0.25,0.5,0.75), expand=c(0,0))
P.all <-P.all + scale_y_continuous(expand=c(0,0))
P.all <-P.all +ylab("log(ESR all species+1) (Chao's)")+xlab("Habitat heterogeneity") # Adding x and ylabs to plot
P.all <-P.all +theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
P.all <- P.all +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
P.all <- P.all +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
P.all




## Plot observed data versus prediction (MyData2) #####
# Take out the rows with unused clusters:
Trd_data_ggplot <- subset(TrdRast_clust@data, !(clusterCut==0 | clusterCut==9 | clusterCut==10))
Trd_data_ggplot$clusterCut <- as.factor(Trd_data_ggplot$clusterCut)

# All
{P.all <- ggplot(Trd_data_ggplot,aes(x=Divers, y=log_chao.all))
P.all <- P.all + geom_ribbon(data=MyData2@data,aes(ymin=SeUp_all, ymax=SeLo_all),fill="blue",colour="blue",alpha=.65,lwd=NA,show.legend=F)  # This is the same as an errorbar, just along the entire line rather than a single point
P.all <- P.all +geom_line(data=MyData2@data,#aes(ymin=SeUp, ymax=SeLo),
                          colour="blue",alpha=.9,lwd=2,show.legend=F)
P.all <- P.all +geom_point(data=Trd_data_ggplot, stat="identity",colour="grey50",fill="grey50",size=2.5)   # 'identity' takes you back to the original dataset
P.all <- P.all +facet_wrap(~clusterCut, scale="fixed")
P.all <- P.all+scale_x_continuous(limits=c(0,1), breaks = c(0,0.25,0.5,0.75), labels = c(0,0.25,0.5,0.75), expand=c(0,0))
P.all <-P.all + scale_y_continuous(expand=c(0,0))
P.all <-P.all +ylab("log(ESR all species+1) (Chao's)")+xlab("Habitat heterogeneity") # Adding x and ylabs to plot
P.all <-P.all +theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
P.all <- P.all +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
P.all <- P.all +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
P.all}

# Threatened
{P.reds <- ggplot(Trd_data_ggplot,aes(x=Divers, y=log_chao.reds))
  P.reds <- P.reds + geom_ribbon(data=MyData2@data,aes(ymin=SeUp_reds, ymax=SeLo_reds),fill="red",colour="red",alpha=.65,lwd=NA,show.legend=F)  # This is the same as an errorbar, just along the entire line rather than a single point
  P.reds <- P.reds +geom_line(data=MyData2@data,#aes(ymin=SeUp, ymax=SeLo),
                            colour="red",alpha=.9,lwd=2,show.legend=F)
  P.reds <- P.reds +geom_point(data=Trd_data_ggplot, stat="identity",colour="grey50",fill="grey50",size=2.5)   # 'identity' takes you back to the original dataset
  P.reds <- P.reds +facet_wrap(~clusterCut, scale="fixed")
  P.reds <- P.reds+scale_x_continuous(limits=c(0,1), breaks = c(0,0.25,0.5,0.75), labels = c(0,0.25,0.5,0.75), expand=c(0,0))
  P.reds <-P.reds + scale_y_continuous(expand=c(0,0))
  P.reds <-P.reds +ylab("log(ESR threatened species+1) (Chao's)")+xlab("Habitat heterogeneity") # Adding x and ylabs to plot
  P.reds <-P.reds +theme_bw()+
    theme(
      rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
      ,panel.background=element_rect(fill="transparent")
      ,plot.background=element_rect(fill="transparent",colour=NA)
      ,panel.grid.minor = element_blank() # Removing reds grids and borders
      ,panel.border = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.major.y = element_blank()
      ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
      ,axis.line.x = element_line(color="black", size = .5)
      ,axis.title=element_text(size=12,color="black")
      ,axis.text.x=element_text(size=11,color="black",
                                margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,axis.ticks.length=unit(-1.5, "mm")
      ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,plot.margin = unit(c(5,5,5,5), "mm")
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))
  P.reds <- P.reds +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
  P.reds <- P.reds +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
  P.reds}

# Alien
{P.blacks <- ggplot(Trd_data_ggplot,aes(x=Divers, y=log_chao.blacks))
  P.blacks <- P.blacks + geom_ribbon(data=MyData2@data,aes(ymin=SeUp_blacks, ymax=SeLo_blacks),fill="gray50",colour="gray50",alpha=.65,lwd=NA,show.legend=F)  # This is the same as an errorbar, just along the entire line rather than a single point
  P.blacks <- P.blacks +geom_line(data=MyData2@data,#aes(ymin=SeUp, ymax=SeLo),
                            colour="black",alpha=.9,lwd=2,show.legend=F)
  P.blacks <- P.blacks +geom_point(data=Trd_data_ggplot, stat="identity",colour="grey20",fill="grey20",size=2.5)   # 'identity' takes you back to the original dataset
  P.blacks <- P.blacks +facet_wrap(~clusterCut, scale="fixed")
  P.blacks <- P.blacks+scale_x_continuous(limits=c(0,1), breaks = c(0,0.25,0.5,0.75), labels = c(0,0.25,0.5,0.75), expand=c(0,0))
  P.blacks <-P.blacks + scale_y_continuous(expand=c(0,0))
  P.blacks <-P.blacks +ylab("log(ESR alien species+1) (Chao's)")+xlab("Habitat heterogeneity") # Adding x and ylabs to plot
  P.blacks <-P.blacks +theme_bw()+
    theme(
      rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
      ,panel.background=element_rect(fill="transparent")
      ,plot.background=element_rect(fill="transparent",colour=NA)
      ,panel.grid.minor = element_blank() # Removing blacks grids and borders
      ,panel.border = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.major.y = element_blank()
      ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
      ,axis.line.x = element_line(color="black", size = .5)
      ,axis.title=element_text(size=12,color="black")
      ,axis.text.x=element_text(size=11,color="black",
                                margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,axis.ticks.length=unit(-1.5, "mm")
      ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
      ,plot.margin = unit(c(5,5,5,5), "mm")
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))
  P.blacks <- P.blacks +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 1) 
  P.blacks <- P.blacks +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 1) 
  P.blacks}

## Put it all in the same graph ####
# Prepare the original data
Trd_data_ggplot2 <- data.frame(Pixelnr=c(rep(Trd_data_ggplot$Pixelnr, 3)),
                               clusterCut=c(rep(as.character(Trd_data_ggplot$clusterCut), 3)),
                               Divers=c(rep(Trd_data_ggplot$Divers)),
                               log.chao=c(Trd_data_ggplot$log_chao.all, Trd_data_ggplot$log_chao.reds, Trd_data_ggplot$log_chao.blacks),
                               Group=factor(c(rep("All", 1490), rep("Threatened", 1490), rep("Alien", 1490)), levels=c("All", "Threatened", "Alien")))
Trd_data_ggplot2$clusterCut <- as.character(Trd_data_ggplot2$clusterCut)
for(i in 1:NROW(Trd_data_ggplot2)){
  Trd_data_ggplot2[i,"clusterCut"] <- ifelse(Trd_data_ggplot2[i,"clusterCut"]==1, paste("Coastal"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==2, paste("Urban_dev"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==3, paste("Urban_veg_rip"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==4, paste("Cultivated"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==5, paste("Conif.low"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==6, paste("Conif.med"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==7, paste("Marsh"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==8, paste("Conif.high"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==11, paste("Open_ground"),
                                             ifelse(Trd_data_ggplot2[i,"clusterCut"]==12, paste("Freshwater"), "NA"))))))))))
}
Trd_data_ggplot2$clusterCut <- factor(Trd_data_ggplot2$clusterCut, levels = c("Coastal", "Urban_dev", "Urban_veg_rip", "Cultivated",
                                                                              "Conif.low", "Conif.med", "Marsh", "Conif.high",
                                                                              "Open_ground", "Freshwater"))

# Prepare the predicted values
MyData3 <- data.frame(Pixelnr=c(rep(MyData2@data$Pixelnr, 3)),
                      clusterCut=factor(c(rep(as.character(Trd_data_ggplot$clusterCut), 3)),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", "11", "12")),
                      Divers=c(rep(Trd_data_ggplot$Divers)),Divers=c(rep(MyData2@data$Divers)),
                      log.chao=c(MyData2@data$log_chao.all, MyData2@data$log_chao.reds, MyData2@data$log_chao.blacks),
                                 SE= c(MyData2@data$se.log_chao.all, MyData2@data$se.log_chao.reds, MyData2@data$se.log_chao.blacks),
                      Group=factor(c(rep("All", 1490), rep("Threatened", 1490), rep("Alien", 1490)), levels=c("All", "Threatened", "Alien")))

MyData3$clusterCut <- as.character(MyData3$clusterCut)
for(i in 1:NROW(MyData3)){
  MyData3[i,"clusterCut"] <- ifelse(MyData3[i,"clusterCut"]==1, paste("Coastal"),
                                             ifelse(MyData3[i,"clusterCut"]==2, paste("Urban_dev"),
                                             ifelse(MyData3[i,"clusterCut"]==3, paste("Urban_veg_rip"),
                                             ifelse(MyData3[i,"clusterCut"]==4, paste("Cultivated"),
                                             ifelse(MyData3[i,"clusterCut"]==5, paste("Conif.low"),
                                             ifelse(MyData3[i,"clusterCut"]==6, paste("Conif.med"),
                                             ifelse(MyData3[i,"clusterCut"]==7, paste("Marsh"),
                                             ifelse(MyData3[i,"clusterCut"]==8, paste("Conif.high"),
                                             ifelse(MyData3[i,"clusterCut"]==11, paste("Open_ground"),
                                             ifelse(MyData3[i,"clusterCut"]==12, paste("Freshwater"), "NA"))))))))))
}
MyData3$clusterCut <- factor(MyData3$clusterCut, levels = c("Coastal", "Urban_dev", "Urban_veg_rip", "Cultivated",
                                                                              "Conif.low", "Conif.med", "Marsh", "Conif.high",
                                                                              "Open_ground", "Freshwater"))


# Calculate confidence interval
MyData3$SeUp <- MyData3$log.chao + 1.96 * MyData3$SE
MyData3$SeLow <- MyData3$log.chao - 1.96 * MyData3$SE


# Make the plot
P <- ggplot(Trd_data_ggplot2,aes(x=Divers, y=log.chao, colour=Group))
P <- P + scale_colour_manual(values=c("blue","red","black"))
P <- P + scale_fill_manual(values=c("blue","red","black"))
P <- P + geom_ribbon(data=MyData3,aes(ymin=SeUp, ymax=SeLow, colour=Group , fill=Group),alpha=.4,lwd=NA,show.legend=F)
P <- P +geom_line(data=MyData3,#aes(ymin=SeUp, ymax=SeLo),
                          alpha=.75,lwd=1,show.legend=F)
P <- P +geom_point(data=Trd_data_ggplot2, stat="identity" ,size=1)   # 'identity' takes you back to the original dataset
P <- P +facet_wrap(~clusterCut, scale="fixed")
P <- P+scale_x_continuous(limits=c(0,1), breaks = c(0,0.25,0.5,0.75), labels = c(0,0.25,0.5,0.75), expand=c(0,0))
P <-P + scale_y_continuous(expand=c(0,0))
P <-P +ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity") # Adding x and ylabs to plot
P <-P +theme_bw()+
  theme(
    rect = element_rect(fill ="transparent") # This makes the background transparent rather than white
    ,panel.background=element_rect(fill="transparent")
    ,plot.background=element_rect(fill="transparent",colour=NA)
    ,panel.grid.minor = element_blank() # Removing all grids and borders
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_blank()
    ,axis.line.y = element_line(color="black", size = .5) # Adding back the axis lines
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.title=element_text(size=12,color="black")
    ,axis.text.x=element_text(size=11,color="black",
                              margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,axis.ticks.length=unit(-1.5, "mm")
    ,axis.text.y = element_text(margin=margin(2.5,2.5,2.5,2.5,"mm"))
    ,plot.margin = unit(c(5,5,5,5), "mm")
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the strip (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))
P <- P +annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = 0, xend = 0, size = 0.5) 
P <- P +annotate(geom = 'segment', y = 0, yend = 0, color = 'black', x = -Inf, xend = Inf, size = 0.5) 
P
