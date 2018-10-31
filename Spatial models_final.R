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
par(mar=c(10,1.1,1,2.1))
par(las=2)
par(cex.axis=0.75)
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
                        "(12) Freshwater"), yaxlabels = "")

par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("Communications/traffic", "Developed area",
                          "Forest, coniferous", "Forest, deciduous", "Forest, mix",
                          "Freshwater", "Fully cultivated land",
                          "Hfgl ('Innmarksbeite')", "Marsh,", "Ocean",
                          "Ofg ('Åpen fastmark')", "Superficially cultivated land", "NA"),
       fill=c("hotpink", "lightpink", "forestgreen", "darkolivegreen1",
              "darkolivegreen3", "dodgerblue", "darkorange", "khaki1",
              "cyan", "navy", "sandybrown",
              "gold", "gray"), cex=0.75)
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

# With a better legend:
par(mfrow=c(1,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(TrdRast_clust, main="",
     col=as.character(TrdRast_clust@data$col.clust))  
legend("top", 
       c("Not grouped (0)", "Coastal (1)", "Urban/developed (2)", "Urban/vegetated/riparian (3)",
                   "Cultivated (4)", "Coniferous forest, \nlow production (5)",
                   "Coniferous forest, \nmedium production (6)", "Open marsh and \nconif. forest (7)",
                   "Coniferous forest, \nhigh production (8)", "Open firm ground \nand forest (10)",
                   "Open firm ground \nand cultivated land (11)", "Freshwater (12)"),
       fill=c("gray", "blue", "hotpink", "red", "orange", "palegreen", "green",
              "lightcyan", "forestgreen", "sandybrown", "yellow", "cyan"), 
       bty="n", cex=0.8, x.intersp = 2, y.intersp = 2, ncol = 4)

plot(TrdRast_clust_model[TrdRast_clust_model$Pixelnr==1369,], border="red", add=TRUE)

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
     col=TrdRast_clust@data$Divers*10)    # Habitat diversity
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

# Extract the northness values by the grid-polygons  (obs: the polygons are CRS-transformed to matc the raster):
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


##--- 3.1 DATA EXPLORATION ---####
##----------------------------####
source("HighstatLibV10.R")

# An important point for why we're using the Chao-estimation rather than the raw species numbers, is to take sampling
# effort into account - hence, we have to make see if we have a correlation between number of observed species and the
# estimated numbers within the cells we're using for the analyses
MyVar_numbers <- c("S.obs_2013", "S.obs_reds_2013", "S.obs_blacks_2013",
                   "S.chao1_2013", "S.chao1_reds_2013", "S.chao1_blacks_2013")
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
# it is luckily only one grid cell (covered entirely by freshwater, north.mean = NaN). We have to omit that one,
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
stepAIC(gls_ML_clust)              # For unknown reasons, the standard 'step()' doesn't work - this one does
rm(gls_ML_clust)                   # To save some memory

# According to the SAC-function, the best model is: log_chao.reds ~ clusterCut + Divers
# This is similar to the optimal model for the non-spatial approach 

# Define the better model(s):
summary(gls_clust_reds <- gls(log_chao.reds ~  clusterCut + Divers,
                              data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                              method = "ML"))

AIC(M1, gls_clust_reds)              # The model is seemingly not better, according to AIC. This could potentially
                                     # be an artifact of the warning: "models are not all fitted to the same number
                                     # of observations" - this is probably as we introduce the neighbours, some grid
                                     # cells are "floating". The AIC might be worse, but we have shown that we have
                                     # SAC. We continue with the spatial models then

# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
            method = "ML"))


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

stepAIC(gls.b_ML_clust)        # log_chao.blacks ~ clusterCut + Divers
rm(gls.b_ML_clust)

# This is somewhat similar to the optimal model for the non-spatial approach, minus the north.mean

# Define the better model and compare AIC:
summary(gls_clust_blacks <- gls(log_chao.blacks ~  clusterCut + Divers,
                                data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                                method = "ML"))

AIC(M2, gls_clust_blacks)


# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))


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

stepAIC(gls.a_ML_clust)        # log_chao.all ~ clusterCut + Divers
rm(gls.a_ML_clust)

# Define the better model and compare AIC:
summary(gls_clust_all <- gls(log_chao.all ~  clusterCut + Divers,
                                data = TrdRast_clust_model@data, correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]),
                             method = "ML"))

AIC(M3, gls_clust_all)


# To check the coefficients of the same model, but with other factor levels as reference, use the following and change
# the factor level in "ref="
summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2])))




##--- 3.4.3 Model validation ---####
##------------------------------####
summary(gls_clust_reds)
summary(gls_clust_blacks)
summary(gls_clust_all)

# Pseudo-R^2 calculated as correlation between observed and predicted values - this is what was done in
# in the Ballesteros-Meija paper - I am uncertain whether is is desirable for me.
# However, it seems to be my best option for a Goodness of Fit-test - I can compare them to the R^2 of the
# non-spatial models (calculated as (1-(Residual deviance/Null deviance))).
# Get the needed data:
pseudo <- merge(TrdRast_clust_model@data[,c(1,22,23,24)], data_predict_clust@data[,c(1,7,8,9)], by="Pixelnr")

# Make the calculations (and potentially compare):
cor(pseudo$log_chao.reds, pseudo$predict_red)
1-(172.04/268.22)    # summary(M1)

cor(pseudo$log_chao.blacks, pseudo$predict_blacks)
1-(220.00/291.46)   # summary(M2)

cor(pseudo$log_chao.all, pseudo$predict_all)
1-(220.03/254.68)   # summary(M3)


# Plot residuals vs fitted values (gls.exp_clust_reds)
F1_threat <- fitted(gls_clust_reds)
E1_threat <- resid(gls_clust_reds, type = "pearson")      
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


# Plot residuals vs fitted values (gls.exp_clust_blacks)
F1_alien <- fitted(gls_clust_blacks)
E1_alien <- resid(gls_clust_blacks, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_alien, 
     y = E1_alien,
     xlab = "Fitted values - alien",
     ylab = "Pearson residuals - alien",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_alien <- E1_alien
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "Resid_alien")
TrdRast_clust_model@data$Resid_alien <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_alien)

# Plot residuals vs fitted values (gls.exp_clust_all)
F1_all <- fitted(gls_clust_all)
E1_all <- resid(gls_clust_all, type = "pearson")      # Remember, Pearson residuals are the same as standardized residuals -these are the best ones for detecting patterns (or lack of same) in the residuals
par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = F1_all, 
     y = E1_all,
     xlab = "Fitted values - all",
     ylab = "Pearson residuals - all",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# Plot the residuals vs each covariate     
TrdRast_clust_model@data$Resid_all <- E1_all
Myxyplot(TrdRast_clust_model@data, MyVar2[-4], "Resid_all")
TrdRast_clust_model@data$Resid_all <- NULL

# Histogram of the residuals to check is they are Gaussian:
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
hist(E1_all)

# QQ-plots
qqnorm(gls_clust_reds, abline=c(0,1))
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
axis(1, at=c(1:11), labels=c("(1) Coastal \n(Intercept)", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
                             "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
                             "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
                             "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
                             "(12) Freshwater", "Habitat \nheterogeneity"), las=2)
abline(h=0, lty=2, col="gray")

### Threatened ####
# Cluster 1
segments( 0.9, coef(summary(gls_clust_reds))[1,1] - coef(summary(gls_clust_reds))[1,2],  
          x1=0.9, y1=coef(summary(gls_clust_reds))[1,1] + coef(summary(gls_clust_reds))[1,2], col="red")
      points(0.9, (coef(summary(gls_clust_reds))[1,1]), pch=20, col="red")
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

  
legend("topright", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)

##-------####

##--- 3.4.3.2 Other versions of the coefficient plots ---####
##-------------------------------------------------------####
## All species along the x-axis, threatened and alien along the y axis ####

# Make an empty plot:
par(mar=c(5.1,4.1,0.5,2.1))
plot(1, type="n", xlab="Model coefficients, all species", ylab="Model coeffiecient, threatened and alien species",
     xlim=c(0, 5), ylim=c(-1.5, 2.5))
# Threatened vs. all
points(x=c(coef(summary(gls_clust_all))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls_clust_all))[11,1]),
       y=c(coef(summary(gls_clust_reds))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls_clust_reds))[11,1]), pch=19,
       col=c("blue", "hotpink", "red", "orange", "palegreen", "green", "lightcyan", "forestgreen", "yellow", "cyan", "gray"))

# Alien vs. all
points(x=c(coef(summary(gls_clust_all))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls_clust_all))[11,1]),
       y=c(coef(summary(gls_clust_blacks))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
           coef(summary(gls_clust_blacks))[11,1]), pch=17,
       col=c("blue", "hotpink", "red", "orange", "palegreen", "green", "lightcyan", "forestgreen", "yellow", "cyan", "gray"))

legend("topleft", legend = c("Threatened species", "Alien species", "Coastal (1)", "Urban/developed (2)",
                             "Urban/vegetated/riparian (3)", "Cultivated (4)", "Coniferous forest, low production (5)",
                             "Coniferous forest, medium production (6)", "Open marsh and conif. forest (7)",
                             "Coniferous forest, high production (8)", "Open firm ground and cultivated land (11)",
                             "Freshwater (12)", "Habitat heterogeneity"), cex = 0.6,
       pch=c(1,2, rep(15,11)), col=c("black", "black", "blue", "hotpink", "red", "orange", "palegreen", "green",
                                     "lightcyan", "forestgreen", "yellow", "cyan", "gray"))

# This doesn't immediately tell us much - if needed, the std.errors can be added (the code for that has not been added yet)



## Coefficients minus the mean coefficient value for each model ####

mean.coef.all <- mean(c(coef(summary(gls_clust_all))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls_clust_all))[11,1]))

mean.coef.reds <- mean(c(coef(summary(gls_clust_reds))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls_clust_reds))[11,1]))

mean.coef.blacks <- mean(c(coef(summary(gls_clust_blacks))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1],
                        coef(summary(gls_clust_blacks))[11,1]))

# Make the plot - for each point/segment, the mean coeffiecient is subtracted - thus, the plot is only useful for
# comparing the importance of each coefficient for the model relative to the other models:
par(mfrow=c(1,1))
par(mar=c(10,4.1,0.5,2.1))
plot(1, type="n", xlab="", ylab="Model coefficient - mean of all model coefficients", xlim=c(1, 11), ylim=c(-2.5, 1.5), xaxt="n")
axis(1, at=c(1:11), labels=c("(1) Coastal \n(Intercept)", "(2) Urban/\ndeveloped", "(3) Urb./veg./\nrip.",
                             "(4) Cultivated", "(5) Conif. forest, \nlow prod.",
                             "(6) Conif. forest, \nmedium prod.", "(7) Open marsh and \nconif. forest",
                             "(8) Conif. forest, \nhigh prod.", "(11) Open firm ground \nand forest",
                             "(12) Freshwater", "Habitat \nheterogeneity"), las=2)
abline(h=0, lty=2, col="gray")

### Threatened ####
# Cluster 1
segments( 0.9, (coef(summary(gls_clust_reds))[1,1] - mean.coef.reds) - coef(summary(gls_clust_reds))[1,2],  
          x1=0.9, y1=(coef(summary(gls_clust_reds))[1,1] - mean.coef.reds) + coef(summary(gls_clust_reds))[1,2], col="red")
points(0.9, (coef(summary(gls_clust_reds))[1,1])- mean.coef.reds, pch=20, col="red")
# Cluster 2
segments( 1.9, ((coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds) -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=1.9, y1=((coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds) +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(1.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")
# Cluster 3
segments( 2.9, ((coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds) -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.9, y1=((coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds) +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(2.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 4
segments( 3.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(3.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 5
segments( 4.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(4.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 6
segments( 5.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(5.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 7
segments( 6.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(6.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 8
segments( 7.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(7.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 11
segments( 8.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(8.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

# Cluster 12
segments( 9.9, (coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds -
                  coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.9, y1=(coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds +
                        coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
points(9.9, coef(summary(gls(log_chao.reds ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.reds, pch=20, col="red")

segments(10.9, coef(summary(gls_clust_reds))[11,1]- mean.coef.reds - coef(summary(gls_clust_reds))[11,2],  # Habitat diversity
         x1=10.9, y1=coef(summary(gls_clust_reds))[11,1]- mean.coef.reds + coef(summary(gls_clust_reds))[11,2], col="red")
points(10.9, (coef(summary(gls_clust_reds))[11,1]- mean.coef.reds), pch=20, col="red")


## Alien ####
# Cluster 1
segments( 1.1, coef(summary(gls_clust_blacks))[1,1]- mean.coef.blacks - coef(summary(gls_clust_blacks))[1,2],  
          x1=1.1, y1=coef(summary(gls_clust_blacks))[1,1]- mean.coef.blacks + coef(summary(gls_clust_blacks))[1,2], col="black")
points(1.1, (coef(summary(gls_clust_blacks))[1,1]- mean.coef.blacks), pch=20, col="black")

# Cluster 2
segments( 2.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(2.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 3
segments( 3.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(3.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 4
segments( 4.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(4.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 5
segments( 5.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(5.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 6
segments( 6.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(6.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 7
segments( 7.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(7.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 8
segments( 8.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(8.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 11
segments( 9.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                  coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                        coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(9.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Cluster 12
segments( 10.1, (coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks -
                   coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=10.1, y1=(coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks +
                         coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="black")
points(10.1, coef(summary(gls(log_chao.blacks ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.blacks, pch=20, col="black")

# Habitat heterogeneity    
segments( 11.1, coef(summary(gls_clust_blacks))[11,1]- mean.coef.blacks - coef(summary(gls_clust_blacks))[11,2],
          x1=11.1, y1=coef(summary(gls_clust_blacks))[11,1]- mean.coef.blacks + coef(summary(gls_clust_blacks))[11,2])
points(11.1, (coef(summary(gls_clust_blacks))[11,1]- mean.coef.blacks), pch=20)


## All ####
# Cluster 1
segments( 1, coef(summary(gls_clust_all))[1,1]- mean.coef.all - coef(summary(gls_clust_all))[1,2],  
          x1=1, y1=coef(summary(gls_clust_all))[1,1]- mean.coef.all + coef(summary(gls_clust_all))[1,2], col="blue")
points(1, (coef(summary(gls_clust_all))[1,1]- mean.coef.all), pch=20, col="blue")

# Cluster 2
segments( 2, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=2, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(2, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 3
segments( 3, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=3, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(3, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 4
segments( 4, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=4, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(4, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 5
segments( 5, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=5, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(5, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 6
segments( 6, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=6, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(6, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 7
segments( 7, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=7, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(7, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 8
segments( 8, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=8, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(8, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 11
segments( 9, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                 correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                  coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                   correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=9, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                       correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                        coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                         correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(9, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                             correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Cluster 12
segments( 10, (coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all -
                   coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
          x1=10, y1=(coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all +
                         coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="blue")
points(10, coef(summary(gls(log_chao.all ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                              correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1]- mean.coef.all, pch=20, col="blue")

# Hab. heterogenity    
segments( 11, coef(summary(gls_clust_all))[11,1]- mean.coef.all - coef(summary(gls_clust_all))[11,2],  
          x1=11, y1=coef(summary(gls_clust_all))[11,1]- mean.coef.all + coef(summary(gls_clust_all))[11,2], col="blue")
points(11, (coef(summary(gls_clust_all))[11,1]- mean.coef.all), pch=20, col="blue")


legend("topright", legend=c("Threatened", "Alien", "All"), lty=1, col=c("red", "black", "blue"), cex=0.75)

##-------####


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
data_predict_clust$predict_reds <- predict(gls_clust_reds, newdata=data_predict_clust)
data_predict_clust$predict_blacks <- predict(gls_clust_blacks, newdata=data_predict_clust)
data_predict_clust$predict_all <- predict(gls_clust_all, newdata=data_predict_clust)


range(data_predict_clust$predict_reds)        
range(data_predict_clust$predict_blacks)
range(data_predict_clust$predict_all)

# Back-transform the predictions:
data_predict_clust$predict_blacks_number <- (exp(data_predict_clust@data$predict_blacks))-1
data_predict_clust$predict_reds_number <- (exp(data_predict_clust@data$predict_reds))-1
data_predict_clust$predict_all_number <- (exp(data_predict_clust@data$predict_all))-1

range(data_predict_clust$predict_reds_number)
range(data_predict_clust$predict_blacks_number)
range(data_predict_clust$predict_all_number)



##--- 3.5.1 Make the vectors with colour names           ---####
##----------------------------------------------------------####
# Get the numbers to base the colour on:
col_ESR_red_vec <- c(TrdRast_clust_model@data$log_chao.reds)
col_ESR_black_vec <- c(TrdRast_clust_model@data$log_chao.blacks)
col_pred_red_vec <- c(data_predict_clust@data$predict_reds)
col_pred_black_vec <- c(data_predict_clust@data$predict_blacks)

# The vectors with colour names needs to have the same range, if the maps are to be comparable
# Chao1:
col_ESR_red <- rep(0, length(col_ESR_red_vec))
for(i in 1:length(col_ESR_red_vec)){
  col_ESR_red[i] <- ifelse(col_ESR_red_vec[i]>-0.5 & col_ESR_red_vec[i]<=0, paste("#7F00FFFF"),
                           ifelse(col_ESR_red_vec[i]>0 & col_ESR_red_vec[i]<=0.5, paste("#001AFFFF"),
                           ifelse(col_ESR_red_vec[i]>0.5 & col_ESR_red_vec[i]<=1, paste("#00B3FFFF"),
                           ifelse(col_ESR_red_vec[i]>1 & col_ESR_red_vec[i]<=1.5, paste("#00FFFFFF"),
                           ifelse(col_ESR_red_vec[i]>1.5 & col_ESR_red_vec[i]<=2, paste("#00FF19FF"),
                           ifelse(col_ESR_red_vec[i]>2 & col_ESR_red_vec[i]<=2.5, paste("#80FF00FF"),
                           ifelse(col_ESR_red_vec[i]>2.5 & col_ESR_red_vec[i]<=3, paste("#FFE500FF"),
                           ifelse(col_ESR_red_vec[i]>3 & col_ESR_red_vec[i]<=3.5, paste("#FF9900FF"),
                           ifelse(col_ESR_red_vec[i]>3.5 & col_ESR_red_vec[i]<=4, paste("#FF4D00FF"),
                           ifelse(col_ESR_red_vec[i]>4 & col_ESR_red_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

col_ESR_black <- rep(0, length(col_ESR_black_vec))
for(i in 1:length(col_ESR_black_vec)){
  col_ESR_black[i] <- ifelse(col_ESR_black_vec[i]>-0.5 & col_ESR_black_vec[i]<=0, paste("#7F00FFFF"),
                             ifelse(col_ESR_black_vec[i]>0 & col_ESR_black_vec[i]<=0.5, paste("#001AFFFF"),
                             ifelse(col_ESR_black_vec[i]>0.5 & col_ESR_black_vec[i]<=1, paste("#00B3FFFF"),
                             ifelse(col_ESR_black_vec[i]>1 & col_ESR_black_vec[i]<=1.5, paste("#00FFFFFF"),
                             ifelse(col_ESR_black_vec[i]>1.5 & col_ESR_black_vec[i]<=2, paste("#00FF19FF"),
                             ifelse(col_ESR_black_vec[i]>2 & col_ESR_black_vec[i]<=2.5, paste("#80FF00FF"),
                             ifelse(col_ESR_black_vec[i]>2.5 & col_ESR_black_vec[i]<=3, paste("#FFE500FF"),
                             ifelse(col_ESR_black_vec[i]>3 & col_ESR_black_vec[i]<=3.5, paste("#FF9900FF"),
                             ifelse(col_ESR_black_vec[i]>3.5 & col_ESR_black_vec[i]<=4, paste("#FF4D00FF"),
                             ifelse(col_ESR_black_vec[i]>4 & col_ESR_black_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

# Predicted:
col_pred_red <- rep(0, length(col_pred_red_vec))
for(i in 1:length(col_pred_red_vec)){
  col_pred_red[i] <- ifelse(col_pred_red_vec[i]>-0.5 & col_pred_red_vec[i]<=0, paste("#7F00FFFF"),
                            ifelse(col_pred_red_vec[i]>0 & col_pred_red_vec[i]<=0.5, paste("#001AFFFF"),
                            ifelse(col_pred_red_vec[i]>0.5 & col_pred_red_vec[i]<=1, paste("#00B3FFFF"),
                            ifelse(col_pred_red_vec[i]>1 & col_pred_red_vec[i]<=1.5, paste("#00FFFFFF"),
                            ifelse(col_pred_red_vec[i]>1.5 & col_pred_red_vec[i]<=2, paste("#00FF19FF"),
                            ifelse(col_pred_red_vec[i]>2 & col_pred_red_vec[i]<=2.5, paste("#80FF00FF"),
                            ifelse(col_pred_red_vec[i]>2.5 & col_pred_red_vec[i]<=3, paste("#FFE500FF"),
                            ifelse(col_pred_red_vec[i]>3 & col_pred_red_vec[i]<=3.5, paste("#FF9900FF"),
                            ifelse(col_pred_red_vec[i]>3.5 & col_pred_red_vec[i]<=4, paste("#FF4D00FF"),
                            ifelse(col_pred_red_vec[i]>4 & col_pred_red_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}

col_pred_black <- rep(0, length(col_pred_black_vec))
for(i in 1:length(col_pred_black_vec)){
  col_pred_black[i] <- ifelse(col_pred_black_vec[i]>-0.5 & col_pred_black_vec[i]<=0, paste("#7F00FFFF"),
                              ifelse(col_pred_black_vec[i]>0 & col_pred_black_vec[i]<=0.5, paste("#001AFFFF"),
                              ifelse(col_pred_black_vec[i]>0.5 & col_pred_black_vec[i]<=1, paste("#00B3FFFF"),
                              ifelse(col_pred_black_vec[i]>1 & col_pred_black_vec[i]<=1.5, paste("#00FFFFFF"),
                              ifelse(col_pred_black_vec[i]>1.5 & col_pred_black_vec[i]<=2, paste("#00FF19FF"),
                              ifelse(col_pred_black_vec[i]>2 & col_pred_black_vec[i]<=2.5, paste("#80FF00FF"),
                              ifelse(col_pred_black_vec[i]>2.5 & col_pred_black_vec[i]<=3, paste("#FFE500FF"),
                              ifelse(col_pred_black_vec[i]>3 & col_pred_black_vec[i]<=3.5, paste("#FF9900FF"),
                              ifelse(col_pred_black_vec[i]>3.5 & col_pred_black_vec[i]<=4, paste("#FF4D00FF"),
                              ifelse(col_pred_black_vec[i]>4 & col_pred_black_vec[i]<4.5, paste("#FF0000FF"), '#BEBEBE'))))))))))
}




##--- 3.5.2 Make the maps                                ---####
##----------------------------------------------------------####
#par(mfrow=c(2,2))
#par(mar=c(0.5,0.5,6,1))

layout(rbind(c(1,2,3), c(4,5,3)), widths=c(4,4,1))
par(mar=c(0.5,0.5,6,0.5))

# Threatened species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of threatened species) \n in 500m x 500m cell")
plot(TrdRast_clust_model[, "log_chao.reds"],
     col=col_ESR_red, border=col_ESR_red, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell \n(area of land cover)")
plot(data_predict_clust,
     col=col_pred_red, border=col_pred_red, add=T, cex.main=0.75)

plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("-3-(-1)", "-1-0", "0-0.5", "0.5-1", "1-1.5",
                          "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5-4"),
       fill=c("#7F00FFFF", "#001AFFFF", "#00B3FFFF", "#00FFFFFF", "#00FF19FF",
              "#80FF00FF", "#FFE500FF", "#FF9900FF", "#FF4D00FF", "#FF0000FF"), bty="n", cex=1)


# Alien species:
DivMap(AR5, Trondheim, TrdRast_AR5, "log(ESR of alien species) \nin 500m x 500m cell")
plot(TrdRast_clust_model[, "log_chao.blacks"],
     col=col_ESR_black, border=col_ESR_black, add=T, cex.main=0.75)

DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell \n(area of land cover)")
plot(data_predict_clust,
     col=col_pred_black, border=col_pred_black, add=T, cex.main=0.75)


##--- 3.5.3 BETTER MAPS ---####
##-------------------------####
library(RColorBrewer)
col.reds <- colorRampPalette(c("white","#FF0000"))
col.blacks <- colorRampPalette(c("white","black"))
col.all <- colorRampPalette(c("white", "blue"))

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

palette(col.all(144))
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of all \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col= data_predict_clust@data$predict_all_number,
     border=data_predict_clust@data$predict_all_number,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=(0:(max(data_predict_clust@data$predict_all_number))),
      z=t(0:(max(data_predict_clust@data$predict_all_number))),
      col=palette(col.all(144)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

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


palette(col.reds(16))
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of threatened \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col= data_predict_clust@data$predict_reds_number + abs(min(data_predict_clust@data$predict_reds_number)),
     border=data_predict_clust@data$predict_reds_number + abs(min(data_predict_clust@data$predict_reds_number)),
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=(min(data_predict_clust@data$predict_reds_number)+abs(min(data_predict_clust@data$predict_reds_number))):(max(data_predict_clust@data$predict_reds_number)+abs(min(data_predict_clust@data$predict_reds_number))),
      z=t((min(data_predict_clust@data$predict_reds_number)):(max(data_predict_clust@data$predict_reds_number))),
      col=palette(col.reds(16)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))

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

palette(col.blacks(34))
par(mar=c(0.5,0.5,6,0.5))
DivMap(AR5, Trondheim, TrdRast_AR5, "Modelled richness of alien \nspecies in 500m x 500m cell")
plot(data_predict_clust,
     col=(data_predict_clust@data$predict_blacks_number + abs(min(data_predict_clust@data$predict_blacks_number)))*10,
     border=(data_predict_clust@data$predict_blacks_number + abs(min(data_predict_clust@data$predict_blacks_number)))*10,
     add=T, cex.main=0.75)
plot(Trondheim, add=TRUE, lty=2) 
par(mar=c(0.5,0.5,6,4))
image(y=((min(data_predict_clust@data$predict_blacks_number)+abs(min(data_predict_clust@data$predict_blacks_number)))*10):((max(data_predict_clust@data$predict_blacks_number)+abs(min(data_predict_clust@data$predict_blacks_number)))*10),
      z=t(((min(data_predict_clust@data$predict_blacks_number))*10):((max(data_predict_clust@data$predict_blacks_number))*10)),
      col=palette(col.blacks(34)), axes=FALSE, main="# \nspecies", cex.main=.75)
axis(4, cex.axis=0.8, mgp=c(0,.5,0),
     labels=c("0", "0.5", "1", "1.5", "2", "2.5", "3"),
     at=c(0,5,10,15,20,25,30))


##--- 4. EXTRA MODEL VALIDATION  ---####
##----------------------------------####
# Get the useful data for comparing the Observed species richness and the predicted species richness - 
# there should not be a correlation here. Otherwise, we see an effect of sampling effort

val.plot <- TrdRast_clust_model@data[, c("Pixelnr", "S.obs_2013", "S.chao1_2013", "S.obs_reds_2013", "S.chao1_reds_2013",
                                         "S.obs_blacks_2013", "S.chao1_blacks_2013", "Ntotal", "Nred", "Nblack")]
val.plot <- merge(val.plot, data_predict_clust@data[,c("Pixelnr", "predict_all_number", "predict_reds_number",
                                                       "predict_blacks_number")], by="Pixelnr")

par(mar=c(5,4,4,2))
par(mfrow=c(3,4))
plot(log(val.plot$Ntotal), val.plot$predict_all_number, ylab="Predicted # species, all", xlab="log(# records, all sp.)")
plot(val.plot$S.obs_2013, val.plot$predict_all_number, ylab="Predicted # species, all", xlab="Observed # species, all")
plot(0,type='n',axes=FALSE,ann=FALSE)
plot(0,type='n',axes=FALSE,ann=FALSE)

plot(log(val.plot$Ntotal), val.plot$predict_reds_number, ylab="Predicted # species, treatened", xlab="log(# records, all)")
plot(log(val.plot$Nred), val.plot$predict_reds_number, ylab="Predicted # species, treatened", xlab="log(# records, threatened)")
plot(val.plot$S.obs_2013, val.plot$predict_reds_number, ylab="Predicted # species, treatened", xlab="Observed # species, all")
plot(val.plot$S.obs_reds_2013, val.plot$predict_reds_number, ylab="Predicted # species, threatened", xlab="Observed # species, threatened")

plot(log(val.plot$Ntotal), val.plot$predict_blacks_number, ylab="Predicted # species, alien", xlab="log(# records, all)")
plot(log(val.plot$Nblack), val.plot$predict_blacks_number, ylab="Predicted # species, alien", xlab="log(# records, alien)")
plot(val.plot$S.obs_2013, val.plot$predict_blacks_number, ylab="Predicted # species, alien", xlab="Observed # species, all")
plot(val.plot$S.obs_blacks_2013, val.plot$predict_blacks_number, ylab="Predicted # species, alien", xlab="Observed # species, alien")


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
