# After comments on my presentation at the CBD: I'll try to "smooth out" the habitat classification.
# Instead of letting it depend on only the land cover within the grid cell, let it depend on the 
# land cover within the cell AND the 1st-order neighbouring cells.

# Other comments were regarding (1) the taxonomical extent and (2) the spatial resolution:
# 1)  As birds are the dominating group, they might be influencing the results disproportionally.
#     It might be useful to split the dataset, and do the analyses on e.g. birds and plants separately
# 2)  500*500m^2 might not be the best choice of scale. I can try doing the abalyses with different
#     resolutions, and potentially compare the results, hopefully to show that the results are similar
# 3)  Do both of these things simultaneously

#####################################

##--- 1. RE-CLASSIFYING HABITAT CLUSTERS --####
##-----------------------------------------####
library(vegan) 
library(dplyr)
TrdRast_clust_smooth <- TrdRast_AR5
TrdRast_clust_smooth <- TrdRast_clust_smooth[!is.na(TrdRast_clust_smooth@data$total_area),]
TrdRast_clust_smooth <- TrdRast_clust_smooth[!(TrdRast_clust_smooth@data$Ocean==TrdRast_clust_smooth@data$total_area),]

# We need to make a new dissimilarity matrix, where we compare the land cover within the focal
# cell AND its neighbours with the others.
# First, identify the neighbouring cells
adj <- gTouches(TrdRast_clust_smooth, byid=TRUE)
      # To make it a bit easier to work with in the following:
      adj <- ifelse(adj==TRUE, as.character("true"), NA)
      # Replace "true" with column name
      ones <- which(adj=="true", arr.ind=T)
      adj[ones]<-colnames(adj)[ones[,2]]
      ###adj[] <- lapply(adj, function(x) as.numeric(as.character(x)))
      
      
# Recalculate the surrounding area of each land cover type for each grid cell, based on the
# identified neighbouring cells
Trd_smooth <- TrdRast_clust_smooth@data[,c(1,3:68)]

adj.cover <- list()   # Subset to only have the land cover of the neighbour cells in each list element
    for(i in 1:1509){
      adj.cover[[i]] <- subset(Trd_smooth, Pixelnr %in% as.numeric((adj[i, !is.na(adj[i,])])) )
    }
    names(adj.cover) <- Trd_smooth$Pixelnr
    
adj.cover_smooth <- list()   # Sum the rows to get the total and cover in the neighbour cells
    for(i in 1:1509){
      adj.cover_smooth[[i]] <- colSums(adj.cover[[i]]) 
    }
    names(adj.cover_smooth) <- Trd_smooth$Pixelnr

for(i in 1:1509){         # Fill in the Trd_smooth with the focal and neighbour cells area cover
  for(j in 2:67){
    Trd_smooth[i,j] <- sapply(adj.cover_smooth[i], function(x) (x[j]))[[1]]  + Trd_smooth[i,j]
  }
}
    
TrdRast_clust_smooth@data[,c(3:68)] <- Trd_smooth[,c(2:67)]    # "Put it back" into the spatial dataframe


# Make the cluster-dendrogram based on a distance matrix with Bray-Curtis similarity
clusters_smooth <- hclust(vegdist(TrdRast_clust_smooth@data[, 3:68], method="bray"))
par(mfrow=c(1,2))
par(mar=c(4.1,4.1,5.1,2.1))
plot(clusters, cex=0.5, main="Focal grid cell", xlab="Grid cell number")
      abline(h=0.99, col="red", lty=2)   
plot(clusters_smooth, cex=0.5, main="Neighbour grid cells", xlab="Grid cell number")
      abline(h=0.99, col="red", lty=2) 

# Identify the optimal number of clusters:
library(NbClust)
nclust_smooth <- NbClust(TrdRast_clust_smooth@data[, 3:68], diss=vegdist(TrdRast_clust_smooth@data[, 3:68], method="bray"), distance=NULL,
                        min.nc = 2, max.nc = 25, method="complete")
      
      #nclust$Best.nc   # Best number of clusters: 5, according to the majority rule

# Have a look at the number of cluster by different cut-offs
### Cut-off 5 clusters ###
{
  clusterCut_smooth <- as.data.frame(cutree(clusters_smooth, h=0.99))
table(clusterCut_smooth)
names(clusterCut_smooth) <- c("clusterCut_smooth")
# In all cases, we end up with something quite different campared to the previous analyses!

clusterCut_smooth$Pixelnr <- TrdRast_clust_smooth@data$Pixelnr
TrdRast_clust_smooth <- merge(TrdRast_clust_smooth, clusterCut_smooth, by="Pixelnr")
TrdRast_clust_smooth$col.clust <- NA
# Make the vector with colour names (redo this multiple times until the colours are reasonable):
for(i in 1:length(TrdRast_clust_smooth@data$clusterCut_smooth)){
  TrdRast_clust_smooth@data$col.clust[i] <- ifelse(TrdRast_clust_smooth@data$clusterCut_smooth[i]==1, paste("hotpink"),
                                            ifelse(TrdRast_clust_smooth@data$clusterCut_smooth[i]==2, paste("forestgreen"),
                                            ifelse(TrdRast_clust_smooth@data$clusterCut_smooth[i]==3, paste("orange"),
                                            ifelse(TrdRast_clust_smooth@data$clusterCut_smooth[i]==4, paste("palegreen"),
                                            ifelse(TrdRast_clust_smooth@data$clusterCut_smooth[i]==5, paste("cyan"), NA
                                            )))))
}

TrdRast_clust_smooth@data$col.clust <- as.factor(TrdRast_clust_smooth@data$col.clust)

# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust_smooth, main="Clusters_smoothed",
     col=as.character(TrdRast_clust_smooth@data$col.clust))  
par(mar=c(5,1,5,2.5))
image(y=1:5,z=t(1:5), col=c("hotpink", "forestgreen", "orange", "palegreen", "cyan"),
      axes=FALSE, main="clusterCut", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))
}

### Cut-off 9 clusters ###
{
clusterCut_smooth2 <- as.data.frame(cutree(clusters_smooth, h=0.91))
  table(clusterCut_smooth2)
  names(clusterCut_smooth2) <- c("clusterCut_smooth2")
  # In all cases, we end up with somethin quite different campared to the previous analyses!
  
  clusterCut_smooth2$Pixelnr <- TrdRast_clust_smooth@data$Pixelnr
  
  TrdRast_clust_smooth <- merge(TrdRast_clust_smooth, clusterCut_smooth2, by="Pixelnr")
  
TrdRast_clust_smooth$col.clust2 <- NA
# Make the vector with colour names (redo this multiple times until the colours are reasonable):
for(i in 1:length(TrdRast_clust_smooth@data$clusterCut_smooth2)){
    TrdRast_clust_smooth@data$col.clust2[i] <- ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==1, paste("blue"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==2, paste("forestgreen"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==3, paste("hotpink"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==4, paste("orange"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==5, paste("brown"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==6, paste("palegreen"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==7, paste("gray"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==8, paste("violet"),
                                                     ifelse(TrdRast_clust_smooth@data$clusterCut_smooth2[i]==9, paste("cyan"),NA
                                                     )))))))))
  }
  
TrdRast_clust_smooth@data$col.clust2 <- as.factor(TrdRast_clust_smooth@data$col.clust2)
  
# Plot the grid coloured according to cluster:
layout(t(1:2),widths=c(6,1))
par(mar=c(1,1,3,1))
plot(TrdRast_clust_smooth, main="Clusters_smoothed2",
     col=as.character(TrdRast_clust_smooth@data$col.clust2))  
par(mar=c(5,1,5,2.5))
image(y=1:9,z=t(1:9), col=c("blue", "forestgreen", "hotpink", "orange", "brown", "palegreen", "gray", "violet", "cyan"),
      axes=FALSE, main="clusterCut", cex.main=.6)
axis(4,cex.axis=0.8,mgp=c(0,.5,0))
}

### Spineplot 1 ###
{# Make a dataframe with cluster as column and habitat as rows.
# In each entry is then the average of that habitat type for all cells within that cluster.
spine_smooth <- matrix(nrow=5, ncol = 66)
colnames(spine_smooth) <- colnames(TrdRast_AR5@data[3:68])
rownames(spine_smooth) <- c(1:5)
# Calculate the mean of habitat in the grid cells included in each cluster:
for(i in 1:dim(spine_smooth)[1]) {
  for(j in 1:dim(spine_smooth)[2]) {
    spine_smooth[i,j] = mean(TrdRast_clust_smooth@data[TrdRast_clust_smooth@data$clusterCut_smooth==i, colnames(spine_smooth)[j]])
  }
}

layout(t(1:2),widths=c(2,1))
par(mar=c(11,.1,2,2))
par(las=2)
par(cex.axis=0.8)
spineplot(spine_smooth, main="",
          col = c("hotpink", "lightpink", rep("forestgreen", 15), rep("darkolivegreen1", 11),
                  rep("darkolivegreen3", 10), "dodgerblue", rep("darkorange",2), rep("khaki1", 4),
                  rep("cyan", 9), "navy", rep("sandybrown", 7),
                  rep("gold",3), "gray"),
          xlab="", ylab="Mean cover of habitat in grid cells",
          #xaxlabels = c("(1) Coastal", "(2) Urban/developed", "(3) Urban/vegetated/riparian",
                        #"(4) Cultivated", "(5) Conif. forest, \nlow production", "(6) Conif. forest, \nmedium production",
                        #"(7) Open marsh and \nconif. forest", "(8) Conif. forest, \nhigh production",
                        #"(10) Open firm ground \n and forest", "(11) Open firm ground \nand cultivated land",
                        #"(12) Freshwater"),
          yaxlabels = "", border=NA)

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
              "gold", "gray"), border=NA, cex=1)}
### Spineplot 2 ###
{# Make a dataframe with cluster as column and habitat as rows.
  # In each entry is then the average of that habitat type for all cells within that cluster.
  spine_smooth2 <- matrix(nrow=9, ncol = 66)
  colnames(spine_smooth2) <- colnames(TrdRast_AR5@data[3:68])
  rownames(spine_smooth2) <- c(1:9)
  # Calculate the mean of habitat in the grid cells included in each cluster:
  for(i in 1:dim(spine_smooth2)[1]) {
    for(j in 1:dim(spine_smooth2)[2]) {
      spine_smooth2[i,j] = mean(TrdRast_clust_smooth@data[TrdRast_clust_smooth@data$clusterCut_smooth2==i, colnames(spine_smooth2)[j]])
    }
  }
  
  layout(t(1:2),widths=c(2,1))
  par(mar=c(11,.1,2,2))
  par(las=2)
  par(cex.axis=0.8)
  spineplot(spine_smooth2, main="",
            col = c("hotpink", "lightpink", rep("forestgreen", 15), rep("darkolivegreen1", 11),
                    rep("darkolivegreen3", 10), "dodgerblue", rep("darkorange",2), rep("khaki1", 4),
                    rep("cyan", 9), "navy", rep("sandybrown", 7),
                    rep("gold",3), "gray"),
            xlab="", ylab="Mean cover of habitat in grid cells",
            #xaxlabels = c("(1) Coastal", "(2) Urban/developed", "(3) Urban/vegetated/riparian",
            #"(4) Cultivated", "(5) Conif. forest, \nlow production", "(6) Conif. forest, \nmedium production",
            #"(7) Open marsh and \nconif. forest", "(8) Conif. forest, \nhigh production",
            #"(10) Open firm ground \n and forest", "(11) Open firm ground \nand cultivated land",
            #"(12) Freshwater"),
            yaxlabels = "", border=NA)
  
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
  }

par(las=1)
par(cex.axis=1)


## I am not entirely sure what to gather from this. This definitely "smoothes" the habitat categories, 
## but potentially to a degree where it is no longer useful.
## I need to have a chat with my supervisors regarding this




##--- 2. SPLIT THE DATASET AND DO THE ANALYSES ON BIRDS AND PLANTS SEPARATELY ---####
##--- 2.1 Split and make community matrices                                   ---####
##-------------------------------------------------------------------------------####
# Make a smaller, regular dataframe to speed up the proces (only including scientific name and fPixelnr)
# Check that the column numbers are correct!
# In case there are issues, run some of the calculation from the "Data download and cleaning" first
birds <- GBIF.trd_2013[GBIF.trd_2013$class=="Aves",]
plants <- GBIF.trd_2013[GBIF.trd_2013$kingdom=="Plantae",]
GBIF_other <- GBIF.trd_2013[!(GBIF.trd_2013$kingdom=="Plantae" |
                                                GBIF.trd_2013$class=="Aves"),]
birds@data <- droplevels(birds@data)
plants@data <- droplevels(plants@data)
GBIF_other@data <- droplevels(GBIF_other@data)

rar.birds <- as.data.frame(birds@data[,c("species", "Pixelnr")])
rar.birds$Pixelnr <- as.factor(rar.birds$Pixelnr)
rar.birds <- droplevels(rar.birds)
rar.plants <- as.data.frame(plants@data[,c("species", "Pixelnr")])
rar.plants$Pixelnr <- as.factor(rar.plants$Pixelnr)
rar.plants <- droplevels(rar.plants)
rar.GBIF_other <- as.data.frame(GBIF_other@data[,c("species", "Pixelnr")])
rar.GBIF_other$Pixelnr <- as.factor(rar.GBIF_other$Pixelnr)
rar.GBIF_other <- droplevels(rar.GBIF_other)

# Create empty matrix
est_birds <- matrix(data=NA, ncol=nlevels(rar.birds$species), nrow=nlevels((rar.birds$Pixelnr)))
est_plants <- matrix(data=NA, ncol=nlevels(rar.plants$species), nrow=nlevels((rar.plants$Pixelnr)))
est_GBIF_other <- matrix(data=NA, ncol=nlevels(rar.GBIF_other$species), nrow=nlevels((rar.GBIF_other$Pixelnr)))

# Add column names and row names (species names and Pixelnr)
colnames(est_birds) <- levels(rar.birds$species)
rownames(est_birds) <- levels(rar.birds$Pixelnr)
colnames(est_plants) <- levels(rar.plants$species)
rownames(est_plants) <- levels(rar.plants$Pixelnr)
colnames(est_GBIF_other) <- levels(rar.GBIF_other$species)
rownames(est_GBIF_other) <- levels(rar.GBIF_other$Pixelnr)

# Make a tally of the rar.data
tallied_birds <- rar.birds %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied_plants <- rar.plants %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied_GBIF_other <- rar.GBIF_other %>%
  group_by(species, Pixelnr) %>%
  tally()


##--- 2.1.2 RED- AND BLACKLISTED ---####
##----------------------------------####
# Red and blacklisted observations only:
red_birds <- reds_2013[reds_2013$class=="Aves",]
black_birds <- blacks_2013[blacks_2013$class=="Aves",]

red_plants <- reds_2013[reds_2013$kingdom=="Plantae",]
black_plants <- blacks_2013[blacks_2013$kingdom=="Plantae",]

red_other <- reds_2013[!(reds_2013$kingdom=="Plantae" |
                                reds_2013$class=="Aves"),]
black_other <- blacks_2013[!(blacks_2013$kingdom=="Plantae" |
                           blacks_2013$class=="Aves"),]

red_birds@data <- droplevels(red_birds@data)
red_plants@data <- droplevels(red_plants@data)
red_other@data <- droplevels(red_other@data)
black_birds@data <- droplevels(black_birds@data)
black_plants@data <- droplevels(black_plants@data)
black_other@data <- droplevels(black_other@data)

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
rar.red_birds <- as.data.frame(red_birds@data[,c("species", "Pixelnr")])
rar.red_birds$Pixelnr <- as.factor(rar.red_birds$Pixelnr)
rar.red_birds <- droplevels(rar.red_birds)
rar.red_plants <- as.data.frame(red_plants@data[,c("species", "Pixelnr")])
rar.red_plants$Pixelnr <- as.factor(rar.red_plants$Pixelnr)
rar.red_plants <- droplevels(rar.red_plants)
rar.red_other <- as.data.frame(red_other@data[,c("species", "Pixelnr")])
rar.red_other$Pixelnr <- as.factor(rar.red_other$Pixelnr)
rar.red_other <- droplevels(rar.red_other)

rar.black_birds <- as.data.frame(black_birds@data[,c("species", "Pixelnr")])
rar.black_birds$Pixelnr <- as.factor(rar.black_birds$Pixelnr)
rar.black_birds <- droplevels(rar.black_birds)
rar.black_plants <- as.data.frame(black_plants@data[,c("species", "Pixelnr")])
rar.black_plants$Pixelnr <- as.factor(rar.black_plants$Pixelnr)
rar.black_plants <- droplevels(rar.black_plants)
rar.black_other <- as.data.frame(black_other@data[,c("species", "Pixelnr")])
rar.black_other$Pixelnr <- as.factor(rar.black_other$Pixelnr)
rar.black_other <- droplevels(rar.black_other)

# Create empty matrix
est.red_birds <- matrix(data=NA, ncol=nlevels(rar.red_birds$species), nrow=nlevels(rar.red_birds$Pixelnr))
est.black_birds <- matrix(data=NA, ncol=nlevels(rar.black_birds$species), nrow=nlevels(rar.black_birds$Pixelnr))
est.red_plants <- matrix(data=NA, ncol=nlevels(rar.red_plants$species), nrow=nlevels(rar.red_plants$Pixelnr))
est.black_plants <- matrix(data=NA, ncol=nlevels(rar.black_plants$species), nrow=nlevels(rar.black_plants$Pixelnr))
est.red_other <- matrix(data=NA, ncol=nlevels(rar.red_other$species), nrow=nlevels(rar.red_other$Pixelnr))
est.black_other <- matrix(data=NA, ncol=nlevels(rar.black_other$species), nrow=nlevels(rar.black_other$Pixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est.red_birds) <- levels(rar.red_birds$species)
rownames(est.red_birds) <- levels(rar.red_birds$Pixelnr)
colnames(est.black_birds) <- levels(rar.black_birds$species)
rownames(est.black_birds) <- levels(rar.black_birds$Pixelnr)
colnames(est.red_plants) <- levels(rar.red_plants$species)
rownames(est.red_plants) <- levels(rar.red_plants$Pixelnr)
colnames(est.black_plants) <- levels(rar.black_plants$species)
rownames(est.black_plants) <- levels(rar.black_plants$Pixelnr)
colnames(est.red_other) <- levels(rar.red_other$species)
rownames(est.red_other) <- levels(rar.red_other$Pixelnr)
colnames(est.black_other) <- levels(rar.black_other$species)
rownames(est.black_other) <- levels(rar.black_other$Pixelnr)

# Make a tally of the rar.data
tallied.red_birds <- rar.red_birds %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.black_birds <- rar.black_birds %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.red_plants <- rar.red_plants %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.black_plants <- rar.black_plants %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.red_other <- rar.red_other %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.black_other <- rar.black_other %>%
  group_by(species, Pixelnr) %>%
  tally()


##--- 2.2 RUN THE FUNCTION AND FILL IN THE COMMUNITY MATRIX  ---####
##--------------------------------------------------------------####

## ALL OBSERVATIONS:
# Birds
for(r in 1:dim(est_birds)[1]){
  for(c in 1:dim(est_birds)[2]){
    est_birds[r,c]=ntally(i=r, j=c, Tally=tallied_birds, Com.matrix=est_birds)}
}

write.csv(est_birds, file="Comm_matrix_birds.csv")

# Plants
for(r in 1:dim(est_plants)[1]){
  for(c in 1:dim(est_plants)[2]){
    est_plants[r,c]=ntally(i=r, j=c, Tally=tallied_plants, Com.matrix=est_plants)}
}

write.csv(est_plants, file="Comm_matrix_plants.csv")

# Other
est1_other <- est_GBIF_other[,c(1:962)]      
est2_other <- est_GBIF_other[,c(963:1923)] 
# 1. 
for(r in 1:dim(est1_other)[1]){
  for(c in 1:dim(est1_other)[2]){
    est1_other[r,c]=ntally(i=r, j=c, Tally=tallied_GBIF_other, Com.matrix=est1_other)}
}

# 2.
for(r in 1:dim(est2_other)[1]){
  for(c in 1:dim(est2_other)[2]){
    est2_other[r,c]=ntally(i=r, j=c, Tally=tallied_GBIF_other, Com.matrix=est2_other)}
}

# Combine the small matrices to one large community matrix
est_GBIF_other <- cbind(est1_other, est2_other) 

write.csv(est_GBIF_other, file="Comm_matrix_other.csv")


### REDLISTED
for(r in 1:dim(est.red_birds)[1]){
  for(c in 1:dim(est.red_birds)[2]){
    est.red_birds[r,c]=ntally(i=r, j=c, Tally=tallied.red_birds, Com.matrix=est.red_birds)}
}

for(r in 1:dim(est.red_plants)[1]){
  for(c in 1:dim(est.red_plants)[2]){
    est.red_plants[r,c]=ntally(i=r, j=c, Tally=tallied.red_plants, Com.matrix=est.red_plants)}
}

for(r in 1:dim(est.red_other)[1]){
  for(c in 1:dim(est.red_other)[2]){
    est.red_other[r,c]=ntally(i=r, j=c, Tally=tallied.red_other, Com.matrix=est.red_other)}
}

write.csv(est.red_birds, file="Comm_matrix_red_brids.csv")
write.csv(est.red_plants, file="Comm_matrix_red_plants.csv")
write.csv(est.red_other, file="Comm_matrix_red_other.csv")

### BLACKLISTED
for(r in 1:dim(est.black_birds)[1]){
  for(c in 1:dim(est.black_birds)[2]){
    est.black_birds[r,c]=ntally(i=r, j=c, Tally=tallied.black_birds, Com.matrix=est.black_birds)}
}

for(r in 1:dim(est.black_plants)[1]){
  for(c in 1:dim(est.black_plants)[2]){
    est.black_plants[r,c]=ntally(i=r, j=c, Tally=tallied.black_plants, Com.matrix=est.black_plants)}
}

for(r in 1:dim(est.black_other)[1]){
  for(c in 1:dim(est.black_other)[2]){
    est.black_other[r,c]=ntally(i=r, j=c, Tally=tallied.black_other, Com.matrix=est.black_other)}
}

write.csv(est.black_birds, file="Comm_matrix_black_birds.csv")
write.csv(est.black_plants, file="Comm_matrix_black_plants.csv")
write.csv(est.black_other, file="Comm_matrix_black_other.csv")

est_birds <- read.csv("Comm_matrix_birds.csv", row.names = 1)
est_plants <- read.csv("Comm_matrix_plants.csv", row.names = 1)
est_GBIF_other <- read.csv("Comm_matrix_other.csv", row.names = 1)
est.red_birds <- read.csv("Comm_matrix_red_birds.csv", row.names = 1)
est.red_plants <- read.csv("Comm_matrix_red_plants.csv", row.names = 1)
est.red_other <- read.csv("Comm_matrix_red_other.csv", row.names = 1)
est.black_birds <- read.csv("Comm_matrix_black_birds.csv", row.names = 1)
est.black_plants <- read.csv("Comm_matrix_black_plants.csv", row.names = 1)
est.black_other <- read.csv("Comm_matrix_black_other.csv", row.names = 1)

# Check that the community matrices seems reasonable:
dim(est.black_other)
hist(rowSums(est.black_other))
hist(log(rowSums(est.black_birds)))
View(as.data.frame(rowSums(est.red_other)))


##--- 3. CALCULATE THE ESTIMATED SPECIES RICHNESS FOR EACH OF THE GRID CELLS ---####
##------------------------------------------------------------------------------####
# Estimate species richness for each site/grid cell, and save the results. For cells with only one
# observation, the returned value will  be NA. 
library(vegan)
{est.sp_birds <- estimateR(est_birds)
est.sp_red_birds <- estimateR(est.red_birds)
est.sp_black_birds <- estimateR(est.black_birds)
est.sp_plants <- estimateR(est_plants)
est.sp_red_plants <- estimateR(est.red_plants)
est.sp_black_plants <- estimateR(est.black_plants)
est.sp_other <- estimateR(est_GBIF_other)
est.sp_red_other <- estimateR(est.red_other)
est.sp_black_other <- estimateR(est.black_other)}

# Rotate it and make it a dataframe
{est.sp_birds <- as.data.frame(t(est.sp_birds))
    est.sp_birds$Pixelnr <- row.names(est.sp_birds)
est.sp_red_birds <- as.data.frame(t(est.sp_red_birds))
    est.sp_red_birds$Pixelnr <- row.names(est.sp_red_birds)
est.sp_black_birds <- as.data.frame(t(est.sp_black_birds))
    est.sp_black_birds$Pixelnr <- row.names(est.sp_black_birds)
est.sp_plants <- as.data.frame(t(est.sp_plants))
    est.sp_plants$Pixelnr <- row.names(est.sp_plants)
est.sp_red_plants <- as.data.frame(t(est.sp_red_plants))
    est.sp_red_plants$Pixelnr <- row.names(est.sp_red_plants)
est.sp_black_plants <- as.data.frame(t(est.sp_black_plants))
    est.sp_black_plants$Pixelnr <- row.names(est.sp_black_plants)
est.sp_other <- as.data.frame(t(est.sp_other))
    est.sp_other$Pixelnr <- row.names(est.sp_other)
est.sp_red_other <- as.data.frame(t(est.sp_red_other))
    est.sp_red_other$Pixelnr <- row.names(est.sp_red_other)
est.sp_black_other <- as.data.frame(t(est.sp_black_other))
    est.sp_black_other$Pixelnr <- row.names(est.sp_black_other)}

# Add these calculations (Species richness) to the SpatialPointsDataframes (later: also to the TrdRast SpatialPolygonsDataFrame)
{birds <- merge(birds, est.sp_birds, by="Pixelnr", all=TRUE)
red_birds <- merge(red_birds, est.sp_birds, by="Pixelnr", all=TRUE)
black_birds <- merge(black_birds, est.sp_birds, by="Pixelnr", all=TRUE)
plants <- merge(plants, est.sp_plants, by="Pixelnr", all=TRUE)
red_plants <- merge(red_plants, est.sp_red_plants, by="Pixelnr", all=TRUE)
black_plants <- merge(black_plants, est.sp_black_plants, by="Pixelnr", all=TRUE)
GBIF_other <- merge(GBIF_other, est.sp_other, by="Pixelnr", all=TRUE)
red_other <- merge(red_other, est.sp_red_other, by="Pixelnr", all=TRUE)
black_other <- merge(black_other, est.sp_black_other, by="Pixelnr", all=TRUE)}

# Also add the the number of estimated redlisted species to the TrdRast-data for plotting
# First, retrieve the needed data for each of the raster cells 
{birds_sp <- unique(birds@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(birds_sp) <- c("Pixelnr", "S.birds", "S.chao1_birds", "se.chao1_birds")
plants_sp <- unique(plants@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(plants_sp) <- c("Pixelnr", "S.obs_plants", "S.chao1_plants", "se.chao1_plants")
other_sp <- unique(GBIF_other@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(other_sp) <- c("Pixelnr", "S.obs_other", "S.chao1_other", "se.chao1_other")

red_birds_sp<- unique(red_birds@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(red_birds_sp) <- c("Pixelnr", "S.obs_red_birds", "S.chao1_red_birds", "se.chao1_red_birds")
red_plants_sp<- unique(red_plants@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(red_plants_sp) <- c("Pixelnr", "S.obs_red_plants", "S.chao1_red_plants", "se.chao1_red_plants")
red_other_sp<- unique(red_other@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(red_other_sp) <- c("Pixelnr", "S.obs_red_other", "S.chao1_red_other", "se.chao1_red_other")

black_birds_sp <- unique(black_birds@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(black_birds_sp) <- c("Pixelnr", "S.obs_black_birds", "S.chao1_black_birds", "se.chao1_black_birds")
black_plants_sp <- unique(black_plants@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(black_plants_sp) <- c("Pixelnr", "S.obs_black_plants", "S.chao1_black_plants", "se.chao1_black_plants")
black_other_sp <- unique(black_other@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
colnames(black_other_sp) <- c("Pixelnr", "S.obs_black_other", "S.chao1_black_other", "se.chao1_black_other")}

# Merge into the TrdRast_AR5
{TrdRast_AR5 <- merge(TrdRast_AR5, birds_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, plants_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, other_sp, by="Pixelnr", all=TRUE)

TrdRast_AR5 <- merge(TrdRast_AR5, red_birds_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, red_plants_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, red_other_sp, by="Pixelnr", all=TRUE)

TrdRast_AR5 <- merge(TrdRast_AR5, black_birds_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, black_plants_sp, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, black_other_sp, by="Pixelnr", all=TRUE)
summary(TrdRast_AR5)}

##--- 4. NUMBER OF SAMPLES/RECORDS IN EACH CELL ---####
##-------------------------------------------------####
# Add number of observations to the dataframes (these are picked up from the "Rasterization'-chunk,
# and has to be done for the sub-groups).

# Make a table with the number of observations per pixel
{  pointstable_birds <- table(birds@data$Pixelnr)
pointstable_plants <- table(plants@data$Pixelnr)
pointstable_other <- table(GBIF_other@data$Pixelnr)
pointstable_red_birds <- table(red_birds@data$Pixelnr)
pointstable_red_plants <- table(red_plants@data$Pixelnr)
pointstable_red_other <- table(red_other@data$Pixelnr)
pointstable_black_birds <- table(black_birds@data$Pixelnr)
pointstable_black_plants <- table(black_plants@data$Pixelnr)
pointstable_black_other <- table(black_other@data$Pixelnr)
}

# Add numbers to the dataframes
{birds@data <- merge(birds@data, as.data.frame(pointstable_birds), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(birds)[names(birds)=="Freq"] <- "Nbirds"
plants@data <- merge(plants@data, as.data.frame(pointstable_plants), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(plants)[names(plants)=="Freq"] <- "Nplants"
GBIF_other@data <- merge(GBIF_other@data, as.data.frame(pointstable_other), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(GBIF_other)[names(GBIF_other)=="Freq"] <- "Nother"
red_birds@data <- merge(red_birds@data, as.data.frame(pointstable_red_birds), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(red_birds)[names(red_birds)=="Freq"] <- "Nred_birds"
red_plants@data <- merge(red_plants@data, as.data.frame(pointstable_red_plants), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(red_plants)[names(red_plants)=="Freq"] <- "Nred_plants"
red_other@data <- merge(red_other@data, as.data.frame(pointstable_red_other), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(red_other)[names(red_other)=="Freq"] <- "Nred_other"
black_birds@data <- merge(black_birds@data, as.data.frame(pointstable_black_birds), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(black_birds)[names(black_birds)=="Freq"] <- "Nblack_birds"
black_plants@data <- merge(black_plants@data, as.data.frame(pointstable_black_plants), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(black_plants)[names(black_plants)=="Freq"] <- "Nblack_plants"
black_other@data <- merge(black_other@data, as.data.frame(pointstable_black_other), by.x="Pixelnr", by.y="Var1", all=TRUE)
names(black_other)[names(black_other)=="Freq"] <- "Nblack_other"}

# Also add these to the TrdRast_AR5 dataframe for plotting - first, retrieve the needed data, and remove
# cells not within Trondheim borders:
cells_in_trondheim <- TrdRast_AR5@data[!is.na(TrdRast_AR5@data$Communications_traffic), "Pixelnr"]

{{birds_sp2 <- unique(birds@data[,c("Pixelnr", "Nbirds")])
birds_sp2 <- birds_sp2 %>%
  filter(Pixelnr %in% cells_in_trondheim)}
{plants_sp2 <- unique(plants@data[,c("Pixelnr", "Nplants")])
  plants_sp2 <- plants_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{other_sp2 <- unique(GBIF_other@data[,c("Pixelnr", "Nother")])
  other_sp2 <- other_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{red_birds_sp2 <- unique(red_birds@data[,c("Pixelnr", "Nred_birds")])
  red_birds_sp2 <- red_birds_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{red_plants_sp2 <- unique(red_plants@data[,c("Pixelnr", "Nred_plants")])
  red_plants_sp2 <- red_plants_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{red_other_sp2 <- unique(red_other@data[,c("Pixelnr", "Nred_other")])
  red_other_sp2 <- red_other_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{black_birds_sp2 <- unique(black_birds@data[,c("Pixelnr", "Nblack_birds")])
  black_birds_sp2 <- black_birds_sp2 %>%
      filter(Pixelnr %in% cells_in_trondheim)}
{black_plants_sp2 <- unique(black_plants@data[,c("Pixelnr", "Nblack_plants")])
  black_plants_sp2 <- black_plants_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}
{black_other_sp2 <- unique(black_other@data[,c("Pixelnr", "Nblack_other")])
  black_other_sp2 <- black_other_sp2 %>%
    filter(Pixelnr %in% cells_in_trondheim)}}

# Merge both into the TrdRast_AR5
{TrdRast_AR5 <- merge(TrdRast_AR5, birds_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, plants_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, other_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, red_birds_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, red_plants_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, red_other_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, black_birds_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, black_plants_sp2, by="Pixelnr", all=TRUE)
TrdRast_AR5 <- merge(TrdRast_AR5, black_other_sp2, by="Pixelnr", all=TRUE)}

{rm(birds_sp)
rm(birds_sp2)
rm(plants_sp)
rm(plants_sp2)
rm(other_sp)
rm(other_sp2)
rm(red_birds_sp)
rm(red_birds_sp2)
rm(red_other_sp)
rm(red_other_sp2)
rm(red_plants_sp)
rm(red_plants_sp2)
rm(black_birds_sp)
rm(black_birds_sp2)
rm(black_plants_sp)
rm(black_plants_sp2)
rm(black_other_sp)
rm(black_other_sp2)}

##--- 5. "PRUNING" OF DATA ---####
##----------------------------####

# Have a look at how the number of samples in each grid cell is ditributed.
# Most cells have 0 observations in them - hence this bar is cut out of the plot:
par(mfrow=c(3,3))
par(mar=c(5.1,4.1,4.1,2.1))
{barplot(table(TrdRast_AR5@data$Nbirds), ylim=c(0,170), xlab="Records of birds", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nplants), ylim=c(0,170), xlab="Records of plants", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nother), ylim=c(0,170), xlab="Records of other taxa", ylab="Frequency")}
{barplot(table(TrdRast_AR5@data$Nred_birds), ylim=c(0,110), xlab="Records of threatened birds", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nred_plants), ylim=c(0,110), xlab="Records of threatened plants", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nred_other), ylim=c(0,110), xlab="Records of threatened other taxa", ylab="Frequency")}
{barplot(table(TrdRast_AR5@data$Nblack_birds), ylim=c(0,135), xlab="Records of alien birds", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nblack_plants), ylim=c(0,135), xlab="Records of alien plants", ylab="Frequency")
barplot(table(TrdRast_AR5@data$Nblack_other), ylim=c(0,135), xlab="Records of other alien taxa", ylab="Frequency")}

# One rule for pruning could be only using cells with a minimum number of records, e.g. 10, 15, 20 etc.
# Using all cells in the grid gives us respectively (842, 508, 635, 540, 49, 123, 169, 301, 52) cells
# (with data, not counting zeros).

# Another criterion for pruning is coefficient of variation (the SE of the estimated number of species/ESR)
# This should not be too large, e.g. less than 0.2 census Ballesteros-Meija et al. (2013).
# First, we add that calculation to the dataframe, then look at the results:
{TrdRast_AR5@data$CoV_birds <- TrdRast_AR5@data$se.chao1_birds/TrdRast_AR5@data$S.chao1_birds
TrdRast_AR5@data$CoV_plants <- TrdRast_AR5@data$se.chao1_plants/TrdRast_AR5@data$S.chao1_plants
TrdRast_AR5@data$CoV_other <- TrdRast_AR5@data$se.chao1_other/TrdRast_AR5@data$S.chao1_other
TrdRast_AR5@data$CoV_red_birds <- TrdRast_AR5@data$se.chao1_red_birds/TrdRast_AR5@data$S.chao1_red_birds
TrdRast_AR5@data$CoV_red_plants <- TrdRast_AR5@data$se.chao1_red_plants/TrdRast_AR5@data$S.chao1_red_plants
TrdRast_AR5@data$CoV_red_other <- TrdRast_AR5@data$se.chao1_red_other/TrdRast_AR5@data$S.chao1_red_other
TrdRast_AR5@data$CoV_black_birds <- TrdRast_AR5@data$se.chao1_black_birds/TrdRast_AR5@data$S.chao1_black_birds
TrdRast_AR5@data$CoV_black_plants <- TrdRast_AR5@data$se.chao1_black_plants/TrdRast_AR5@data$S.chao1_black_plants
TrdRast_AR5@data$CoV_black_other <- TrdRast_AR5@data$se.chao1_black_other/TrdRast_AR5@data$S.chao1_black_other}

par(mfrow=c(2,3))
{hist(TrdRast_AR5@data$CoV_birds)
hist(TrdRast_AR5@data$CoV_plants)
hist(TrdRast_AR5@data$CoV_other)
    hist(TrdRast_AR5@data$Nbirds)
    hist(TrdRast_AR5@data$Nplants)
    hist(TrdRast_AR5@data$Nother)
hist(TrdRast_AR5@data$CoV_red_birds)
hist(TrdRast_AR5@data$CoV_red_plants)
hist(TrdRast_AR5@data$CoV_red_other)
    hist(TrdRast_AR5@data$Nred_birds)
    hist(TrdRast_AR5@data$Nred_plants)
    hist(TrdRast_AR5@data$Nred_other)
hist(TrdRast_AR5@data$CoV_black_birds)
hist(TrdRast_AR5@data$CoV_black_plants)
hist(TrdRast_AR5@data$CoV_black_other)
    hist(TrdRast_AR5@data$Nblack_birds)
    hist(TrdRast_AR5@data$Nblack_plants)
    hist(TrdRast_AR5@data$Nblack_other)}

### In the further analyses, we will continue with some minorly "pruned" data - in this case,
# we'll go for cells with more than 10 records and a CoV <0.25.
# As in the previous, this is ONLY for the total number, NOT the threatened or alien subgroups
{Trd_birds_analyses <- TrdRast_AR5[TrdRast_AR5@data$CoV_birds<=0.25 &
                                  TrdRast_AR5@data$Nbirds>=10 &
                                  !is.na(TrdRast_AR5@data$Communications_traffic) &
                                  !is.na(TrdRast_AR5@data$CoV_birds), ]
Trd_plants_analyses <- TrdRast_AR5[TrdRast_AR5@data$CoV_plants<=0.25 &
                                    TrdRast_AR5@data$Nplants>=10 &
                                    !is.na(TrdRast_AR5@data$Communications_traffic) &
                                    !is.na(TrdRast_AR5@data$CoV_plants), ]
Trd_other_analyses <- TrdRast_AR5[TrdRast_AR5@data$CoV_other<=0.25 &
                                    TrdRast_AR5@data$Nother>=10 &
                                    !is.na(TrdRast_AR5@data$Communications_traffic) &
                                    !is.na(TrdRast_AR5@data$CoV_other), ]
}

# Number of cells:  316   40    39

# It does not help particularly much to lower neither of the criteria - potentially, we would have to not
# separate the plants from the "other"

## Have a look at a map - grid cells with enough data according the previously used criteria: ####
par(mfrow=c(2,2))
par(mar=c(0.5,0.5,5,0.5))
#AR5map(Trondheim, AR5_crop, "All birds, threatened and alien")
plot(Trondheim, main="Birds")
plot(TrdRast_AR5, border="gray", add=T)   # The entire grid
    plot(Trd_birds_analyses,
        col=rgb(0, 0, 204, alpha = 125, maxColorValue = 255), border=NA, add=T) 
    
#AR5map(Trondheim, AR5_crop, "All plants, threatened and alien")
plot(Trondheim, main="Plants")
plot(TrdRast_AR5, border="gray", add=T)   # The entire grid
    plot(Trd_plants_analyses,
         col=rgb(0, 0, 204, alpha = 125, maxColorValue = 255), border=NA, add=T) 
    
#AR5map(Trondheim, AR5_crop, "All other, threatened and alien")
plot(Trondheim, main="Other taxa")
plot(TrdRast_AR5, border="gray", add=T)   # The entire grid
    plot(Trd_other_analyses,
         col=rgb(0, 0, 204, alpha = 125, maxColorValue = 255), border=NA, add=T)  
    
##--- 6. COMBINE PLANTS AND OTHER ---####
##-----------------------------------####
# Make a smaller, regular dataframe to speed up the proces (only including scientific name and fPixelnr)
# Check that the column numbers are correct!
plantsother <- GBIF.trd_2013[!(GBIF.trd_2013$class=="Aves"),]
plantsother@data <- droplevels(plantsother@data)

rar.plantsother <- as.data.frame(plantsother@data[,c("species", "Pixelnr")])
rar.plantsother$Pixelnr <- as.factor(rar.plantsother$Pixelnr)
rar.plantsother <- droplevels(rar.plantsother)
    
# Create empty matrix
est_plantsother <- matrix(data=NA, ncol=nlevels(rar.plantsother$species), nrow=nlevels((rar.plantsother$Pixelnr)))
    
# Add column names and row names (species names and Pixelnr)
colnames(est_plantsother) <- levels(rar.plantsother$species)
rownames(est_plantsother) <- levels(rar.plantsother$Pixelnr)
    
# Make a tally of the rar.data
tallied_plantsother <- rar.plantsother %>%
  group_by(species, Pixelnr) %>%
  tally()   

##--- 6.1 RED- AND BLACKLISTED ---####
##----------------------------------####
# Red and blacklisted observations only:
red_plantsother <- reds_2013[!(reds_2013$class=="Aves"),]
black_plantsother <- blacks_2013[!(blacks_2013$class=="Aves"),]

red_plantsother@data <- droplevels(red_plantsother@data)
black_plantsother@data <- droplevels(black_plantsother@data)

# Make a smaller, regular dataframe to speed up the proces (only includng scientific name and fPixelnr)
# Check that the column numbers are correct!
rar.red_plantsother <- as.data.frame(red_plantsother@data[,c("species", "Pixelnr")])
rar.red_plantsother$Pixelnr <- as.factor(rar.red_plantsother$Pixelnr)
rar.red_plantsother <- droplevels(rar.red_plantsother)

rar.black_plantsother <- as.data.frame(black_plantsother@data[,c("species", "Pixelnr")])
rar.black_plantsother$Pixelnr <- as.factor(rar.black_plantsother$Pixelnr)
rar.black_plantsother <- droplevels(rar.black_plantsother)

# Create empty matrix
est.red_plantsother <- matrix(data=NA, ncol=nlevels(rar.red_plantsother$species), nrow=nlevels(rar.red_plantsother$Pixelnr))
est.black_plantsother <- matrix(data=NA, ncol=nlevels(rar.black_plantsother$species), nrow=nlevels(rar.black_plantsother$Pixelnr))

# Add column names and row names (species names and Pixelnr)
colnames(est.red_plantsother) <- levels(rar.red_plantsother$species)
rownames(est.red_plantsother) <- levels(rar.red_plantsother$Pixelnr)
colnames(est.black_plantsother) <- levels(rar.black_plantsother$species)
rownames(est.black_plantsother) <- levels(rar.black_plantsother$Pixelnr)

# Make a tally of the rar.data
tallied.red_plantsother <- rar.red_plantsother %>%
  group_by(species, Pixelnr) %>%
  tally()
tallied.black_plantsother <- rar.black_plantsother %>%
  group_by(species, Pixelnr) %>%
  tally()


##--- 6.2 RUN THE FUNCTION AND FILL IN THE COMMUNITY MATRIX  ---####
##--------------------------------------------------------------####
## ALL OBSERVATIONS:
# Other
est1_plantsother <- est_plantsother[,c(1:750)]      
est2_plantsother <- est_plantsother[,c(751:1500)]
est3_plantsother <- est_plantsother[,c(1501:2250)] 
est4_plantsother <- est_plantsother[,c(2251:2875)] 

# 1. 
for(r in 1:dim(est1_plantsother)[1]){
  for(c in 1:dim(est1_plantsother)[2]){
    est1_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied_plantsother, Com.matrix=est1_plantsother)}
}

# 2.
for(r in 1:dim(est2_plantsother)[1]){
  for(c in 1:dim(est2_plantsother)[2]){
    est2_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied_plantsother, Com.matrix=est2_plantsother)}
}

# 3.
for(r in 1:dim(est3_plantsother)[1]){
  for(c in 1:dim(est3_plantsother)[2]){
    est3_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied_plantsother, Com.matrix=est3_plantsother)}
}

# 4.
for(r in 1:dim(est4_plantsother)[1]){
  for(c in 1:dim(est4_plantsother)[2]){
    est4_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied_plantsother, Com.matrix=est4_plantsother)}
}



# Combine the small matrices to one large community matrix
est_plantsother <- cbind(est1_plantsother, est2_plantsother, est3_plantsother, est4_plantsother) 

write.csv(est_plantsother, file="Comm_matrix_plantsother.csv")


### REDLISTED
for(r in 1:dim(est.red_plantsother)[1]){
  for(c in 1:dim(est.red_plantsother)[2]){
    est.red_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied.red_plantsother, Com.matrix=est.red_plantsother)}
}

write.csv(est.red_plantsother, file="Comm_matrix_red_plantsother.csv")

### BLACKLISTED
for(r in 1:dim(est.black_plantsother)[1]){
  for(c in 1:dim(est.black_plantsother)[2]){
    est.black_plantsother[r,c]=ntally(i=r, j=c, Tally=tallied.black_plantsother, Com.matrix=est.black_plantsother)}
}

write.csv(est.black_plantsother, file="Comm_matrix_black_plantsother.csv")

est_plantsother <- read.csv("Comm_matrix_plantsother.csv", row.names = 1)
est.red_plantsother <- read.csv("Comm_matrix_red_plantsother.csv", row.names = 1)
est.black_plantsother <- read.csv("Comm_matrix_black_plantsother.csv", row.names = 1)

# Check that the community matrices seems reasonable:
dim(est.black_plantsother)
hist(rowSums(est.black_plantsother))
hist(log(rowSums(est.black_plantsother)))
View(as.data.frame(rowSums(est.red_plantsother)))





##--- 7. CALCULATE THE ESTIMATED SPECIES RICHNESS FOR EACH OF THE GRID CELLS ---####
##------------------------------------------------------------------------------####
# Estimate species richness for each site/grid cell, and save the results. For cells with only one
# observation, the returned value will  be NA. 
{est.sp_plantsother <- estimateR(est_plantsother)
  est.sp_red_plantsother <- estimateR(est.red_plantsother)
  est.sp_black_plantsother <- estimateR(est.black_plantsother)}

# Rotate it and make it a dataframe
{est.sp_plantsother <- as.data.frame(t(est.sp_plantsother))
  est.sp_plantsother$Pixelnr <- rownames(est.sp_plantsother)
  est.sp_red_plantsother <- as.data.frame(t(est.sp_red_plantsother))
  est.sp_red_plantsother$Pixelnr <- rownames(est.sp_red_plantsother)
  est.sp_black_plantsother <- as.data.frame(t(est.sp_black_plantsother))
  est.sp_black_plantsother$Pixelnr <- rownames(est.sp_black_plantsother)}

# Add these calculations (Species richness) to the SpatialPointsDataframes (later: also to the TrdRast SpatialPolygonsDataFrame)
{plantsother <- merge(plantsother, est.sp_plantsother, by="Pixelnr", all=TRUE)
  red_plantsother <- merge(red_plantsother, est.sp_plantsother, by="Pixelnr", all=TRUE)
  black_plantsother <- merge(black_plantsother, est.sp_plantsother, by="Pixelnr", all=TRUE)}

# Also add the the number of estimated redlisted species to the TrdRast-data for plotting
# First, retrieve the needed data for each of the raster cells 
{plantsother_sp <- unique(plantsother@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
  colnames(plantsother_sp) <- c("Pixelnr", "S.plantsother", "S.chao1_plantsother", "se.chao1_plantsother")
  red_plantsother_sp<- unique(red_plantsother@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
  colnames(red_plantsother_sp) <- c("Pixelnr", "S.obs_red_plantsother", "S.chao1_red_plantsother", "se.chao1_red_plantsother")
  black_plantsother_sp <- unique(black_plantsother@data[,c("Pixelnr", "S.obs", "S.chao1", "se.chao1")])
  colnames(black_plantsother_sp) <- c("Pixelnr", "S.obs_black_plantsother", "S.chao1_black_plantsother", "se.chao1_black_plantsother")}

# Merge into the TrdRast_AR5
{TrdRast_AR5 <- merge(TrdRast_AR5, plantsother_sp, by="Pixelnr", all=TRUE)
  TrdRast_AR5 <- merge(TrdRast_AR5, red_plantsother_sp, by="Pixelnr", all=TRUE)
  TrdRast_AR5 <- merge(TrdRast_AR5, black_plantsother_sp, by="Pixelnr", all=TRUE)
  summary(TrdRast_AR5)}

##--- 8. NUMBER OF SAMPLES/RECORDS IN EACH CELL ---####
##-------------------------------------------------####
# Add number of observations to the dataframes (these are picked up from the "Rasterization'-chunk,
# and has to be done for the sub-groups).

# Make a table with the number of observations per pixel
{  pointstable_plantsother <- table(plantsother@data$Pixelnr)
pointstable_red_plantsother <- table(red_plantsother@data$Pixelnr)
pointstable_black_plantsother <- table(black_plantsother@data$Pixelnr)
}

# Add numbers to the dataframes
{plantsother@data <- merge(plantsother@data, as.data.frame(pointstable_plantsother), by.x="Pixelnr", by.y="Var1", all=TRUE)
  names(plantsother)[names(plantsother)=="Freq"] <- "Nplantsother"
  red_plantsother@data <- merge(red_plantsother@data, as.data.frame(pointstable_red_plantsother), by.x="Pixelnr", by.y="Var1", all=TRUE)
  names(red_plantsother)[names(red_plantsother)=="Freq"] <- "Nred_plantsother"
  black_plantsother@data <- merge(black_plantsother@data, as.data.frame(pointstable_black_plantsother), by.x="Pixelnr", by.y="Var1", all=TRUE)
  names(black_plantsother)[names(black_plantsother)=="Freq"] <- "Nblack_plantsother"
  }

# Also add these to the TrdRast_AR5 dataframe for plotting - first, retrieve the needed data, and remove
# cells not within Trondheim borders:
{{plantsother_sp2 <- unique(plantsother@data[,c("Pixelnr", "Nplantsother")])
plantsother_sp2 <- plantsother_sp2 %>%
  filter(Pixelnr %in% cells_in_trondheim)}
  {red_plantsother_sp2 <- unique(red_plantsother@data[,c("Pixelnr", "Nred_plantsother")])
    red_plantsother_sp2 <- red_plantsother_sp2 %>%
      filter(Pixelnr %in% cells_in_trondheim)}
  {black_plantsother_sp2 <- unique(black_plantsother@data[,c("Pixelnr", "Nblack_plantsother")])
    black_plantsother_sp2 <- black_plantsother_sp2 %>%
      filter(Pixelnr %in% cells_in_trondheim)}}

# Merge both into the TrdRast_AR5
{TrdRast_AR5 <- merge(TrdRast_AR5, plantsother_sp2, by="Pixelnr", all=TRUE)
  TrdRast_AR5 <- merge(TrdRast_AR5, red_plantsother_sp2, by="Pixelnr", all=TRUE)
  TrdRast_AR5 <- merge(TrdRast_AR5, black_plantsother_sp2, by="Pixelnr", all=TRUE)}

{rm(plantsother_sp)
  rm(plantsother_sp2)
  rm(red_plantsother_sp)
  rm(red_plantsother_sp2)
  rm(black_plantsother_sp)
  rm(black_plantsother_sp2)}

##--- 9. "PRUNING" OF DATA ---####
##----------------------------####

# Have a look at how the number of samples in each grid cell is ditributed.
# Most cells have 0 observations in them - hence this bar is cut out of the plot:
par(mfrow=c(1,3))
par(mar=c(5.1,4.1,4.1,2.1))
{barplot(table(TrdRast_AR5@data$Nplantsother), ylim=c(0,175), xlab="Records of plants/other", ylab="Frequency")
  barplot(table(TrdRast_AR5@data$Nred_plantsother), ylim=c(0,100), xlab="Records of threatened plants/other", ylab="Frequency")
  barplot(table(TrdRast_AR5@data$Nblack_plantsother), ylim=c(0,150), xlab="Records of alien plants/other", ylab="Frequency")}

# One rule for pruning could be only using cells with a minimum number of records, e.g. 10, 15, 20 etc.
# Using all cells in the grid gives us respectively (842, 508, 635, 540, 49, 123, 169, 301, 52) cells
# (with data, not counting zeros).
# Using the same criteria as for the total biodiv. gives ()

# Another criterion for pruning is coefficient of variation (the SE of the estimated number of species/ESR)
# This should not be too large, e.g. less than 0.2 census Ballesteros-Meija et al. (2013).
# First, we add that calculation to the dataframe, then look at the results:
{TrdRast_AR5@data$CoV_plantsother <- TrdRast_AR5@data$se.chao1_plantsother/TrdRast_AR5@data$S.chao1_plantsother
  TrdRast_AR5@data$CoV_red_plantsother <- TrdRast_AR5@data$se.chao1_red_plantsother/TrdRast_AR5@data$S.chao1_red_plantsother
  TrdRast_AR5@data$CoV_black_plantsother <- TrdRast_AR5@data$se.chao1_black_plantsother/TrdRast_AR5@data$S.chao1_black_plantsother}

par(mfrow=c(3,2))
{hist(TrdRast_AR5@data$CoV_plantsother)
  hist(TrdRast_AR5@data$Nplantsother)
  hist(TrdRast_AR5@data$CoV_red_plantsother)
  hist(TrdRast_AR5@data$Nred_plantsother)
  hist(TrdRast_AR5@data$CoV_black_plantsother)
  hist(TrdRast_AR5@data$Nblack_plantsother)}

### In the further analyses, we will continue with some minorly "pruned" data - in this case,
# we'll go for cells with more than 10 records and a CoV <0.25.
{Trd_plantsother_analyses <- TrdRast_AR5[TrdRast_AR5@data$CoV_plantsother<=0.25 &
                                     TrdRast_AR5@data$Nplantsother>=10 &
                                     !is.na(TrdRast_AR5@data$Communications_traffic) &
                                     !is.na(TrdRast_AR5@data$CoV_plantsother), ]}

# Number of cells:  316   40    39     72 
# It does not help particularly much to lower neither of the criteria - potentially, we would have to not
# separate the plants from the "other"

## Have a look at a map: ####
#AR5map(Trondheim, AR5_crop, "All plants and other, threatened and alien")
plot(Trondheim, main="Plants and other taxa")
plot(TrdRast_AR5, border="gray", add=T)   # The entire grid
plot(Trd_plantsother_analyses,
     col=rgb(0, 0, 204, alpha = 125, maxColorValue = 255), border=NA, add=T) 

# Seemingly, we're not going to be able to make any kinds of reliable projections for any sub-group,
# (except for birds) if we divide the data. However, let's give it a try anyways (I'll skip some of the
# SAC-testing steps and the data exploration in this one)


##--- 10. SPATIAL MODELLING ---####
##-----------------------------####
TrdRast_clust@data <- merge(TrdRast_clust@data, TrdRast_AR5@data[, c(1,83:142)], by="Pixelnr", all.x=TRUE)
# Transformation of the response variables (here we have to add a constant to make the calculations, as log(0) is meaningsless)
# I here use 1, as this gives the closest approximation of a Gaussian distribution (smaller make the data even more skewed):
{ TrdRast_clust$log_chao.red_birds <- NA
  TrdRast_clust$log_chao.black_birds <- NA
  TrdRast_clust$log_chao.birds <- NA
  TrdRast_clust$log_chao.red_plants <- NA
  TrdRast_clust$log_chao.black_plants <- NA
  TrdRast_clust$log_chao.plants <- NA
  TrdRast_clust$log_chao.red_other <- NA
  TrdRast_clust$log_chao.black_other <- NA
  TrdRast_clust$log_chao.other <- NA
  TrdRast_clust$log_chao.red_plantsother <- NA
  TrdRast_clust$log_chao.black_plantsother <- NA
  TrdRast_clust$log_chao.plantsother <- NA
  for(i in 1:NROW(TrdRast_clust@data)){
    TrdRast_clust@data[i,"log_chao.red_birds"] <- log(TrdRast_clust@data[i,"S.chao1_red_birds"] + 1)
    TrdRast_clust@data[i,"log_chao.black_birds"] <- log(TrdRast_clust@data[i,"S.chao1_black_birds"] + 1)
    TrdRast_clust@data[i,"log_chao.birds"] <- log(TrdRast_clust@data[i,"S.chao1_birds"] + 1)
    TrdRast_clust@data[i,"log_chao.red_plants"] <- log(TrdRast_clust@data[i,"S.chao1_red_plants"] + 1)
    TrdRast_clust@data[i,"log_chao.black_plants"] <- log(TrdRast_clust@data[i,"S.chao1_black_plants"] + 1)
    TrdRast_clust@data[i,"log_chao.plants"] <- log(TrdRast_clust@data[i,"S.chao1_plants"] + 1)
    TrdRast_clust@data[i,"log_chao.red_other"] <- log(TrdRast_clust@data[i,"S.chao1_red_other"] + 1)
    TrdRast_clust@data[i,"log_chao.black_other"] <- log(TrdRast_clust@data[i,"S.chao1_black_other"] + 1)
    TrdRast_clust@data[i,"log_chao.other"] <- log(TrdRast_clust@data[i,"S.chao1_other"] + 1)
    TrdRast_clust@data[i,"log_chao.red_plantsother"] <- log(TrdRast_clust@data[i,"S.chao1_red_plantsother"] + 1)
    TrdRast_clust@data[i,"log_chao.black_plantsother"] <- log(TrdRast_clust@data[i,"S.chao1_black_plantsother"] + 1)
    TrdRast_clust@data[i,"log_chao.plantsother"] <- log(TrdRast_clust@data[i,"S.chao1_plantsother"] + 1)
}
  
  # Replace 'NA's with zeros - OBS! Important to do ONLY for the response variables!
  TrdRast_clust@data$log_chao.black_birds[is.na(TrdRast_clust@data$log_chao.black_birds)] <- 0
  TrdRast_clust@data$log_chao.red_birds[is.na(TrdRast_clust@data$log_chao.red_birds)] <- 0
  TrdRast_clust@data$log_chao.birds[is.na(TrdRast_clust@data$log_chao.birds)] <- 0
  TrdRast_clust@data$log_chao.black_plants[is.na(TrdRast_clust@data$log_chao.black_plants)] <- 0
  TrdRast_clust@data$log_chao.red_plants[is.na(TrdRast_clust@data$log_chao.red_plants)] <- 0
  TrdRast_clust@data$log_chao.plants[is.na(TrdRast_clust@data$log_chao.plants)] <- 0
  TrdRast_clust@data$log_chao.black_other[is.na(TrdRast_clust@data$log_chao.black_other)] <- 0
  TrdRast_clust@data$log_chao.red_other[is.na(TrdRast_clust@data$log_chao.red_other)] <- 0
  TrdRast_clust@data$log_chao.other[is.na(TrdRast_clust@data$log_chao.other)] <- 0
  TrdRast_clust@data$log_chao.black_plantsother[is.na(TrdRast_clust@data$log_chao.black_plantsother)] <- 0
  TrdRast_clust@data$log_chao.red_plantsother[is.na(TrdRast_clust@data$log_chao.red_plantsother)] <- 0
  TrdRast_clust@data$log_chao.plantsother[is.na(TrdRast_clust@data$log_chao.plantsother)] <- 0
}


TrdRast_clust_model_birds <- TrdRast_clust[TrdRast_clust@data$CoV_birds<=0.25 &             # Only cells with low CoV
                                       TrdRast_clust$Nbirds>=10 &               # Only cells with >10 records
                                       !is.na(TrdRast_clust$CoV_birds), -c(2:68)]        # Only cells with a valid CoV
TrdRast_clust_model_plants <- TrdRast_clust[TrdRast_clust@data$CoV_plants<=0.25 &             # Only cells with low CoV
                                             TrdRast_clust$Nplants>=10 &               # Only cells with >10 records
                                             !is.na(TrdRast_clust$CoV_plants),-c(2:68)]        # Only cells with a valid CoV
TrdRast_clust_model_other <- TrdRast_clust[TrdRast_clust@data$CoV_other<=0.25 &             # Only cells with low CoV
                                             TrdRast_clust$Nother>=10 &               # Only cells with >10 records
                                             !is.na(TrdRast_clust$CoV_other),-c(2:68)]        # Only cells with a valid CoV
TrdRast_clust_model_plantsother <- TrdRast_clust[TrdRast_clust@data$CoV_plantsother<=0.25 &             # Only cells with low CoV
                                             TrdRast_clust$Nplantsother>=10 &               # Only cells with >10 records
                                             !is.na(TrdRast_clust$CoV_plantsother),-c(2:68)]        # Only cells with a valid CoV

# Make sure that the factor levels are correct:
TrdRast_clust_model_birds@data$clusterCut <- as.factor(TrdRast_clust_model_birds@data$clusterCut)
TrdRast_clust_model_plants@data$clusterCut <- as.factor(TrdRast_clust_model_plants@data$clusterCut)
TrdRast_clust_model_other@data$clusterCut <- as.factor(TrdRast_clust_model_other@data$clusterCut)
TrdRast_clust_model_plantsother@data$clusterCut <- as.factor(TrdRast_clust_model_plantsother@data$clusterCut)

#Remove cells with NaN-values in north.mean
TrdRast_clust_model_birds <- TrdRast_clust_model_birds[!is.na(TrdRast_clust_model_birds@data$north.mean),]
TrdRast_clust_model_plants <- TrdRast_clust_model_plants[!is.na(TrdRast_clust_model_plants@data$north.mean),]
TrdRast_clust_model_other <- TrdRast_clust_model_other[!is.na(TrdRast_clust_model_other@data$north.mean),]
TrdRast_clust_model_plantsother <- TrdRast_clust_model_plantsother[!is.na(TrdRast_clust_model_plantsother@data$north.mean),]

##--- 10.1 DEALING WITH SAC ---####
##--- 10.1.1 Chao1_reds     ---####
##-----------------------------####
xy_birds <- coordinates(TrdRast_clust_model_birds) 
xy_plants <- coordinates(TrdRast_clust_model_plants) 
xy_other <- coordinates(TrdRast_clust_model_other) 
xy_plantsother <- coordinates(TrdRast_clust_model_plantsother) 

## Birds ####
summary(gls_ML_birds <- gls(log_chao.red_birds ~  clusterCut + Divers + north.mean,
                                data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                                method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_birds <- dredge(gls_ML_birds)
d.red2_birds <- subset(d.red_birds, delta<2)
model.sel(d.red2_birds)
output_red_birds <- model.sel(d.red2_birds)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_red_birds <- model.avg(output_red_birds, fit=TRUE) 
summary(gls_avg_red_birds)$coefmat.full

## Plants ####
summary(gls_ML_plants <- gls(log_chao.red_plants ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_plants@data,
                            correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_plants <- dredge(gls_ML_plants)
d.red2_plants <- subset(d.red_plants, delta<2)
model.sel(d.red2_plants)
output_red_plants <- model.sel(d.red2_plants)

# Here we only have 1 candidate model - no averaging. Even more strange, there simply is no model!
#gls_avg_red_plants <- model.avg(output_red_plants, fit=TRUE) 
#summary(gls_avg_red_plants)$coefmat.full

## Other ####
summary(gls_ML_other <- gls(log_chao.red_other ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_other@data,
                             correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                             method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_other <- dredge(gls_ML_other)
d.red2_other <- subset(d.red_other, delta<2)
model.sel(d.red2_other)
output_red_other <- model.sel(d.red2_other)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_red_other <- model.avg(output_red_other, fit=TRUE) 
summary(gls_avg_red_other)$coefmat.full

## Plants and other ####
summary(gls_ML_plantsother <- gls(log_chao.red_plantsother ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_plantsother@data,
                            correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_plantsother <- dredge(gls_ML_plantsother)
d.red2_plantsother <- subset(d.red_plantsother, delta<2)
model.sel(d.red2_plantsother)
output_red_plantsother <- model.sel(d.red2_plantsother)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_red_plantsother <- model.avg(output_red_plantsother, fit=TRUE) 
summary(gls_avg_red_plantsother)$coefmat.full


##--- 10.1.2 Chao1_blacks    ---####
##------------------------------####
## Birds ####
summary(gls_ML_birds <- gls(log_chao.black_birds ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_birds@data,
                            correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_birds <- dredge(gls_ML_birds)
d.black2_birds <- subset(d.black_birds, delta<2)
model.sel(d.black2_birds)
output_black_birds <- model.sel(d.black2_birds)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_black_birds <- model.avg(output_black_birds, fit=TRUE) 
summary(gls_avg_black_birds)$coefmat.full

## Plants ####
summary(gls_ML_plants <- gls(log_chao.black_plants ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_plants@data,
                             correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                             method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_plants <- dredge(gls_ML_plants)
d.black2_plants <- subset(d.black_plants, delta<2)
model.sel(d.black2_plants)
output_black_plants <- model.sel(d.black2_plants)

# No model can predict this!
gls_avg_black_plants <- model.avg(output_black_plants, fit=TRUE) 
summary(gls_avg_black_plants)$coefmat.full


## Other ####
summary(gls_ML_other <- gls(log_chao.black_other ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_other@data,
                            correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_other <- dredge(gls_ML_other)
d.black2_other <- subset(d.black_other, delta<2)
model.sel(d.black2_other)
output_black_other <- model.sel(d.black2_other)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_black_other <- model.avg(output_black_other, fit=TRUE) 
summary(gls_avg_black_other)$coefmat.full

## Plants and other ####
summary(gls_ML_plantsother <- gls(log_chao.black_plantsother ~  clusterCut + Divers + north.mean,
                                  data = TrdRast_clust_model_plantsother@data,
                                  correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                                  method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_plantsother <- dredge(gls_ML_plantsother)
d.black2_plantsother <- subset(d.black_plantsother, delta<2)
model.sel(d.black2_plantsother)
output_black_plantsother <- model.sel(d.black2_plantsother)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_black_plantsother <- model.avg(output_black_plantsother, fit=TRUE) 
summary(gls_avg_black_plantsother)$coefmat.full



##--- 10.1.3 Chao1_all    ---####
##---------------------------####
## Birds ####
summary(gls_ML_birds <- gls(log_chao.birds ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_birds@data,
                            correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_birds <- dredge(gls_ML_birds)
d2_birds <- subset(d_birds, delta<2)
model.sel(d2_birds)
output_birds <- model.sel(d2_birds)

# Here we have only one candidate model! Define it:
gls_avg_birds <- gls(log_chao.birds ~  clusterCut + Divers,
                     data = TrdRast_clust_model_birds@data,
                     correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                     method = "ML") 
summary(gls_avg_birds)

## Plants ####
summary(gls_ML_plants <- gls(log_chao.plants ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_plants@data,
                             correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                             method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_plants <- dredge(gls_ML_plants)
d2_plants <- subset(d_plants, delta<2)
model.sel(d2_plants)
output_plants <- model.sel(d2_plants)

# No model can predict this!
#gls_avg_plants <- model.avg(output_plants, fit=TRUE) 
#summary(gls_avg_plants)$coefmat.full

## Other ####
summary(gls_ML_other <- gls(log_chao.other ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_other@data,
                            correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_other <- dredge(gls_ML_other)
d2_other <- subset(d_other, delta<2)
model.sel(d2_other)
output_other <- model.sel(d2_other)

# Here we have more than one model:
gls_avg_other <- model.avg(output_other, fit=TRUE) 
summary(gls_avg_other)$coefmat.full

## Plants and other ####
summary(gls_ML_plantsother <- gls(log_chao.plantsother ~  clusterCut + Divers + north.mean,
                                  data = TrdRast_clust_model_plantsother@data,
                                  correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                                  method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_plantsother <- dredge(gls_ML_plantsother)
d2_plantsother <- subset(d_plantsother, delta<2)
model.sel(d2_plantsother)
output_plantsother <- model.sel(d2_plantsother)

# Here we have more than 1 candidate model - we thus need model averaging!
gls_avg_plantsother <- model.avg(output_plantsother, fit=TRUE) 
summary(gls_avg_plantsother)$coefmat.full


##--- 10.2 Model validation ---####
##-----------------------------####
# As previously, standard model validation is not really feasible for model averages. We might have to move
# straight to making predictions, and then compare those
# Check the AIC of the different models. We cannot get this from the model averages, instead we can compare the 
# AICc's of the compononent models:
    # All
output_all$AICc               # 740.9-741.4  - No cluster10
  output_birds$AICc           # 586.57       - All clusters
  output_plants$AICc          # 135.41       - Null model, no cluster 3, 7, 10 or 12 
  output_other$AICc           # 112.78-114.75   - cluster not included
  output_plantsother$AICc     # 215.4-217.2  - cluster not included

    # Red
output_red$AICc               # 692.6        - Single model 
  output_red_birds$AICc       # 996.56-997.02 
  output_red_plants$AICc      # Null model
  output_red_other$AICc       # 48.33-48.41    - cluster not included
  output_red_plantsother$AICc # 281.7-283.5  - cluster not included

    # Black
output_black$AICc             # 743-743.3   - 
  output_black_birds$AICc     # 1287.80-1288.90  - No cluster 
  output_black_plants$AICc    # 117.96        - Null model
  output_black_other$AICc     # 41.5-43.5    - cluster not included
  output_black_plantsother$AICc # 282.7-283.7 - cluster not included

# As the models do not uae the same data, it does not really say anything
  
##--- 11. Making predictions from models  ---####
##-------------------------------------------####

# Now we want to try and make predictions on the number species based on the spatial models.
data_predict_tax <- TrdRast_clust[, c("Pixelnr", "clusterCut", "Divers", "north.mean")]
  
# Remove the grid cells with categories which cannot be used i the model (0)
data_predict_tax <- data_predict_tax[!data_predict_tax@data$clusterCut==0,]
data_predict_tax <- data_predict_tax[!is.na(data_predict_tax@data$north.mean),]
data_predict_tax@data$clusterCut <- as.factor(data_predict_tax@data$clusterCut)

### Make the predictions for threatened, alien and all species with the averaged models:
{data_predict_tax$predict_birds <- predict(gls_avg_birds, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_red_birds <- predict(gls_avg_red_birds, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_black_birds <- predict(gls_avg_black_birds, newdata=data_predict_tax, full=TRUE)                     
    #data_predict_tax$predict_plants <- predict(gls_avg_plants, newdata=data_predict_tax, full=TRUE)    # Null model                 
    #data_predict_tax$predict_red_plants <- predict(gls_avg_red_plants, newdata=data_predict_tax, full=TRUE)                     
    #data_predict_tax$predict_black_plants <- predict(gls_avg_black_plants, newdata=data_predict_tax, full=TRUE)    # null model                 
data_predict_tax$predict_other <- predict(gls_avg_other, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_red_other <- predict(gls_avg_red_other, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_black_other <- predict(gls_avg_black_other, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_plantsother <- predict(gls_avg_plantsother, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_red_plantsother <- predict(gls_avg_red_plantsother, newdata=data_predict_tax, full=TRUE)                     
data_predict_tax$predict_black_plantsother <- predict(gls_avg_black_plantsother, newdata=data_predict_tax, full=TRUE)}                     

# Back-transform the predictions:
{data_predict_tax$predict_no_birds <- (exp(data_predict_tax@data$predict_birds))-1
data_predict_tax$predict_no_red_birds <- (exp(data_predict_tax@data$predict_red_birds))-1
data_predict_tax$predict_no_black_birds <- (exp(data_predict_tax@data$predict_black_birds))-1
data_predict_tax$predict_no_other <- (exp(data_predict_tax@data$predict_other))-1
data_predict_tax$predict_no_red_other <- (exp(data_predict_tax@data$predict_red_other))-1
data_predict_tax$predict_no_black_other <- (exp(data_predict_tax@data$predict_black_other))-1
data_predict_tax$predict_no_plantsother <- (exp(data_predict_tax@data$predict_plantsother))-1
data_predict_tax$predict_no_red_plantsother <- (exp(data_predict_tax@data$predict_red_plantsother))-1
data_predict_tax$predict_no_black_plantsother <- (exp(data_predict_tax@data$predict_black_plantsother))-1}

range(data_predict_tax$predict_no_birds)
range(data_predict_tax$predict_no_red_birds)
range(data_predict_tax$predict_no_black_birds)
range(data_predict_tax$predict_no_other)
range(data_predict_tax$predict_no_red_other)
range(data_predict_tax$predict_no_black_other)
range(data_predict_tax$predict_no_plantsother)
range(data_predict_tax$predict_no_red_plantsother)
range(data_predict_tax$predict_no_black_plantsother)

##--- 11.1  MAPS ---####
##------------------####
library(RColorBrewer)

# Birds
layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1))
{# All
palette(col.all(173 +1))  
par(mar=c(0.01,0.01,2,0.01))
DivMap_3(AR5, Trondheim, "ESR birds")   
plot(TrdRast_clust_model_birds[, "S.chao1_birds"],
     col=TrdRast_clust_model_birds@data$S.chao1_birds,
     border=TrdRast_clust_model_birds@data$S.chao1_birds,
     add=T, cex.main=0.75)
par(mar=c(2,0.01,4,3))
image(y=0:max(TrdRast_clust_model_birds@data$S.chao1_birds),
z=t(0:max(TrdRast_clust_model_birds@data$S.chao1_birds)),
col=palette(col.all(173 +1)), axes=FALSE)
mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
axis(4,cex.axis=1,mgp=c(0,1,0),
     las=2)

palette(col.all(88))   
par(mar=c(0.01,0.01,2,0.01))
DivMap_3(AR5, Trondheim, "Pred. birds")   
plot(data_predict_tax,
     col= data_predict_tax@data$predict_no_birds,
     border=data_predict_tax@data$predict_no_birds,
     add=T, cex.main=0.75)
par(mar=c(2,0.01,4,3))
image(y=(0:(max(data_predict_tax@data$predict_no_birds))),
      z=t(0:(max(data_predict_tax@data$predict_no_birds))),
      col=palette(col.all(5)), axes=FALSE)
mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Threatened
  palette(col.reds(173 +1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR threatened birds")   
  plot(TrdRast_clust_model_birds[, "S.chao1_red_birds"],
       col=TrdRast_clust_model_birds@data$S.chao1_red_birds,
       border=TrdRast_clust_model_birds@data$S.chao1_red_birds,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_birds@data$S.chao1_red_birds[!is.na(TrdRast_clust_model_birds@data$S.chao1_red_birds)]),
        z=t(0:max(TrdRast_clust_model_birds@data$S.chao1_red_birds[!is.na(TrdRast_clust_model_birds@data$S.chao1_red_birds)])),
        col=palette(col.reds(173 +1)), axes=FALSE)
  mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.reds(123))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. threatened birds")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_red_birds,
       border=data_predict_tax@data$predict_no_red_birds,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_red_birds))),
        z=t(0:(max(data_predict_tax@data$predict_no_red_birds))),
        col=palette(col.reds(123)), axes=FALSE)
  mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Alien birds
  palette(col.blacks(173 +1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR alien birds")   
  plot(TrdRast_clust_model_birds[, "S.chao1_black_birds"],
       col=TrdRast_clust_model_birds@data$S.chao1_black_birds,
       border=TrdRast_clust_model_birds@data$S.chao1_black_birds,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_birds@data$S.chao1_black_birds[!is.na(TrdRast_clust_model_birds@data$S.chao1_black_birds)]),
        z=t(0:max(TrdRast_clust_model_birds@data$S.chao1_black_birds[!is.na(TrdRast_clust_model_birds@data$S.chao1_black_birds)])),
        col=palette(col.blacks(173 +1)), axes=FALSE)
  mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.blacks(5))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. alien birds")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_black_birds,
       border=data_predict_tax@data$predict_no_black_birds,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_black_birds))),
        z=t(0:(max(data_predict_tax@data$predict_no_black_birds))),
        col=palette(col.blacks(5)), axes=FALSE)
  mtext("Birds #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}

# Plants
# No models

# Other
layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1))
{# All
  palette(col.all(4560/5 +1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR other")   
  plot(TrdRast_clust_model_other[, "S.chao1_other"],
       col=TrdRast_clust_model_other@data$S.chao1_other,
       border=TrdRast_clust_model_other@data$S.chao1_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_other@data$S.chao1_other),
        z=t(0:max(TrdRast_clust_model_other@data$S.chao1_other)),
        col=palette(col.all(4560/5 +1)), axes=FALSE)
  mtext("other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.all(323))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_other,
       border=data_predict_tax@data$predict_no_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_other))),
        z=t(0:(max(data_predict_tax@data$predict_no_other))),
        col=palette(col.all(323)), axes=FALSE)
  mtext("other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Threatened
  palette(col.reds(5+1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR threatened other")   
  plot(TrdRast_clust_model_other[, "S.chao1_red_other"],
       col=TrdRast_clust_model_other@data$S.chao1_red_other,
       border=TrdRast_clust_model_other@data$S.chao1_red_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_other@data$S.chao1_red_other[!is.na(TrdRast_clust_model_other@data$S.chao1_red_other)]),
        z=t(0:max(TrdRast_clust_model_other@data$S.chao1_red_other[!is.na(TrdRast_clust_model_other@data$S.chao1_red_other)])),
        col=palette(col.reds(5 +1)), axes=FALSE)
  mtext("other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.reds(1*10))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. threatened other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_red_other,
       border=data_predict_tax@data$predict_no_red_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_red_other))),
        z=t(0:(max(data_predict_tax@data$predict_no_red_other))),
        col=palette(col.reds(1*10)), axes=FALSE)
  mtext("other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Alien other
  palette(col.blacks(3 +1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR alien other")   
  plot(TrdRast_clust_model_other[, "S.chao1_black_other"],
       col=TrdRast_clust_model_other@data$S.chao1_black_other,
       border=TrdRast_clust_model_other@data$S.chao1_black_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_other@data$S.chao1_black_other[!is.na(TrdRast_clust_model_other@data$S.chao1_black_other)]),
        z=t(0:max(TrdRast_clust_model_other@data$S.chao1_black_other[!is.na(TrdRast_clust_model_other@data$S.chao1_black_other)])),
        col=palette(col.blacks(3 +1)), axes=FALSE)
  mtext("other #", side=3, cex=1, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.blacks(1*10 +1))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. alien other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_black_other,
       border=data_predict_tax@data$predict_no_black_other,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_black_other))),
        z=t(0:(max(data_predict_tax@data$predict_no_black_other))),
        col=palette(col.blacks(1*10 +1)), axes=FALSE)
  mtext("other #", side=3, cex=1, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}

# Plants/other
layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)), widths=c(5,1,5,1))
{# All
  palette(col.all(5356/6+1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR Plants/other")   
  plot(TrdRast_clust_model_plantsother[, "S.chao1_plantsother"],
       col=TrdRast_clust_model_plantsother@data$S.chao1_plantsother,
       border=TrdRast_clust_model_plantsother@data$S.chao1_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_plantsother@data$S.chao1_plantsother),
        z=t(0:max(TrdRast_clust_model_plantsother@data$S.chao1_plantsother)),
        col=palette(col.all(5356/6 +1)), axes=FALSE)
  mtext("Plants/other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.all(131))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. Plants/other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_plantsother,
       border=data_predict_tax@data$predict_no_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_plantsother))),
        z=t(0:(max(data_predict_tax@data$predict_no_plantsother))),
        col=palette(col.all(131)), axes=FALSE)
  mtext("Plants/other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Threatened
  palette(col.reds(664+1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR threatened Plants/other")   
  plot(TrdRast_clust_model_plantsother[, "S.chao1_red_plantsother"],
       col=TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother,
       border=TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother[!is.na(TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother)]),
        z=t(0:max(TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother[!is.na(TrdRast_clust_model_plantsother@data$S.chao1_red_plantsother)])),
        col=palette(col.reds(664 +1)), axes=FALSE)
  mtext("Plants/other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.reds(17))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. threatened Plants/other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_red_plantsother,
       border=data_predict_tax@data$predict_no_red_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_red_plantsother))),
        z=t(0:(max(data_predict_tax@data$predict_no_red_plantsother))),
        col=palette(col.reds(17)), axes=FALSE)
  mtext("Plants/other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
{# Alien plantsother
  palette(col.blacks(5356/6 +1))  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "ESR alien Plants/other")   
  plot(TrdRast_clust_model_plantsother[, "S.chao1_black_plantsother"],
       col=TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother,
       border=TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=0:max(TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother[!is.na(TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother)]),
        z=t(0:max(TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother[!is.na(TrdRast_clust_model_plantsother@data$S.chao1_black_plantsother)])),
        col=palette(col.blacks(5356/6 +1)), axes=FALSE)
  mtext("plantsother #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,1,0),
       las=2)
  
  palette(col.blacks(41))  # max(data_predict_clust@data$predict_all_number.avg)  
  par(mar=c(0.01,0.01,2,0.01))
  DivMap_3(AR5, Trondheim, "Pred. alien Plants/other")   
  plot(data_predict_tax,
       col= data_predict_tax@data$predict_no_black_plantsother,
       border=data_predict_tax@data$predict_no_black_plantsother,
       add=T, cex.main=0.75)
  par(mar=c(2,0.01,4,3))
  image(y=(0:(max(data_predict_tax@data$predict_no_black_plantsother))),
        z=t(0:(max(data_predict_tax@data$predict_no_black_plantsother))),
        col=palette(col.blacks(41)), axes=FALSE)
  mtext("Plants/other #", side=3, cex=0.5, font=2, line = 1)
  axis(4,cex.axis=1,mgp=c(0,.5,0), las=2)
}
  
  
##--- 12. MODEL COMPARISON THROUGH PSEUDO-R^2  ---####
##------------------------------------------------####
# Pseudo-R^2 calculated as correlation between observed and predicted values - this is what was done in
# in the Ballesteros-Meija paper - I am uncertain whether is is desirable for me.
# However, it seems to be my best option for a Goodness of Fit-test - I can compare them to the R^2 of the
# non-spatial models (calculated as (1-(Residual deviance/Null deviance))).
# Get the needed data:
pseudo_tax <- full_join(TrdRast_clust_model_birds@data[,c("Pixelnr", "log_chao.birds", "log_chao.red_birds", "log_chao.black_birds")],
                        TrdRast_clust_model_plants@data[,c("Pixelnr", "log_chao.plants", "log_chao.red_plants", "log_chao.black_plants")], by="Pixelnr") %>%
  full_join(., TrdRast_clust_model_other@data[,c("Pixelnr", "log_chao.other", "log_chao.red_other", "log_chao.black_other")], by="Pixelnr") %>%
  full_join(., TrdRast_clust_model_plantsother@data[,c("Pixelnr", "log_chao.plantsother", "log_chao.red_plantsother", "log_chao.black_plantsother")], by="Pixelnr") %>%
  full_join(., data_predict_tax@data[,c("Pixelnr", "predict_birds", "predict_red_birds", "predict_black_birds",
                                        "predict_other", "predict_red_other", "predict_black_other", 
                                        "predict_plantsother", "predict_red_plantsother", "predict_black_plantsother")],by="Pixelnr")

# Make the calculations (and potentially compare) - the correlations for the full models are:
# 0.304 for all species,  0.594 for threatened and  0.473 for alien species
cor(pseudo_tax$log_chao.birds, pseudo_tax$predict_birds, use='complete.obs')               # 0.365
      # No plants
cor(pseudo_tax$log_chao.other, pseudo_tax$predict_other, use='complete.obs')               # 0.403
cor(pseudo_tax$log_chao.plantsother, pseudo_tax$predict_plantsother, use='complete.obs')   # 0.215

cor(pseudo_tax$log_chao.red_birds, pseudo_tax$predict_red_birds, use='complete.obs')       # 0.548
      # No plants
cor(pseudo_tax$log_chao.red_other, pseudo_tax$predict_red_other, use='complete.obs')       # 0.297
cor(pseudo_tax$log_chao.red_plantsother, pseudo_tax$predict_red_plantsother, use='complete.obs')   # 0.123

cor(pseudo_tax$log_chao.black_birds, pseudo_tax$predict_black_birds, use='complete.obs')   # 0.156
      # No plants
cor(pseudo_tax$log_chao.black_other, pseudo_tax$predict_black_other, use='complete.obs')   # 0.448
cor(pseudo_tax$log_chao.black_plantsother, pseudo_tax$predict_black_plantsother, use='complete.obs')   # 0.297


##--- 13. Prediction plot (to evaluate SE) ---####
##--------------------------------------------####
# Make the needed predictions (in the right way)
#A:Specify covariate values for predictions
{MyData.tax <- TrdRast_clust[, c(1, 83, 86, 88)]
# Remove the grid cells with categories which cannot be used i the model (0)
MyData.tax <- MyData.tax[!MyData.tax@data$clusterCut==0,]
MyData.tax <- MyData.tax[!is.na(MyData.tax@data$north.mean),]
MyData.tax@data$clusterCut <- as.factor(MyData.tax@data$clusterCut)}

#C. Calculate predicted values and #D. Calculate standard error
{MyData.tax$log_chao.birds <- predict(gls_avg_birds, newdata=MyData.tax, se.fit=TRUE)$fit
      MyData.tax$se.log_chao.birds <- predict(gls_avg_birds, newdata=MyData.tax, se.fit=TRUE)$se.fit
  MyData.tax$log_chao.red_birds <- predict(gls_avg_red_birds, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.red_birds <- predict(gls_avg_red_birds, newdata=MyData.tax, se.fit=TRUE)$se.fit
    MyData.tax$log_chao.black_birds <- predict(gls_avg_black_birds, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.black_birds <- predict(gls_avg_black_birds, newdata=MyData.tax, se.fit=TRUE)$se.fit

MyData.tax$log_chao.other <- predict(gls_avg_other, newdata=MyData.tax, se.fit=TRUE)$fit
      MyData.tax$se.log_chao.other <- predict(gls_avg_other, newdata=MyData.tax, se.fit=TRUE)$se.fit
  MyData.tax$log_chao.red_other <- predict(gls_avg_red_other, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.red_other <- predict(gls_avg_red_other, newdata=MyData.tax, se.fit=TRUE)$se.fit
  MyData.tax$log_chao.black_other <- predict(gls_avg_black_other, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.black_other <- predict(gls_avg_black_other, newdata=MyData.tax, se.fit=TRUE)$se.fit

MyData.tax$log_chao.plantsother <- predict(gls_avg_plantsother, newdata=MyData.tax, se.fit=TRUE)$fit
      MyData.tax$se.log_chao.plantsother <- predict(gls_avg_plantsother, newdata=MyData.tax, se.fit=TRUE)$se.fit
  MyData.tax$log_chao.red_plantsother <- predict(gls_avg_red_plantsother, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.red_plantsother <- predict(gls_avg_red_plantsother, newdata=MyData.tax, se.fit=TRUE)$se.fit
  MyData.tax$log_chao.black_plantsother <- predict(gls_avg_black_plantsother, newdata=MyData.tax, se.fit=TRUE)$fit
        MyData.tax$se.log_chao.black_plantsother <- predict(gls_avg_black_plantsother, newdata=MyData.tax, se.fit=TRUE)$se.fit
}

# Calculate 0.95 confidence interval
{MyData.tax$SeUp_birds <- MyData.tax$log_chao.birds + 1.96 * MyData.tax$se.log_chao.birds
  MyData.tax$SeLo_birds <- MyData.tax$log_chao.birds - 1.96 * MyData.tax$se.log_chao.birds
  MyData.tax$SeUp_red_birds <- MyData.tax$log_chao.red_birds + 1.96 * MyData.tax$se.log_chao.red_birds
  MyData.tax$SeLo_red_birds <- MyData.tax$log_chao.red_birds - 1.96 * MyData.tax$se.log_chao.red_birds
  MyData.tax$SeUp_black_birds <- MyData.tax$log_chao.black_birds + 1.96 * MyData.tax$se.log_chao.black_birds
  MyData.tax$SeLo_black_birds <- MyData.tax$log_chao.black_birds - 1.96 * MyData.tax$se.log_chao.black_birds
  
  MyData.tax$SeUp_other <- MyData.tax$log_chao.other + 1.96 * MyData.tax$se.log_chao.other
  MyData.tax$SeLo_other <- MyData.tax$log_chao.other - 1.96 * MyData.tax$se.log_chao.other
  MyData.tax$SeUp_red_other <- MyData.tax$log_chao.red_other + 1.96 * MyData.tax$se.log_chao.red_other
  MyData.tax$SeLo_red_other <- MyData.tax$log_chao.red_other - 1.96 * MyData.tax$se.log_chao.red_other
  MyData.tax$SeUp_black_other <- MyData.tax$log_chao.black_other + 1.96 * MyData.tax$se.log_chao.black_other
  MyData.tax$SeLo_black_other <- MyData.tax$log_chao.black_other - 1.96 * MyData.tax$se.log_chao.black_other
  
  MyData.tax$SeUp_plantsother <- MyData.tax$log_chao.plantsother + 1.96 * MyData.tax$se.log_chao.plantsother
  MyData.tax$SeLo_plantsother <- MyData.tax$log_chao.plantsother - 1.96 * MyData.tax$se.log_chao.plantsother
  MyData.tax$SeUp_red_plantsother <- MyData.tax$log_chao.red_plantsother + 1.96 * MyData.tax$se.log_chao.red_plantsother
  MyData.tax$SeLo_red_plantsother <- MyData.tax$log_chao.red_plantsother - 1.96 * MyData.tax$se.log_chao.red_plantsother
  MyData.tax$SeUp_black_plantsother <- MyData.tax$log_chao.black_plantsother + 1.96 * MyData.tax$se.log_chao.black_plantsother
  MyData.tax$SeLo_black_plantsother <- MyData.tax$log_chao.black_plantsother - 1.96 * MyData.tax$se.log_chao.black_plantsother
  }

# Add the data from the original models as well, for comparison:
MyData.tax@data <- merge(MyData.tax@data, MyData2[,c(1,4:15)], by="Pixelnr", all=T)


### Plot the data from the similar models to evaluate them against each other
# The original data
{Trd_ggplot_data <- data.frame(Pixelnr= c(rep(TrdRast_clust@data$Pixelnr, (3*5))),
                                  clusterCut= as.character(c(rep(TrdRast_clust@data$clusterCut, (3*5)))),
                                  Divers = c(rep(TrdRast_clust@data$Divers, (3*5))),
                                  north.mean = c(rep(TrdRast_clust@data$north.mean, (3*5))),  
                                  log.chao= c(TrdRast_clust@data$log_chao.all, TrdRast_clust@data$log_chao.reds, TrdRast_clust@data$log_chao.blacks,
                                              TrdRast_clust@data$log_chao.birds,TrdRast_clust@data$log_chao.red_birds, TrdRast_clust@data$log_chao.black_birds,
                                              TrdRast_clust@data$log_chao.plants, TrdRast_clust@data$log_chao.red_plants, TrdRast_clust@data$log_chao.black_plants,
                                              TrdRast_clust@data$log_chao.other, TrdRast_clust@data$log_chao.red_other, TrdRast_clust@data$log_chao.black_other,
                                              TrdRast_clust@data$log_chao.plantsother, TrdRast_clust@data$log_chao.red_plantsother, TrdRast_clust@data$log_chao.black_plantsother),
                                  Group= factor(c(rep("All", nrow(TrdRast_clust@data)), rep("Threatened_all", nrow(TrdRast_clust@data)), rep("Alien_all", nrow(TrdRast_clust@data)),
                                                  rep("Birds", nrow(TrdRast_clust@data)), rep("Threat_birds", nrow(TrdRast_clust@data)), rep("Alien_birds", nrow(TrdRast_clust@data)),
                                                  rep("Plants", nrow(TrdRast_clust@data)), rep("Threat_plants", nrow(TrdRast_clust@data)), rep("Alien_plants", nrow(TrdRast_clust@data)),
                                                  rep("Other", nrow(TrdRast_clust@data)), rep("Threat_other", nrow(TrdRast_clust@data)), rep("Alien_other", nrow(TrdRast_clust@data)),
                                                  rep("Plants_other", nrow(TrdRast_clust@data)), rep("Threat_plants_other", nrow(TrdRast_clust@data)), rep("Alien_plants_other", nrow(TrdRast_clust@data))),
                                                levels=c("All", "Threatened_all", "Alien_all",
                                                         "Birds", "Threat_birds", "Alien_birds",
                                                         "Plants", "Threat_plants", "Alien_plants",
                                                         "Other", "Threat_other", "Alien_other",
                                                         "Plants_other", "Threat_plants_other", "Alien_plants_other")))}

# Prepare the predicted values
{MyData.tax2 <- data.frame(Pixelnr=c(rep(MyData.tax$Pixelnr, (3*4))),
                      clusterCut=factor(c(rep(as.character(MyData.tax$clusterCut), (3*4))),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", 10, "11", "12")),
                      Divers=c(rep(MyData.tax$Divers, (3*4))),
                      north.mean=c(rep(MyData.tax$north.mean, (3*4))),
                      log.chao=c(MyData.tax$log_chao.all, MyData.tax$log_chao.reds, MyData.tax$log_chao.blacks,
                                 MyData.tax$log_chao.birds, MyData.tax$log_chao.red_birds, MyData.tax$log_chao.black_birds,
                                 MyData.tax$log_chao.other, MyData.tax$log_chao.red_other, MyData.tax$log_chao.black_other,
                                 MyData.tax$log_chao.plantsother, MyData.tax$se.log_chao.red_plantsother, MyData.tax$log_chao.black_plantsother),
                      SE= c(MyData.tax$se.log_chao.all, MyData.tax$se.log_chao.reds, MyData.tax$se.log_chao.blacks,
                            MyData.tax$se.log_chao.birds, MyData.tax$se.log_chao.red_birds, MyData.tax$se.log_chao.black_birds,
                            MyData.tax$se.log_chao.other, MyData.tax$se.log_chao.red_other, MyData.tax$se.log_chao.black_other,
                            MyData.tax$se.log_chao.plantsother, MyData.tax$se.log_chao.red_plantsother, MyData.tax$se.log_chao.black_plantsother),
                      Group=factor(c(rep("All", nrow(MyData.tax)), rep("Threatened_all", nrow(MyData.tax)), rep("Alien_all", nrow(MyData.tax)),
                                     rep("Birds", nrow(MyData.tax)), rep("Threat_birds", nrow(MyData.tax)), rep("Alien_birds", nrow(MyData.tax)),
                                     rep("Other", nrow(MyData.tax)), rep("Threat_other", nrow(MyData.tax)), rep("Alien_other", nrow(MyData.tax)),
                                     rep("Plants_other", nrow(MyData.tax)), rep("Threat_plants_other", nrow(MyData.tax)), rep("Alien_plants_other", nrow(MyData.tax))),
                                   levels=c("All", "Threatened_all", "Alien_all",
                                            "Birds", "Threat_birds", "Alien_birds",
                                            "Other", "Threat_other", "Alien_other",
                                            "Plants_other", "Threat_plants_other", "Alien_plants_other")))
}

# Calculate confidence interval
MyData.tax2$SeUp <- MyData.tax2$log.chao + 1.96 * MyData.tax2$SE
MyData.tax2$SeLow <- MyData.tax2$log.chao - 1.96 * MyData.tax2$SE


# Make the plots - one at a time!
# Full model
{P_all <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="All",],
                aes(x=Divers, y=log.chao))   +
  ggtitle("All species, all subgroups")  +
  geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="All",],
              aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="blue")  +
  geom_line(data=MyData.tax2[MyData.tax2$Group=="All",], 
                  alpha=.75,lwd=1,show.legend=F, colour="blue")   +
  geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="All",],
             stat="identity" ,size=1,  colour="blue")  +   # 'identity' takes you back to the original dataset
  facet_wrap(~clusterCut, scale="fixed")  +
  ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
  theme_bw() +
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
    ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
    ,strip.background = element_rect(fill="transparent",colour=NA))   +
  annotate(geom = 'segment', y = -Inf, yend = Inf,
           color = 'black', x = 0, xend = 0, size = 0.5)    +
  annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
           x = -Inf, xend = Inf, size = 0.5) 
P_all}
{P_threat <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threatened_all",],
                 aes(x=Divers, y=log.chao))   +
    ggtitle("All species, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threatened_all",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threatened_all",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threatened_all",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    facet_wrap(~clusterCut, scale="fixed")  +
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  P_threat}
{P_alien <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_all",],
                    aes(x=Divers, y=log.chao))   +
    ggtitle("All species, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_all",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_all",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_all",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    facet_wrap(~clusterCut, scale="fixed")  +
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  P_alien}

# Birds
{P_birds <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Birds",],
                 aes(x=Divers, y=log.chao))   +
    ggtitle("Birds, all subgroups")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Birds",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="blue")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Birds",], 
              alpha=.75,lwd=1,show.legend=F, colour="blue")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Birds",],
               stat="identity" ,size=1, colour="blue")  +   # 'identity' takes you back to the original dataset
    facet_wrap(~clusterCut, scale="fixed")  +
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  P_birds}
library(gridExtra)
{{P_red_birds1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_birds",],
                    aes(x=Divers, y=log.chao))   +
    ggtitle("Birds, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_birds",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_birds",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_birds",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    facet_wrap(~clusterCut, scale="fixed")  +
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) }
  {P_red_birds2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_birds",],
                          aes(x=north.mean, y=log.chao))   +
      ggtitle("Birds, threatened")  +
      geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_birds",],
                  aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
      geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_birds",], 
                alpha=.75,lwd=1,show.legend=F, colour="red")   +
      geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_birds",],
                 stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
      facet_wrap(~clusterCut, scale="fixed")  +
      ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
      theme_bw() +
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
        ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
        ,strip.background = element_rect(fill="transparent",colour=NA))   +
      annotate(geom = 'segment', y = -Inf, yend = Inf,
               color = 'black', x = 0, xend = 0, size = 0.5)    +
      annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
               x = -Inf, xend = Inf, size = 0.5) }
grid.arrange(P_red_birds1, P_red_birds2, nrow=1)
  }
{P_black_birds1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_birds",],
                   aes(x=Divers, y=log.chao))   +
    ggtitle("Birds, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_birds",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_birds",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_birds",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  
  P_black_birds2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_birds",],
                           aes(x=north.mean, y=log.chao))   +
    ggtitle("Birds, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_birds",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_birds",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_birds",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5)
  grid.arrange(P_black_birds1, P_black_birds2, nrow=1)}

# Plants - no models, just data
{P_plants <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Plants",],
                 aes(x=Divers, y=log.chao))   +
    ggtitle("Plants, all subgroups")  +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Plants",],
               stat="identity" ,size=1, colour="blue")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
P_red_plants <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants",],
                    aes(x=Divers, y=log.chao))   +
    ggtitle("Plants, threatened")  +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
P_black_plants <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_plants",],
                   aes(x=Divers, y=log.chao))   +
    ggtitle("Plants, alien")  +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_plants",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  grid.arrange(P_plants, P_red_plants, P_black_plants, nrow=1)}

# Other taxa
{P_other <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Other",],
                 aes(x=Divers, y=log.chao))   +
    ggtitle("Other, all subgroups")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="blue")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Other",], 
              alpha=.75,lwd=1,show.legend=F, colour="blue")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Other",],
               stat="identity" ,size=1, colour="blue")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5)
  P_other}
{P_red_other1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_other",],
                    aes(x=Divers, y=log.chao))   +
    ggtitle("Other, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_other",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 

  P_red_other2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_other",],
                         aes(x=north.mean, y=log.chao))   +
    ggtitle("Other, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_other",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
grid.arrange(P_red_other1, P_red_other2, nrow=1)}
{P_black_other1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_other",],
                   aes(x=Divers, y=log.chao))   +
    ggtitle("Other, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_other",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_other",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
P_black_other2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_other",],
                           aes(x=north.mean, y=log.chao))   +
    ggtitle("Other, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_other",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_other",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5)
grid.arrange(P_black_other1, P_black_other2, nrow=1)}

# Plants and other taxa
{P_plantsother1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Plants_other",],
                 aes(x=Divers, y=log.chao))   +
    ggtitle("Plants/other, all subgroups")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Plants_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="blue")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Plants_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="blue")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Plants_other",],
               stat="identity" ,size=1, colour="blue")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
P_plantsother2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Plants_other",],
                           aes(x=north.mean, y=log.chao))   +
    ggtitle("Plants/other, all subgroups")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Plants_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="blue")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Plants_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="blue")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Plants_other",],
               stat="identity" ,size=1, colour="blue")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
grid.arrange(P_plantsother1, P_plantsother2, nrow=1)}
{P_red_plantsother1 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants_other",],
                          aes(x=Divers, y=log.chao))   +
    ggtitle("Plants/other, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_plants_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_plants_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants_other",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Habitat heterogeneity")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  P_red_plantsother2 <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants_other",],
                           aes(x=north.mean, y=log.chao))   +
    ggtitle("Plants/other, threatened")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Threat_plants_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F, fill="red")  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Threat_plants_other",], 
              alpha=.75,lwd=1,show.legend=F, colour="red")   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Threat_plants_other",],
               stat="identity" ,size=1, colour="red")  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
  grid.arrange(P_red_plantsother1, P_red_plantsother2, nrow=1)}
{P_black_plantsother <- ggplot(Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_plants_other",],
                               aes(x=north.mean, y=log.chao))   +
    ggtitle("Plants/other, alien")  +
    geom_ribbon(data=MyData.tax2[MyData.tax2$Group=="Alien_plants_other",],
                aes(ymin=SeLow, ymax=SeUp),alpha=.4,lwd=NA,show.legend=F)  +
    geom_line(data=MyData.tax2[MyData.tax2$Group=="Alien_plants_other",], 
              alpha=.75,lwd=1,show.legend=F)   +
    geom_point(data=Trd_ggplot_data[Trd_ggplot_data$Group=="Alien_plants_other",],
               stat="identity" ,size=1)  +   # 'identity' takes you back to the original dataset
    ylab("log(ESR +1) (Chao's)")+xlab("Northness")   +  # Adding x and ylabs to plot
    theme_bw() +
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
      ,strip.text.x = element_text(size = 12, hjust=0.1,colour = "black") # The text size of the striP_all (facet panel) titles
      ,strip.background = element_rect(fill="transparent",colour=NA))   +
    annotate(geom = 'segment', y = -Inf, yend = Inf,
             color = 'black', x = 0, xend = 0, size = 0.5)    +
    annotate(geom = 'segment', y = 0, yend = 0, color = 'black',
             x = -Inf, xend = Inf, size = 0.5) 
P_black_plantsother}


##--- 14. Comparisons of coefficients ---####
##--- 14.1 Make models with scaled       ####
##---      variables                     ####
##---------------------------------------####
# Standardize the response values - we do that by both scaling and centering:
{TrdRast_clust_model_birds$log_chao.birds.std <- scale(TrdRast_clust_model_birds$log_chao.birds, center = TRUE, scale = TRUE)
TrdRast_clust_model_birds$log_chao.red_birds.std <- scale(TrdRast_clust_model_birds$log_chao.red_birds, center = TRUE, scale = TRUE) 
TrdRast_clust_model_birds$log_chao.black_birds.std <- scale(TrdRast_clust_model_birds$log_chao.black_birds, center = TRUE, scale = TRUE)

TrdRast_clust_model_plants$log_chao.plants.std <- scale(TrdRast_clust_model_plants$log_chao.plants, center = TRUE, scale = TRUE)
TrdRast_clust_model_plants$log_chao.red_plants.std <- scale(TrdRast_clust_model_plants$log_chao.red_plants, center = TRUE, scale = TRUE) 
TrdRast_clust_model_plants$log_chao.black_plants.std <- scale(TrdRast_clust_model_plants$log_chao.black_plants, center = TRUE, scale = TRUE)

TrdRast_clust_model_other$log_chao.other.std <- scale(TrdRast_clust_model_other$log_chao.other, center = TRUE, scale = TRUE)
TrdRast_clust_model_other$log_chao.red_other.std <- scale(TrdRast_clust_model_other$log_chao.red_other, center = TRUE, scale = TRUE) 
TrdRast_clust_model_other$log_chao.black_other.std <- scale(TrdRast_clust_model_other$log_chao.black_other, center = TRUE, scale = TRUE)

TrdRast_clust_model_plantsother$log_chao.plantsother.std <- scale(TrdRast_clust_model_plantsother$log_chao.plantsother, center = TRUE, scale = TRUE)
TrdRast_clust_model_plantsother$log_chao.red_plantsother.std <- scale(TrdRast_clust_model_plantsother$log_chao.red_plantsother, center = TRUE, scale = TRUE) 
TrdRast_clust_model_plantsother$log_chao.black_plantsother.std <- scale(TrdRast_clust_model_plantsother$log_chao.black_plantsother, center = TRUE, scale = TRUE)}
##--- 14.1.1 Chao1_reds     ---####
##-----------------------------####
## Birds ####
(gls_ML_red_birds.std <- gls(log_chao.red_birds.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_birds@data,
                            correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                            method = "ML")) 

d.red_birds.std <- dredge(gls_ML_red_birds.std)
d.red2_birds.std <- subset(d.red_birds.std, delta<2)
output_red_birds.std <- model.sel(d.red2_birds.std)

gls_avg_red_birds.std <- model.avg(output_red_birds.std, fit=TRUE) 

## Plants ####
(gls_ML_red_plants.std <- gls(log_chao.red_plants.std ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_plants@data,
                             correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                             method = "ML")) 

d.red_plants.std <- dredge(gls_ML_red_plants.std)
d.red2_plants.std <- subset(d.red_plants.std, delta<2)
output_red_plants.std <- model.sel(d.red2_plants.std)

## Other ####
(gls_ML_red_other.std <- gls(log_chao.red_other.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_other@data,
                            correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_other.std <- dredge(gls_ML_red_other.std)
d.red2_other.std <- subset(d.red_other.std, delta<2)
output_red_other.std <- model.sel(d.red2_other.std)

gls_avg_red_other.std <- model.avg(output_red_other.std, fit=TRUE) 

## Plants and other ####
(gls_ML_plantsother.std <- gls(log_chao.red_plantsother.std ~  clusterCut + Divers + north.mean,
                                  data = TrdRast_clust_model_plantsother@data,
                                  correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                                  method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.red_plantsother.std <- dredge(gls_ML_plantsother.std)
d.red2_plantsother.std <- subset(d.red_plantsother.std, delta<2)
output_red_plantsother.std <- model.sel(d.red2_plantsother.std)

gls_avg_red_plantsother.std <- model.avg(output_red_plantsother.std, fit=TRUE) 


##--- 14.1.2 Chao1_blacks    ---####
##------------------------------####
## Birds ####
(gls_ML_black_birds.std <- gls(log_chao.black_birds.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_birds@data,
                            correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_birds.std <- dredge(gls_ML_black_birds.std)
d.black2_birds.std <- subset(d.black_birds.std, delta<2)
output_black_birds.std <- model.sel(d.black2_birds.std)

gls_avg_black_birds.std <- model.avg(output_black_birds.std, fit=TRUE) 

## Plants ####
(gls_ML_black_plants.std <- gls(log_chao.black_plants.std ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_plants@data,
                             correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                             method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_plants.std <- dredge(gls_ML_black_plants.std)
d.black2_plants.std <- subset(d.black_plants.std, delta<2)
output_black_plants.std <- model.sel(d.black2_plants.std)

## Other ####
(gls_ML_black_other.std <- gls(log_chao.black_other.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_other@data,
                            correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_other.std <- dredge(gls_ML_black_other.std)
d.black2_other.std <- subset(d.black_other.std, delta<2)
output_black_other.std <- model.sel(d.black2_other.std)

gls_avg_black_other.std <- model.avg(output_black_other.std, fit=TRUE) 

## Plants and other ####
(gls_ML_black_plantsother.std <- gls(log_chao.black_plantsother.std ~  clusterCut + Divers + north.mean,
                                  data = TrdRast_clust_model_plantsother@data,
                                  correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                                  method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d.black_plantsother.std <- dredge(gls_ML_black_plantsother.std)
d.black2_plantsother.std <- subset(d.black_plantsother.std, delta<2)
output_black_plantsother.std <- model.sel(d.black2_plantsother.std)

gls_avg_black_plantsother.std <- model.avg(output_black_plantsother.std, fit=TRUE) 


##--- 14.1.3 Chao1_all    ---####
##---------------------------####

## Birds ####
(gls_ML_birds.std <- gls(log_chao.birds.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_birds@data,
                            correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_birds.std <- dredge(gls_ML_birds.std)
d2_birds.std <- subset(d_birds.std, delta<2)
output_birds.std <- model.sel(d2_birds.std)

gls_avg_birds.std <- gls(log_chao.birds.std ~  clusterCut + Divers,
                     data = TrdRast_clust_model_birds@data,
                     correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]),
                     method = "ML") 

## Plants ####
(gls_ML_plants.std <- gls(log_chao.plants.std ~  clusterCut + Divers + north.mean,
                             data = TrdRast_clust_model_plants@data,
                             correlation=corExp(form=~xy_plants[,1]+xy_plants[,2]),
                             method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_plants.std <- dredge(gls_ML_plants.std)
d2_plants.std <- subset(d_plants.std, delta<2)
output_plants.std <- model.sel(d2_plants.std)

## Other ####
(gls_ML_other.std <- gls(log_chao.other.std ~  clusterCut + Divers + north.mean,
                            data = TrdRast_clust_model_other@data,
                            correlation=corExp(form=~xy_other[,1]+xy_other[,2]),
                            method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_other.std <- dredge(gls_ML_other.std)
d2_other.std <- subset(d_other.std, delta<2)
output_other.std <- model.sel(d2_other.std)

gls_avg_other.std <- model.avg(output_other.std, fit=TRUE) 

## Plants and other ####
(gls_ML_plantsother.std <- gls(log_chao.plantsother.std ~  clusterCut + Divers + north.mean,
                                  data = TrdRast_clust_model_plantsother@data,
                                  correlation=corExp(form=~xy_plantsother[,1]+xy_plantsother[,2]),
                                  method = "ML")) # We need the method to be "ML", otherwise the comparison of AIC does not work properly

d_plantsother.std <- dredge(gls_ML_plantsother.std)
d2_plantsother.std <- subset(d_plantsother.std, delta<2)
output_plantsother.std <- model.sel(d2_plantsother.std)

gls_avg_plantsother.std <- model.avg(output_plantsother.std, fit=TRUE) 


##--- 14.2 Making the coefficient plots  ---####
##------------------------------------------####
# Both threatened, alien and others:
par(mar=c(4,4,4,2))
par(mfrow=c(3,5))

### Habitat cluster ####
## Cluster 1 ####
# Full model
{plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Coastal")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , summary(gls_avg_all.std)$coefmat.full[1,1] - summary(gls_avg_all.std)$coefmat.full[1,3],  
          x1=0.5 , y1=summary(gls_avg_all.std)$coefmat.full[1,1] + summary(gls_avg_all.std)$coefmat.full[1,3], col="blue")
points(0.5 , (summary(gls_avg_all.std)$coefmat.full[1,1]), pch=15, col="blue")
segments( 0.4, coef(summary(gls_avg_reds.std))[1,1] - coef(summary(gls_avg_reds.std))[1,2],  
           x1=0.4, y1=coef(summary(gls_avg_reds.std))[1,1] + coef(summary(gls_avg_reds.std))[1,2], col="red")
  points(0.4, (coef(summary(gls_avg_reds.std))[1,1]), pch=16, col="red")
segments( 0.6, summary(gls_avg_blacks.std)$coefmat.full[1,1] - summary(gls_avg_blacks.std)$coefmat.full[1,3],  
            x1=0.6, y1=summary(gls_avg_blacks.std)$coefmat.full[1,1] + summary(gls_avg_blacks.std)$coefmat.full[1,3], col="black")
  points(0.6, summary(gls_avg_blacks.std)$coefmat.full[1,1], pch=17, col="black") }
  
# Birds
{segments( 1 , coef(summary(gls_avg_birds.std))[1,1] - coef(summary(gls_avg_birds.std))[1,2],  
          x1=1 , y1=coef(summary(gls_avg_birds.std))[1,1] + coef(summary(gls_avg_birds.std))[1,2], col="blue")
points(1 , (coef(summary(gls_avg_birds.std))[1,1]), pch=15, col="blue")
{segments( 0.9 , summary(gls_avg_red_birds.std)$coefmat.full[1,1] - summary(gls_avg_red_birds.std)$coefmat.full[1,3],  
           x1=0.9 , y1=summary(gls_avg_red_birds.std)$coefmat.full[1,1] + summary(gls_avg_red_birds.std)$coefmat.full[1,3], col="red")
  points(0.9 , (summary(gls_avg_red_birds.std)$coefmat.full[1,1]), pch=15, col="red") }
}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 2 ####
# Full model
{p2.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Urban/developed")
  axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
  segments( 0.5 , (summary(p2.all)$coefmat.full[1,1] - summary(p2.all)$coefmat.full[1,3]),  
            x1=0.5 , y1=(summary(p2.all)$coefmat.full[1,1] + summary(p2.all)$coefmat.full[1,3]), col="blue" )
  points(0.5 , summary(p2.all)$coefmat.full[1,1], pch=15, col="blue")
  rm(p2.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p2_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p2_blacks)$coefmat.full[1,1] - summary(p2_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p2_blacks)$coefmat.full[1,1] + summary(p2_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p2_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p2_blacks)}
# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model_birds@data,
                                              correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                               coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model_birds@data,
                                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
                       x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model_birds@data,
                                                    correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                                     coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model_birds@data,
                                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="2") + Divers, data = TrdRast_clust_model_birds@data,
                               correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p2.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="2")))), subset=delta<2, fit=TRUE)
segments( 0.9 , (summary(p2.birds)$coefmat.full[1,1] - summary(p2.birds)$coefmat.full[1,3]),  
          x1=0.9 , y1=(summary(p2.birds)$coefmat.full[1,1] + summary(p2.birds)$coefmat.full[1,3]), col="red" )
points(0.9 , summary(p2.birds)$coefmat.full[1,1], pch=15, col="red")
rm(p2.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 3 ####
# Full model
{p3.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Urban/vegetated/riparian")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p3.all)$coefmat.full[1,1] - summary(p3.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p3.all)$coefmat.full[1,1] + summary(p3.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p3.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p3.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p3_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p3_blacks)$coefmat.full[1,1] - summary(p3_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p3_blacks)$coefmat.full[1,1] + summary(p3_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p3_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p3_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="3") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p3.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="3")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p3.birds)$coefmat.full[1,1] - summary(p3.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p3.birds)$coefmat.full[1,1] + summary(p3.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p3.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p3.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model
## Cluster 4 ####
# Full model
{p4.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Cultivated land")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p4.all)$coefmat.full[1,1] - summary(p4.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p4.all)$coefmat.full[1,1] + summary(p4.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p4.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p4.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p4_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p4_blacks)$coefmat.full[1,1] - summary(p4_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p4_blacks)$coefmat.full[1,1] + summary(p4_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p4_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p4_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="4") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p4.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="4")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p4.birds)$coefmat.full[1,1] - summary(p4.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p4.birds)$coefmat.full[1,1] + summary(p4.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p4.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p4.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 5 ####
# Full model
{p5.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Conif., low prod.")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p5.all)$coefmat.full[1,1] - summary(p5.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p5.all)$coefmat.full[1,1] + summary(p5.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p5.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p5.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p5_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p5_blacks)$coefmat.full[1,1] - summary(p5_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p5_blacks)$coefmat.full[1,1] + summary(p5_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p5_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p5_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="5") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p5.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="5")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p5.birds)$coefmat.full[1,1] - summary(p5.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p5.birds)$coefmat.full[1,1] + summary(p5.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p5.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p5.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 6 ####
# Full model
{p6.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Conif., medium prod.")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p6.all)$coefmat.full[1,1] - summary(p6.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p6.all)$coefmat.full[1,1] + summary(p6.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p6.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p6.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p6_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p6_blacks)$coefmat.full[1,1] - summary(p6_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p6_blacks)$coefmat.full[1,1] + summary(p6_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p6_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p6_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="6") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p6.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="6")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p6.birds)$coefmat.full[1,1] - summary(p6.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p6.birds)$coefmat.full[1,1] + summary(p6.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p6.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p6.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 7 ####
# Full model
{p7.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Open marsh & conif.")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p7.all)$coefmat.full[1,1] - summary(p7.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p7.all)$coefmat.full[1,1] + summary(p7.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p7.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p7.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                     coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                      correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
             x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                           coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                                            correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p7_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p7_blacks)$coefmat.full[1,1] - summary(p7_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p7_blacks)$coefmat.full[1,1] + summary(p7_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p7_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p7_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="7") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p7.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="7")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p7.birds)$coefmat.full[1,1] - summary(p7.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p7.birds)$coefmat.full[1,1] + summary(p7.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p7.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p7.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 8 ####
# Full model
{p8.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Conif, high prod.")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p8.all)$coefmat.full[1,1] - summary(p8.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p8.all)$coefmat.full[1,1] + summary(p8.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p8.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p8.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")
}
{p8_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p8_blacks)$coefmat.full[1,1] - summary(p8_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p8_blacks)$coefmat.full[1,1] + summary(p8_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p8_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p8_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="8") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p8.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="8")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p8.birds)$coefmat.full[1,1] - summary(p8.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p8.birds)$coefmat.full[1,1] + summary(p8.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p8.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p8.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model
## Cluster 10 ####
# Full model
{plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Open firm ground and forest")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)}
# Not for full model
# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="10") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="10") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="10") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="10") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="10") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p10.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="10")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p10.birds)$coefmat.full[1,1] - summary(p10.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p10.birds)$coefmat.full[1,1] + summary(p10.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p10.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p10.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 11 ####
# Full model
{p11.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Open firm ground and cult.")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p11.all)$coefmat.full[1,1] - summary(p11.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p11.all)$coefmat.full[1,1] + summary(p11.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p11.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p11.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p11_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p11_blacks)$coefmat.full[1,1] - summary(p11_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p11_blacks)$coefmat.full[1,1] + summary(p11_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p1_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p11_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="11") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p11.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="11")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p11.birds)$coefmat.full[1,1] - summary(p11.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p11.birds)$coefmat.full[1,1] + summary(p11.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p11.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p11.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

## Cluster 12 ####
# Full model
{p12.all <- model.avg(model.sel(dredge(update(gls.a_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Freshwater")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
segments( 0.5 , (summary(p12.all)$coefmat.full[1,1] - summary(p12.all)$coefmat.full[1,3]),  
          x1=0.5 , y1=(summary(p12.all)$coefmat.full[1,1] + summary(p12.all)$coefmat.full[1,3]), col="blue" )
points(0.5 , summary(p12.all)$coefmat.full[1,1], pch=15, col="blue")
rm(p12.all)}
{segments( 0.4, (coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                  correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] -
                   coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                    correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]),  
           x1=0.4, y1=(coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                        correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1] +
                         coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                                          correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,2]), col="red")
  points(0.4, coef(summary(gls(log_chao.reds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model@data,
                               correlation=corExp(form=~xy_clust[,1]+xy_clust[,2]), method = "ML")))[1,1], pch=16, col="red")}
{p12_blacks <- model.avg(model.sel(dredge(update(gls.b_ML_clust.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 0.6, (summary(p12_blacks)$coefmat.full[1,1] - summary(p12_blacks)$coefmat.full[1,3]),  
            x1=0.6, y1=(summary(p12_blacks)$coefmat.full[1,1] + summary(p12_blacks)$coefmat.full[1,3]), col="black")
  points(0.6, summary(p12_blacks)$coefmat.full[1,1], pch=17, col="black")
  rm(p12_blacks)}

# Birds
{segments( 1, (coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model_birds@data,
                                correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] -
                 coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model_birds@data,
                                  correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]),  
           x1=1, y1=(coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model_birds@data,
                                      correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1] +
                       coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model_birds@data,
                                        correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,2]), col="blue" )
  points(1, coef(summary(gls(log_chao.birds.std ~  relevel(clusterCut, ref="12") + Divers, data = TrdRast_clust_model_birds@data,
                             correlation=corExp(form=~xy_birds[,1]+xy_birds[,2]), method = "ML")))[1,1], pch=15, col="blue")  }
{p12.birds <- model.avg(model.sel(dredge(update(gls_ML_red_birds.std, ~ . - clusterCut + relevel(clusterCut, ref="12")))), subset=delta<2, fit=TRUE)
  segments( 0.9 , (summary(p12.birds)$coefmat.full[1,1] - summary(p12.birds)$coefmat.full[1,3]),  
            x1=0.9 , y1=(summary(p12.birds)$coefmat.full[1,1] + summary(p12.birds)$coefmat.full[1,3]), col="red" )
  points(0.9 , summary(p12.birds)$coefmat.full[1,1], pch=15, col="red")
  rm(p12.birds)}

# Other - habitat not included in model
# Plants and Other - habitat not included in model

### Habitat heterogeneity ####
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Habitat heterogeneity")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)
# Full model
{segments(0.5, summary(gls_avg_all.std)$coefmat.full[2,1] - summary(gls_avg_all.std)$coefmat.full[2,3],
          x1=0.5, y1=summary(gls_avg_all.std)$coefmat.full[2,1] + summary(gls_avg_all.std)$coefmat.full[2,3], col="blue") 
  points(0.5, summary(gls_avg_all.std)$coefmat.full[2,1], pch=15, col="blue")
segments(0.4, coef(summary(gls_avg_reds.std))[11,1] - coef(summary(gls_avg_reds.std))[11,2],  
           x1=0.4, y1=coef(summary(gls_avg_reds.std))[11,1] + coef(summary(gls_avg_reds.std))[11,2], col="red")
  points(0.4, (coef(summary(gls_avg_reds.std))[11,1]), pch=16, col="red")
segments(0.6, summary(gls_avg_blacks.std)$coefmat.full[11,1] - summary(gls_avg_blacks.std)$coefmat.full[11,3],
         x1=0.6, y1=summary(gls_avg_blacks.std)$coefmat.full[11,1] + summary(gls_avg_blacks.std)$coefmat.full[11,3]) 
points(0.6, summary(gls_avg_blacks.std)$coefmat.full[11,1], pch=17)
}
# Birds
{segments(1, coef(summary(gls_avg_birds.std))[12,1] - coef(summary(gls_avg_birds.std))[12,2],
          x1=1, y1=coef(summary(gls_avg_birds.std))[12,1] + coef(summary(gls_avg_birds.std))[12,2], col="blue") 
  points(1, coef(summary(gls_avg_birds.std))[12,1], pch=15, col="blue")
  segments(0.9, summary(gls_avg_red_birds.std)$coefmat.full[12,1] - summary(gls_avg_red_birds.std)$coefmat.full[12,3],  
           x1=0.9, y1=summary(gls_avg_red_birds.std)$coefmat.full[12,1] + summary(gls_avg_red_birds.std)$coefmat.full[12,3], col="red")
  points(0.9, summary(gls_avg_red_birds.std)$coefmat.full[12,1], pch=16, col="red")
  segments(1.1, summary(gls_avg_black_birds.std)$coefmat.full[3,1] - summary(gls_avg_black_birds.std)$coefmat.full[3,3],
           x1=1.1, y1=summary(gls_avg_black_birds.std)$coefmat.full[3,1] + summary(gls_avg_black_birds.std)$coefmat.full[3,3]) 
  points(1.1, summary(gls_avg_black_birds.std)$coefmat.full[3,1], pch=17)
}
# Other
{segments(1.5, summary(gls_avg_other.std)$coefmat.full[2,1] - summary(gls_avg_other.std)$coefmat.full[2,3],  
          x1=1.5, y1=summary(gls_avg_other.std)$coefmat.full[2,1] + summary(gls_avg_other.std)$coefmat.full[2,3], col="blue")
  points(1.5, summary(gls_avg_other.std)$coefmat.full[2,1], pch=15, col="blue")
segments(1.4, summary(gls_avg_red_other.std)$coefmat.full[3,1] - summary(gls_avg_red_other.std)$coefmat.full[3,3],  
           x1=1.4, y1=summary(gls_avg_red_other.std)$coefmat.full[3,1] + summary(gls_avg_red_other.std)$coefmat.full[3,3], col="red")
  points(1.4, summary(gls_avg_red_other.std)$coefmat.full[3,1], pch=16, col="red")
segments(1.6, summary(gls_avg_black_other.std)$coefmat.full[2,1] - summary(gls_avg_black_other.std)$coefmat.full[2,3],  
           x1=1.6, y1=summary(gls_avg_black_other.std)$coefmat.full[2,1] + summary(gls_avg_black_other.std)$coefmat.full[2,3], col="black")
  points(1.6, summary(gls_avg_black_other.std)$coefmat.full[2,1], pch=17, col="black")
}
# Not included in plants/other

### Northness ####
plot(1, type="n", xlab="", ylab="Model coefficient, scaled response", xlim=c(0,2.5), ylim=c(-3, 2), xaxt="n", main="Northness")
axis(1, at=c(0.5,1,1.5,2), labels=c("All groups", "Birds", "Other", "Plants \nand other"), las=2)

# Not in full model!

# Birds
{segments(0.9, summary(gls_avg_red_birds.std)$coefmat.full[13,1] - summary(gls_avg_red_birds.std)$coefmat.full[13,3],  
           x1=0.9, y1=summary(gls_avg_red_birds.std)$coefmat.full[13,1] + summary(gls_avg_red_birds.std)$coefmat.full[13,3], col="red")
  points(0.9, summary(gls_avg_red_birds.std)$coefmat.full[13,1], pch=16, col="red")
  segments(1.1, summary(gls_avg_black_birds.std)$coefmat.full[2,1] - summary(gls_avg_black_birds.std)$coefmat.full[2,3],
           x1=1.1, y1=summary(gls_avg_black_birds.std)$coefmat.full[2,1] + summary(gls_avg_black_birds.std)$coefmat.full[2,3]) 
  points(1.1, summary(gls_avg_black_birds.std)$coefmat.full[2,1], pch=17)
}
# Other
{segments(1.4, summary(gls_avg_red_other.std)$coefmat.full[2,1] - summary(gls_avg_red_other.std)$coefmat.full[2,3],  
          x1=1.4, y1=summary(gls_avg_red_other.std)$coefmat.full[2,1] + summary(gls_avg_red_other.std)$coefmat.full[2,3], col="red")
  points(1.4, summary(gls_avg_red_other.std)$coefmat.full[2,1], pch=16, col="red")
  segments(1.6, summary(gls_avg_black_other.std)$coefmat.full[3,1] - summary(gls_avg_black_other.std)$coefmat.full[3,3],
           x1=1.6, y1=summary(gls_avg_black_other.std)$coefmat.full[3,1] + summary(gls_avg_black_other.std)$coefmat.full[3,3]) 
  points(1.6, summary(gls_avg_black_other.std)$coefmat.full[3,1], pch=17)
}
# Plants/Other
{segments(2, summary(gls_avg_plantsother.std)$coefmat.full[2,1] - summary(gls_avg_plantsother.std)$coefmat.full[2,3],  
          x1=2, y1=summary(gls_avg_plantsother.std)$coefmat.full[2,1] + summary(gls_avg_plantsother.std)$coefmat.full[2,3], col="blue")
  points(2, summary(gls_avg_plantsother.std)$coefmat.full[2,1], pch=15, col="blue")
    segments(1.9, summary(gls_avg_red_plantsother.std)$coefmat.full[3,1] - summary(gls_avg_red_plantsother.std)$coefmat.full[3,3],  
          x1=1.9, y1=summary(gls_avg_red_plantsother.std)$coefmat.full[3,1] + summary(gls_avg_red_plantsother.std)$coefmat.full[3,3], col="red")
  points(1.9, summary(gls_avg_red_plantsother.std)$coefmat.full[3,1], pch=16, col="red")
  segments(2.1, summary(gls_avg_black_plantsother.std)$coefmat.full[2,1] - summary(gls_avg_black_plantsother.std)$coefmat.full[2,3],
           x1=2.1, y1=summary(gls_avg_black_plantsother.std)$coefmat.full[2,1] + summary(gls_avg_black_plantsother.std)$coefmat.full[2,3]) 
  points(2.1, summary(gls_avg_black_plantsother.std)$coefmat.full[2,1], pch=17)
}

