########################################
##      FUNCTIONS FOR GBIF-DATA     ####
########################################

##--- FILLING IN A COMMUNITY MATRIX ---####
##-------------------------------------####

# This functions fills out a community matrix (here called est --> rows=grid cells, columns=species),
# based on a tally (dataframe with 3 columns: Species, grid cell number, number of observations of the respective species in the respective gridcell)

ntally <- function(i, j, Tally, Com.matrix)
{
  nt1 <- subset(Tally, Tally$species==colnames(Com.matrix)[j])
  nt2 <- subset(nt1, nt1$fPixelnr==rownames(Com.matrix)[i])
  nt3 <- if (length(nt2$n)==0) {
    0
  } else {
    nt2$n
  }
  return(nt3)
}


##--- AR5 MAP ---####
##---------------####

AR5map <- function(adm.border, landcover, map.title)
{
  plot(adm.border,  main=map.title)    # Plot the administrative border
  
  plot(landcover[landcover$artype==11,], border="lightpink", col="lightpink", add=TRUE)
  plot(landcover[landcover$artype==21,], border="orange", col="orange", add=TRUE)
  plot(landcover[landcover$artype==22,], border="goldenrod", col="goldenrod", add=TRUE)
  plot(landcover[landcover$artype==23,], border="khaki1", col="khaki1", add=TRUE)
  plot(landcover[landcover$artype==30,], border="yellowgreen", col="yellowgreen", add=TRUE)
  plot(landcover[landcover$artype==50,], border="sandybrown", col="sandybrown", add=TRUE)
  plot(landcover[landcover$artype==60,], border="cadetblue1", col="cadetblue1", add=TRUE)
  plot(landcover[landcover$artype==70,], border="cyan", col="cyan", add=TRUE)
  plot(landcover[landcover$artype==81,], border="dodgerblue2", col="dodgerblue2", add=TRUE)
  plot(landcover[landcover$artype==82,], border="dodgerblue3", col="dodgerblue3", add=TRUE)
  plot(landcover[landcover$artype==99,], border="gray", col="gray", add=TRUE)
  plot(landcover[landcover$artype==12,], border="hotpink", col="hotpink", add=TRUE)
  
}

AR5legend <- function(x.placement, y.placement)
{
  legend(x=x.placement, y=y.placement, legend=c("Developed area", "Communications and traffic", "Fully cultivated land (2)",
                                                "Superficially cultivated land (3)", "Home fields grazing land (15)",
                                                "Forest (15)", "Open firm ground (5)", "Marsh (4)", "Snow/glacier",
                                                "Freshwater", "Ocean", "Not registered"),
         fill=c("lightpink", "hotpink", "orange", "goldenrod", "khaki1",
                "yellowgreen", "sandybrown", "cadetblue1", "cyan", "dodgerblue2", "dodgerblue3", "gray"),
         cex=0.6)
}


AR5map.2012 <- function(adm.border, landcover, map.title)
{
  plot(adm.border,  main=map.title)    # Plot the administrative border
  
  plot(landcover[landcover$artype==11,], border="lightpink", col="lightpink", add=TRUE)
  plot(landcover[landcover$artype==21,], border="orange", col="orange", add=TRUE)
  plot(landcover[landcover$artype==22,], border="goldenrod", col="goldenrod", add=TRUE)
  plot(landcover[landcover$artype==23,], border="khaki1", col="khaki1", add=TRUE)
  plot(landcover[landcover$artype==30,], border="yellowgreen", col="yellowgreen", add=TRUE)
  plot(landcover[landcover$artype==50,], border="sandybrown", col="sandybrown", add=TRUE)
  plot(landcover[landcover$artype==60,], border="cadetblue1", col="cadetblue1", add=TRUE)
  plot(landcover[landcover$artype==70,], border="cyan", col="cyan", add=TRUE)
  plot(landcover[landcover$artype==80,], border="dodgerblue2", col="dodgerblue2", add=TRUE)
  plot(landcover[landcover$artype==99,], border="gray", col="gray", add=TRUE)
  plot(landcover[landcover$artype==12,], border="hotpink", col="hotpink", add=TRUE)
  
}

AR5legend.2012 <- function(x.placement, y.placement)
{
  legend(x=x.placement, y=y.placement, legend=c("Developed area", "Communications and traffic", "Fully cultivated land (2)",
                                                "Superficially cultivated land (3)", "Home fields grazing land (15)",
                                                "Forest (15)", "Open firm ground (5)", "Marsh (4)", "Snow/glacier",
                                                "Water", "Not registered"),
         fill=c("lightpink", "hotpink", "orange", "goldenrod", "khaki1",
                "yellowgreen", "sandybrown", "cadetblue1", "cyan", "dodgerblue2", "dodgerblue3", "gray"),
         cex=0.6)
}


##--- READING IN ALL THE est-FILES FROM A ZIPPED FOLDER ---####
##---------------------------------------------------------####
library(rio)
library(dplyr)
library(tibble)
## UNZIPPING THE FILE:
## Get a list of the files within the archive by using "list=TRUE" in the unzip function.
#archive_files_test <- unzip("test.zip", files = "NULL", list = T) 

## Get the occurrence.txt file in as a dataframe (using import from rio)
#test_data <- import(unzip("test.zip",
#                           files="test.csv"))
#rownames(test_data) <- NULL
#test_data <- column_to_rownames(test_data, var="V1")

# To unzip the datafiles, put in the name of the community matrix in the function. This will
# unzip it into the GBIF_2-folder, and load it:
est.all <- import(unzip("Community_matrices.zip",
                        files="Comm_matrix_GBIF_March2018.csv"))
est.reds <- import(unzip("Community_matrices.zip",
                         files="Comm_matrix_GBIF_March2018_redlisted.csv"))
est.blacks <- import(unzip("Community_matrices.zip",
                           files="Comm_matrix_GBIF_March2018_blacklisted.csv"))

# Remove the rownames (this is unfortunately neccessary, as it will not do this otherwise)
rownames(est.all) <- NULL
rownames(est.reds) <- NULL
rownames(est.blacks) <- NULL

# Make the first column (the Pixelnumbers) rownames:
est.all <- column_to_rownames(est.all, var="V1")
est.reds <- column_to_rownames(est.reds, var="V1")
est.blacks <- column_to_rownames(est.blacks, var="V1")





## When you are done using the community matrices in their raw form, remove them from memory:
rm(est.all)
rm(est.all_1km)
rm(est.blacks)
rm(est.blacks_1km)
rm(est.reds)
rm(est.reds_1km)
rm(est_animals)
rm(est_animals_1km)
rm(est_blacks_animals)
rm(est_blacks_animals_1km)
rm(est_blacks_fungi)
rm(est_blacks_fungi_1km)
rm(est_blacks_invert)
rm(est_blacks_invert_1km)
rm(est_blacks_plant)
rm(est_blacks_plant_1km)
rm(est_blacks_vasc)
rm(est_blacks_vasc_1km)
rm(est_blacks_)
rm(est_blacks_vertebrate)
rm(est_blacks_vertebrate_1km)
rm(est_fungi)
rm(est_fungi_1km)
rm(est_invert)
rm(est_invert_1km)
rm(est_plant)
rm(est_plant_1km)
rm(est_reds_animals)
rm(est_reds_animals_1km)
rm(est_reds_fungi)
rm(est_reds_fungi_1km)
rm(est_reds_invert)
rm(est_reds_invert_1km)
rm(est_reds_plant)
rm(est_reds_plant_1km)
rm(est_reds_vasc)
rm(est_reds_vasc_1km)
rm(est_reds_vertebrate)
rm(est_reds_vertebrate_1km)
rm(est_vasc)
rm(est_vasc_1km)
rm(est_vertebrate)
rm(est_vertebrate_1km)


##--- MAP WITH GRAY GRID CELLS AND MARINE/TERRESTRIAL DIVISION ---####
##----------------------------------------------------------------####

DivMap <- function(basemap, mun.border, griddata, title)
{
  plot(basemap[basemap$artype==82,], border="lightblue1", lwd=1, lty=1, col="lightblue1", 
       xlim=c(mun.border@bbox[1,1],mun.border@bbox[1,2]),
       ylim=c(mun.border@bbox[2,1],mun.border@bbox[2,2]), main=title)     # Colouring the marine parts of the municipality
  plot(mun.border, add=TRUE, lty=2)                           # Municipality border
  plot(griddata[!is.na(griddata@data$Communications_traffic),],
       col=rgb(190, 190, 190, alpha = 150, maxColorValue = 255), border=NA, add=TRUE)  # Cells within the grid  
}


##--- THINGS TO REMOVE FOR MORE SPACE IN WORKSPACE ---####
##----------------------------------------------------####
rm(d_melt_AR5)
rm(d_melt_AR5.2012)
rm(d_pivot_AR5)
rm(d_pivot_AR5.2012)
rm(est.all_2012)
rm(est.all_2013)
rm(est.blacks_2012)
rm(est.blacks_2013)
rm(est.reds_2012)
rm(est.reds_2013)
rm(est.sp.blacks_2012)
rm(est.sp.blacks_2013)
rm(est.sp.reds_2012)
rm(est.sp.reds_2013)
rm(est.sp_2012)
rm(est.sp_2013)
rm(mat)
rm(MyData)
rm(rar.blacks_2012)
rm(rar.blacks_2013)
rm(rar.data_2012)
rm(rar.data_2013)
rm(rar.reds_2012)
rm(rar.reds_2013)
rm(snouter.df)
rm(tallied_2012)
rm(tallied_2013)
rm(tallied.blacks_2012)
rm(tallied.blacks_2013)
rm(tallied.reds_2012)
rm(tallied.reds_2013)
rm(Vario1)
rm(Vario1_pp)
rm(Vario1_pp2)
rm(Vario2)
rm(Vario2.2)
rm(wm)
rm(xy)
rm(xy_fm_all)
rm(xy_pp)
rm(blackstable)
rm(blackstable_2012)
rm(cells_analyses)
rm(cells_analyses.2012)
rm(cells_in_trondheim)
rm(cells_in_trondheim_2012)
rm(col.div)
rm(col.heat)
rm(col_all)
rm(col_black)
rm(col_red)
rm(collevels_all)
rm(colours_all_ESR)
rm(colours_all_spec)
rm(colours_black_ESR)
rm(colours_black_pred)
rm(colours_black_spec)
rm(colours_red_ESR)
rm(colours_red_pred)
rm(colours_red_spec)
rm(colramp_all)
rm(correlog1.1)
rm(correlog1.1_pp)
rm(correlog1.2)
rm(correlog1.2_pp)
rm(correlog1.3)
rm(correlog1.3_pp)
rm(correlog1.4_pp)
rm(correlog1_fm)
rm(correlog2.2_fm)
rm(correlog2_fm)
rm(E1)
rm(E1_a)
rm(E1_fm)
rm(E1_pp)
rm(E2)
rm(E2_a)
rm(E2_fm)
rm(E2_pp)
rm(E3.2_pp)
rm(E3_a)
rm(F1)
rm(F1_a)
rm(F1_fm)
rm(F1_pp)
rm(F2)
rm(F2_a)
rm(F2_fm)
rm(F2_pp)
rm(F3.2_pp)
rm(F3_a)
rm(GlobMT1.1)
rm(GlobMT1.1_pp)
rm(GlobMT1.2_fm)
rm(GlobMT1.2_pp)
rm(GlobMT1.3_pp)
rm(GlobMT1.4_pp)
rm(GlobMT1_fm)
rm(GlobMT2.1)
rm(GlobMT2.2)
rm(GlobMT2.2_fm)
rm(GlobMT2_fm)
rm(gls.exp)
rm(gls.exp_2)
rm(gls.exp_2.2)
rm(gls.exp_2_ML)
rm(gls.exp_ML)
rm(gls.exp_pp)
rm(gls.exp_pp2)
rm(gls.gauss)
rm(gls.gauss_2)
rm(gls.gauss_2.2)
rm(gls.gauss_pp)
rm(gls.gauss_pp2)
rm(gls.lin)
rm(gls.lin_2)
rm(gls.lin_2.2)
rm(gls.lin_pp)
rm(gls.lin_pp2)
rm(gls.Ratio)
rm(gls.Ratio_2)
rm(gls.Ratio_2.2)
rm(gls.Ratio_pp)
rm(gls.Ratio_pp2)
rm(gls.spher)
rm(gls.spher_2)
rm(gls.spher_2.2)
rm(gls.spher_pp)
rm(gls.spher_pp2)
rm(i)
rm(j)
rm(M1.2_a)
rm(M1.listw)
rm(M1.nb)
rm(M1_a)
rm(M1_fm.listw_all)
rm(M1_fm.nb_all)
rm(M1_pp.listw)
rm(M1_pp.nb)
rm(M2_a)
rm(M2_pp.listw)
rm(M2_pp.nb)
rm(M3.2_pp)
rm(M3_a)
rm(M3_pp)
rm(M3_pp.listw)
rm(M3_pp.nb)
rm(M4_a)
rm(M4_pp)
rm(M4_pp.listw)
rm(M4_pp.nb)
rm(MC)
rm(MC.2)
rm(MC_fm)
rm(MC_res)
rm(MC_res.2)
rm(MC_res_fm)
rm(my_colours_all)
rm(my_colours_black)
rm(my_colours_red)
rm(my_colours_ESR_all)
rm(my_colours_ESR_black)
rm(my_colours_ESR_red)
rm(my_colours_pre_all)
rm(my_colours_pre_black)
rm(my_colours_pre_red)
rm(my_colours_pre.ESR_all)
rm(my_colours_pre.ESR_black)
rm(my_colours_pre.ESR_red)
rm(my_colours_pre.pred_black)
rm(my_colours_pre.pred_red)
rm(my_colours_pre.rec_all)
rm(my_colours_pre.rec_black)
rm(my_colours_pre.rec_red)
rm(my_colours_pre.spec_all)
rm(my_colours_pre.spec_black)
rm(my_colours_pre.spec_red)
rm(my_colours_pred_black)
rm(my_colours_pred_red)
rm(my_colours_rec_all)
rm(my_colours_rec_black)
rm(my_colours_rec_red)
rm(my_colours_spec_all)
rm(my_colours_spec_black)
rm(my_colours_spec_red)
rm(MyVar)
rm(N)
rm(p)
rm(P1)
rm(pointstable)
rm(pointstable_2012)
rm(r)
rm(RasterBlack)
rm(RasterPoint)
rm(RasterRed)
rm(RasterBlack_2012)
rm(RasterPoint_2012)
rm(RasterRed_2012)
rm(redstable)
rm(redstable_2012)
rm(rwm)
rm(sp.corr)
rm(sp.corr.2)
rm(sp.corr_fm)
rm(sp.corr_res)
rm(sp.corr_res.2)
rm(TrdRast_a)
rm(TrdRast_analyses)
rm(TrdRast_analyses.2012)
rm(TrdRast_pc)
rm(TrdRast_pp)
rm(TrdRast_rel)
rm(TrdRast_rel2)
rm(TrdRast_rel.2012)
rm(TrdRast_rel2.2012)
rm(vario.df_black1)
rm(vario.df_black2)
rm(vario.df_black_pp)
rm(vario.df_red)
rm(vario.df_red_pp)
rm(w)
rm(ww)
rm(y)
rm(y_res)
rm(ybar)
rm(ybar_res)

# Functions from Zuur:
rm(myvif)
rm(panel.cor)
rm(panel.smooth2)
rm(panel.lines2)
rm(panel.hist)
rm(MyVariogram)
rm(MultiVariogram)
rm(Mydotplot)
rm(Mybwplot)
rm(MyxyplotBin)
rm(Myxyplot)
rm(MyxyplotPolygon)
rm(Mypairs)
rm(ShowZeros)
rm(MyDotplot.ggp2)
rm(Myfapply)
rm(MyMultipanel.ggp2)
rm(MyStd)


#### Second round of removals
rm(GlobMT1.2_ns)
rm(GlobMT1_ns)
rm(GlobMT2.2_ns)
rm(GlobMT2_ns)
rm(gls.exp_alien)
rm(gls.exp.b_fm)
rm(gls.exp_blacks)
rm(gls.exp.b_ML_fm)
rm(gls.exp.b_ML_ns)
rm(gls.exp.b_ns)
rm(gls.exp_fm)
rm(gls.exp_fm_blacks)
rm(gls.exp_fm_reds)
rm(gls.exp_ML_fm)
rm(gls.exp_ML_ns)
rm(gls.exp_ML_pp)
rm(gls.exp_ML_pp2)
rm(gls.exp_ns)
rm(gls.exp_reds)
rm(gls.exp_threat)
rm(gls.gauss.b_fm)
rm(gls.gauss_fm)
rm(gls.lin.b_fm)
rm(gls.lin_fm)
rm(gls.Ratio.b_fm)
rm(gls.Ratio_fm)
rm(gls.spher.b_fm)
rm(gls.spher_fm)
rm(M1)
rm(M1.2_fm)
rm(M1.2_ns)
rm(M1_fm)
rm(M1_fm.listw)
rm(M1_fm.nb)
rm(M1_ns)
rm(M1_ns.listw)
rm(M1_ns.nb)
rm(M1_pp)
rm(M1_test)
rm(M2)
rm(M2.2)
rm(M2.2_fm)
rm(M2.2_ns)
rm(M2_fm)
rm(M2_ns)
rm(M2_pp)
rm(MC_ns)
rm(sp.corr_ns)
rm(spdf_m)

# This is just a random line to test the "push" in git