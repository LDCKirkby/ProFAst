#' MultiDetect
#' @description Main source extraction function. Uses profoundMultiBand with default input parameters set for improved asteroid detection. Default values have been trained based on VST KiDS data, it is recommended that you adjust slightly for your data to get the best results
#' @param RA_DEC Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param frames Vector; Vector containing three pixel-matched astronomical images. Expected order within list is (g,r,i). Output of \code{\link{ProFAst::Pre_Proc}}.
#' @param savepassthru Logical; should intermediate files be saved to directory? Can greatly increase size on disk but useful to see which objects are being filtered out.
#' @param skycut Numeric scalar; the lowest threshold to make on the image in units of the skyRMS. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param pixcut Integer scalar; the number of pixels required to identify an object. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param smooth Logical; should smoothing be done on the target image? Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param sigma Numeric scalar; standard deviation of the blur used when smooth=TRUE. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param reltol Numeric scalar; only relevant for watershed='ProFound'. A modifier to the tolerance, modifying it by the ratio of the segment peak flux divided by the saddle point flux to the power reltol. The default means the reltol has no effect since this modifier becomes 1. A larger value of reltol means segments are more aggressively merged together. Can be (and often should be in practice) negative. The effect of using reltol and setting to negative is that the central brighter parts of galaxies are kept together in a single segment, and deblending is more common on the outskirts (where it should have less effect on the overall flux). The principle is that we need to be very confident a bright source needs to be split near its peak flux, but can be more aggressive in the outskirts. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param tolerance Numeric scalar; the minimum height of the object in the units of skyRMS between its highest point (seed) and the point where it contacts another object (checked for every contact pixel). If the height is smaller than the tolerance, the object will be combined with one of its neighbours, which is the highest. The range 1-5 offers decent results usually. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param ext Numeric scalar; radius of the neighbourhood in pixels for the detection of neighbouring objects. Higher value smooths out small objects. Passed to \code{\link{ProFound::profoundMultiBand}}.
#' @return Data frame containing multi-band photometry data.
#' 
#' @export
#'
MultiDetect <- function(RA_DEC, frames, skycut = 0.6, pixcut = 15, smooth = TRUE, sigma = 2, reltol=-10, tolerance =1, ext=7, savepassthru=FALSE){

savelocation = paste0("./",RA_DEC,"/")

cat("***********\n")
cat("Beginning detection\n")
cat("***********\n")

multi_data=ProFound::profoundMultiBand(
  inputlist = frames,
  skycut=skycut,
  pixcut=pixcut,
  ext=ext,
  tolerance=tolerance,
  reltol=reltol,
  smooth = smooth,
  cliptol=100,
  detectbands=c("g","rx","i1x"),
  multibands=c("g","rx","i1x"),
  keepsegims=TRUE,
  magzero=c(0,0,0),
  dotot=TRUE,
  docol=TRUE,
  dogrp=TRUE,
  verbose=TRUE,
  boxiters=4,
  grid=c(50,50,50),
  roughpedestal=TRUE,
  stats=FALSE,
  groupstats=TRUE,
  mask=0,
  sigma = sigma,
  fluxtype='Jansky',
)

cat("***********\n")
cat("Detection finished\n")
cat("***********\n")

segimlist = multi_data$segimlist
segim = multi_data$pro_detect$segim
segim_orig = multi_data$pro_detect$segim_orig

#NEEDED TO EXTRACT GROUP MASK DATA
#THAT DATA IS FOUND IN multi_data$pro_detect$group$groupsegID (FOR NPIX)
#GROUP PHOTOMETRY DATA IS FOUND IN multi_data$pro_detect$groupstats (FOR RAcen, Deccen, etc.)
group_data = data.table::as.data.table(cbind(multi_data$pro_detect$group$groupsegID,multi_data$pro_detect$groupstats, multi_data$cat_grp))

# Extract segment info, colour, total, deblend, aperture, and groups measurements
objectcat <- data.table::as.data.table(cbind(multi_data$pro_detect$segstats,multi_data$cat_tot))

cat_groupinfo=cbind(segID=unlist(multi_data$pro_detect$group$groupsegID$segID),groupID=rep(multi_data$pro_detect$group$groupsegID$groupID,times=multi_data$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(multi_data$pro_detect$group$groupsegID$Ngroup, times=multi_data$pro_detect$group$groupsegID$Ngroup))

objectcat=cbind(objectcat,cat_groupinfo[match(objectcat$segID, cat_groupinfo[,"segID"]),2:3])

groupcat <- data.table::as.data.table(cbind(multi_data$pro_detect$group$groupsegID$Ngroup,multi_data$pro_detect$groupstats$groupID,multi_data$pro_prodetect$groupstats$Npix, multi_data$cat_grp))

names(groupcat)[1] <- "Ngroup"
names(groupcat)[2] <- "groupID"

group_matches=match(objectcat$segID,groupcat$groupID,nomatch=NA)

allcat=as.data.table(cbind(objectcat,groupcat[group_matches,]))

if(savepassthru == TRUE){
# Save data structure and produce diagnostic plot
dir.create(savelocation)
saveRDS(multi_data,file=paste0(savelocation,"stacked.rds"))

#Saves all segmentation mask images in a list (segimlist)
#Not needed & will likely be removed
cat("Saving slimmed segimlist\n")
write.csv(segimlist, paste0(savelocation,"segimlist.csv"), row.names=FALSE)

#Save segmentation maps (dilated and converged)
cat("Saving slimmed segim\n")
write.csv(segim, paste0(savelocation,"segim.csv"), row.names=FALSE)

#Save colour segmentation maps (pre dilation)
cat("Saving slimmed segim_orig\n")
write.csv(segim_orig, paste0(savelocation,"segim_orig.csv"), row.names=FALSE)

# rm(segim)
# rm(segim_orig)
# rm(segimlist)
write.csv(objectcat,file=paste0(savelocation,"objectcat.csv"), row.names=FALSE)
write.csv(groupcat,file=paste0(savelocation,"groupcat.csv"), row.names=FALSE)
write.csv(allcat,file=paste0(savelocation,"allcat.csv"), row.names=FALSE)
}

return(c())
}