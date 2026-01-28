#' MultiDetect
#' @description${1:Main source extraction function. Uses profoundMultiBand with default input parameters set for improved asteroid detection. Default values have been trained based on VST KiDS data, it is recommended that you adjust slightly for your data to get the best results}
#' @inheritParams ProFAst::ProFAst
#' 
#' @return Data frame containing multi-band photometry data.
#' @export
#'
MultiDetect <- function(RA_DEC, frames, skycut = 0.6, pixcut = 15, smooth = TRUE, sigma = 2, reltol=-10, tolerance =1, ext=7, savepassthru=FALSE){

savelocation = paste0("./",RA_DEC,"/")

cat("***********\n")
cat("Beginning detection\n")
cat("***********\n")

multi_data=profoundMultiBand(
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
group_data = as.data.table(cbind(multi_data$pro_detect$group$groupsegID,multi_data$pro_detect$groupstats, multi_data$cat_grp))

# Extract segment info, colour, total, deblend, aperture, and groups measurements
objectcat <- as.data.table(cbind(multi_data$pro_detect$segstats,multi_data$cat_tot))

cat_groupinfo=cbind(segID=unlist(multi_data$pro_detect$group$groupsegID$segID),groupID=rep(multi_data$pro_detect$group$groupsegID$groupID,times=multi_data$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(multi_data$pro_detect$group$groupsegID$Ngroup, times=multi_data$pro_detect$group$groupsegID$Ngroup))

objectcat=cbind(objectcat,cat_groupinfo[match(objectcat$segID, cat_groupinfo[,"segID"]),2:3])

groupcat <- as.data.table(cbind(multi_data$pro_detect$group$groupsegID$Ngroup,multi_data$pro_detect$groupstats$groupID,multi_data$pro_prodetect$groupstats$Npix, multi_data$cat_grp))

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
write.csv(segim, paste0(savelocation,"/segim.csv"), row.names=FALSE)

#Save colour segmentation maps (pre dilation)
cat("Saving slimmed segim_orig\n")
write.csv(segim_orig, paste0(savelocation,"/segim_orig.csv"), row.names=FALSE)

# rm(segim)
# rm(segim_orig)
# rm(segimlist)


write.csv(objectcat,file=paste0(savelocation,"objectcat.csv"), row.names=FALSE)
write.csv(groupcat,file=paste0(savelocation,"groupcat.csv"), row.names=FALSE)
write.csv(allcat,file=paste0(savelocation,"allcat.csv"), row.names=FALSE)
}

return(c())
}