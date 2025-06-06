#############
# LIBRARIES #
#############
# library(celestial)
# library(devtools)
# library(Cairo)
# library(ProFound)
# library(magicaxis)
# library(data.table)
# library(foreign)
# library(MASS)
# library(dst)
# library(Rwcs)
# library(ProPane)
# library(Rfits)


#######################
# Run ProFound Script #
#######################
#Important variables for detection
# skycut = 0.6
# pixcut = 15
# smooth = TRUE
# sigma = 2
# reltol=-10
# tolerance =1
# ext=7
#######################


Multi_Detect <- function(loc, frames){

#Set working directory and detection parameters
savelocation = paste0("./",loc,"/")

cat("***********\n")
cat("Beginning detection\n")
cat("***********\n")

multi_data=profoundMultiBand( #I prefer this layout for complex calls since then you can write notes to remind ourselves why we have certain settings.
  inputlist = frames,
  skycut=0.6,
  pixcut=15, #Avoids too many detections in very noisy regions etc
  ext=7,
  tolerance=1,
  reltol=-10,
  smooth = TRUE,
  cliptol=100, #Recombines bright stars effectively
  detectbands=c("g","rx","i1x"), # will build a detection band by adding g+r+i
  multibands=c("g","rx","i1x"), #Required since detectbands must be a subset on multibands, even if we are not doing multiband photometry
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
  sigma = 2,
  fluxtype='Jansky',
)

cat("***********\n")
cat("Detection finished\n")
cat("***********\n")

# Save data structure and produce diagnostic plot
dir.create(savelocation)
saveRDS(multi_data,file=paste0(savelocation,"stacked.rds"))

segimlist = multi_data$segimlist
segim = multi_data$pro_detect$segim
segim_orig = multi_data$pro_detect$segim_orig

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

rm(segim)
rm(segim_orig)
rm(segimlist)

#NEEDED TO EXTRACT GROUP MASK DATA
#THAT DATA IS FOUND IN multi_data$pro_detect$group$groupsegID (FOR NPIX)
#GROUP PHOTOMETRY DATA IS FOUND IN multi_data$pro_detect$groupstats (FOR RAcen, Deccen, etc.)
group_data = as.data.table(cbind(multi_data$pro_detect$group$groupsegID,multi_data$pro_detect$groupstats, multi_data$cat_grp))

# Extract segment info, colour, total, deblend, aperture, and groups measurements
cat_objects <- as.data.table(cbind(multi_data$pro_detect$segstats,multi_data$cat_tot))

cat_groupinfo=cbind(segID=unlist(multi_data$pro_detect$group$groupsegID$segID),groupID=rep(multi_data$pro_detect$group$groupsegID$groupID,times=multi_data$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(multi_data$pro_detect$group$groupsegID$Ngroup, times=multi_data$pro_detect$group$groupsegID$Ngroup))

cat_objects=cbind(cat_objects,cat_groupinfo[match(cat_objects$segID, cat_groupinfo[,"segID"]),2:3])

cat_groups <- as.data.table(cbind(multi_data$pro_detect$group$groupsegID$Ngroup,multi_data$pro_detect$groupstats$groupID,multi_data$pro_prodetect$groupstats$Npix, multi_data$cat_grp))

names(cat_groups)[1] <- "Ngroup"
names(cat_groups)[2] <- "groupID"

group_matches=match(cat_objects$segID,cat_groups$groupID,nomatch=NA)

datafile0=as.data.table(cbind(cat_objects,cat_groups[group_matches,]))

write.csv(cat_objects,file=paste0(savelocation,"objectcati.csv"), row.names=FALSE)
write.csv(cat_groups,file=paste0(savelocation,"groupcati.csv"), row.names=FALSE)
write.csv(datafile0,file=paste0(savelocation,"allcati.csv"), row.names=FALSE)


#par(mfrow=c(1,1),mar=c(3,3,2,2))

#CairoPDF(file=paste0(savelocation,"test.pdf"),width=24.0,height=24.0)
#plot(multi_data$pro_detect)
#dev.off()


#Print out input args for verification
# input_args = c(paste0("Pixcut:",pixcut),paste0("Skycut:",skycut),paste0("Smooth:",smooth),paste0("Sigma:",sigma),paste0("Tolerance:",tolerance), paste0("Relative Tolerance:", reltol),paste0("ext:",ext))
# write.csv(input_args, file = paste0(savelocation,"/Input_Args.csv"))


}