# library(celestial)
# library(devtools)
# library(Cairo)
# library(Rfits)
# library(Rwcs)
# library(ProFound)
# library('magicaxis')
# library('data.table')
# library('plotrix')
# require(foreign)
# require(MASS)
# library(ProPane)
# source("./R_files/fastcutout.r")

#
Group_Cutter <- function(loc, images){
  
#Make a directory to save the cutouts
dir.create(paste0("./",loc,"/Group_Cutouts"))
wid = 200.0
  
  
#Read in asteroid data
cat("Reading in asteroid data\n")
asteroids <- as.data.frame(read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv")))


cat("Reading in segmentation map data\n")
segim_orig <- as.matrix(read.csv(paste0("./",loc,"/segim_orig.csv")))

segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))

cat("Generating groupim\n")
groupim <- profoundSegimGroup(segim = segim)

header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))

#trim=readRDS(paste0("./",loc,"/stacked.rds"))
# g_image= Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
# r_image= Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
# Z_image= Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_u_DMAG.fits"),header=TRUE,ext=1)
# r_image=propaneWarp(r_image,keyvalues_out=g_image$keyvalues)
# Z_image=propaneWarp(Z_image,keyvalues_out=g_image$keyvalues)

g_image = images[[1]]
r_image = images[[2]]
Z_image = images[[3]]

cat("Begin iterating through asteroids\n")
for(i in 1:length(asteroids$groupID)){

  ID=asteroids$groupID[i]
  cat("Imaging ", ID,"\n")
  
  galpos=asteroids[asteroids$groupID == ID, c("xmax","ymax")]
  #galpos=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("xmax","ymax")] 
  
  galradec=asteroids[asteroids$groupID == ID, c("RAcen", "Deccen")]
  #galradec=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("RAcen","Deccen")]
  
  galpos=Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=g_image$keyvalues)
  
  box=c(2*wid,2*wid)
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_Z=Z_image[galpos,box=box]
  #
  #cutseg_orig=magcutoutWCS(image = segim_orig, g_image$header , loc=as.numeric(galpos), box=box, loc.type="image")

  #cutseg_dilate=magcutoutWCS(image = segim, g_image$header,loc=as.numeric(galpos),box=box,loc.type="image")
  cat("Here's hoping it doesn't break here );\n")
  cutgroup_dilate=magcutoutWCS(image = groupim$groupim, header, loc=as.numeric(galpos),box=box,loc.type="image")
  #cutgroup_dilate=magcutoutWCS(trim$pro_detect$group$groupim,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  
  
  decoff=2*(wid*0.339/3600.0)
  raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
  #galgroupIDs=trim$pro_detect$groupstats[trim$pro_detect$groupstats$RAcen > galradec$RAcen - raoff & trim$pro_detect$groupstats$RAcen < galradec$RAcen + raoff & trim$pro_detect$groupstats$Deccen > galradec$Deccen - decoff & trim$pro_detect$groupstats$Deccen < galradec$Deccen + decoff,"groupID"]
  
  
  mulim=22.0
  kids=(0.339^2)*(10^(0.4*(0-mulim)))
  viking=(0.339^2)*(10^(0.4*(30-mulim)))
  
  
  if(asteroids[asteroids$groupID == ID, "Colour"] == "g"){
    cat("Printing G",ID," postage stamp\n")
    png(filename=paste0("./",loc,"Group_Cutouts/G",ID,".png"))
  }
  else if(asteroids[asteroids$groupID == ID, "Colour"] == "r"){
    cat("Printing R",ID," postage stamp\n")
    png(filename=paste0("./",loc,"Group_Cutouts/R",ID,".png"))
  }
  else if(asteroids[asteroids$groupID == ID, "Colour"] == "i"){
    cat("Printing I",ID," postage stamp\n")
  png(filename=paste0("./",loc,"Group_Cutouts/I",ID,".png"))
  }
  par(mfrow=c(1,1),mar=c(3,3,2,2))
  
  
  locut = c(median(cutim_Z$imDat,na.rm=TRUE),median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE))
  if(locut[[1]] > kids){
    locut[[1]] = kids
  }
  if(locut[[2]] > kids){
    locut[[2]] = kids
  }
  if(locut[[3]] > kids){
    locut[[3]] = kids
  }
  
  cat("Time to start printing images!\n")
  magimageWCSRGB(R=cutim_r$imDat,G=cutim_g$imDat,B=cutim_Z$imDat, Rheader=cutim_r$header,Gheader=cutim_g$header,Bheader=cutim_Z$header, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num",dowarp=FALSE, hersh = FALSE, grid = TRUE)
  #contplot(galgroupIDs,cutseg_dilate$image,"purple",wid,2)
  
  contplot(galgroupIDs,cutgroup_dilate$image, "skyblue", wid,4)
  #contplot(segID,cutseg_dilate$image,"deeppink",wid,3)?
  
  if(asteroids[asteroids$groupID == ID, "Colour"] == "g"){
    cat("Printing green asteroid. GroupID: ", ID, "\n")
    contplot(groupID, cutgroup_dilate$image, "green", wid, 4)
    text(1,100,label=paste0("ID=",groupID),col="green",cex=2.0)#,pos=4)
  }
  
  else if(asteroids[asteroids$groupID == ID, "Colour"] == "r"){
    cat("Printing red asteroid. GroupID: ", groupID, "\n")
    contplot(groupID, cutgroup_dilate$image, "red", wid, 4)
    text(1,100,label=paste0("ID=",groupID),col="red",cex=2.0)#,pos=4)
  }
  
  else if(asteroids[asteroids$groupID == ID, "Colour"] == "i"){
    cat("Printing blue asteroid. GroupID: ", groupID, "\n")
    contplot(groupID, cutgroup_dilate$image, "blue", wid, 4)
    text(1,100,label=paste0("ID=",groupID),col="blue",cex=2.0)#,pos=4)
  }
  #
  #
  dev.off()
  # 
  # 
  
 


}
}