#NEED TO UPDATE THIS TO FIND ANY OBJECT IN ANY FIELD

library(celestial)
library(devtools)
library(Cairo)
library(Rfits)
library(Rwcs)
library(ProFound)
library('magicaxis')
library('data.table')
library('plotrix')
require(foreign)
require(MASS)
#
stub=paste0(getwd(),"/")
swarpstub="180.0_-0.5"

cat("Enter location to save to: ")
loc = readLines(file("stdin"),1)
attempt=paste0(loc,"/")
source(paste0(stub,"/R_files/fastcutout.r"))
asteroids <- read.csv(paste0(stub,attempt,"Possible_Asteroids.csv"))

#Make a directory to save the cutouts
dir.create(paste0(stub,attempt,"Asteroid_Cutouts"))

rdafile=paste0(attempt,"stacked.rds")

g_image= Rfits_read_image(paste0(stub,"Fits_files/g_",swarpstub,".fits"),header=TRUE,ext=1)
r_image= Rfits_read_image(paste0(stub,"Fits_files/r_",swarpstub,".fits"),header=TRUE,ext=1)
Z_image= Rfits_read_image(paste0(stub,"Fits_files/i_",swarpstub,".fits"),header=TRUE,ext=1)
trim=readRDS(paste0(stub,rdafile))

#
for(ID in asteroids$groupID){
  #
  segid=ID
  wid = 200.0
  #segid=1
  #wid=200
  #
  #swarpstub=unlist(strsplit(unlist(strsplit(rdafile,"g_"))[2],".rds"))[1]
  
  cat(stub,segid,paste0(stub,rdafile),paste0("g_",swarpstub,".fits"),"\n")
  ## delete the line below once you've renamed the rds file
  
  #
  galpos=trim$pro_detect$segstats[trim$pro_detect$segstats$segID==segid,c("xmax","ymax")]
  galradec=trim$pro_detect$segstats[trim$pro_detect$segstats$segID==segid,c("RAcen","Deccen")]
  galpos=Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=g_image$keyvalues)
  #
  box=c(2*wid,2*wid)
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_Z=Z_image[galpos,box=box]
  #
  cutseg_orig=magcutoutWCS(trim$pro_detect$segim_orig,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  cutseg_dilate=magcutoutWCS(trim$pro_detect$segim,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  cutseg_dilate=magcutoutWCS(trim$pro_detect$group$groupim,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  
  #
  decoff=2*(wid*0.339/3600.0)
  raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
  galsegIDs=trim$pro_detect$segstats[trim$pro_detect$segstats$RAcen > galradec$RAcen - raoff & trim$pro_detect$segstats$RAcen < galradec$RAcen + raoff & trim$pro_detect$segstats$Deccen > galradec$Deccen - decoff & trim$pro_detect$segstats$Deccen < galradec$Deccen + decoff,"segID"]
  galgroupIDs = trim$pro_detect$segstats[trim$pro_detect$segstats$RAcen > galradec$RAcen - raoff & trim$pro_detect$segstats$RAcen < galradec$RAcen + raoff & trim$pro_detect$segstats$Deccen > galradec$Deccen - decoff & trim$pro_detect$segstats$Deccen < galradec$Deccen + decoff,"groupID"]
  
  #
  mulim=22.0
  kids=(0.339^2)*(10^(0.4*(0-mulim)))
  viking=(0.339^2)*(10^(0.4*(30-mulim)))
  #
  png(filename=paste0(stub,attempt,"Asteroid_Cutouts/G",segid,".png"))
  par(mfrow=c(1,1),mar=c(3,3,2,2))
  #
  magimageWCSRGB(R=cutim_Z$imDat,G=cutim_r$imDat,B=cutim_g$imDat,Rheader=cutim_Z$hdr,Gheader=cutim_r$hdr,Bheader=cutim_g$hdr,xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=c(median(cutim_Z$imDat,na.rm=TRUE),median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE)),hicut=c(kids,kids,kids),type="num",dowarp=FALSE)
  contplot(galsegIDs,cutseg_dilate$image,"purple",wid,2)
  #contplot(segid,cutseg_dilate$image,"deeppink",wid,3)
  contplot(segid, cutgroup_dilate$image, "red", wid, 4)
  #
  text(1,wid*2*0.9,label=paste0("ID=",segid),col="cyan",cex=2.0,pos=4)
  #
  dev.off()
  # 
  # 
  
  
  
}
