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
Group_Cutter <- function(loc){#, images){
`%notin%`<-Negate(`%in%`)
  
  
#Make a directory to save the cutouts
if("Group_Cutouts" %in% list.dirs(paste0("./",loc))){
  dir_delete(paste0("./",loc,"/Group_Cutouts/"))
  dir_delete(paste0("./",loc,"/Full_Group_Cutouts/"))
  dir_delete(paste0("./",loc,"/N100_Group_Cutouts/"))
}
dir.create(paste0("./",loc,"/Full_Group_Cutouts/"))
dir.create(paste0("./",loc,"/N100_Group_Cutouts/"))


  
  
#Read in asteroid data
cat("Reading in asteroid data\n")
asteroids <- as.data.frame(read.csv(paste0("./",loc,"/",loc,"_Filtered_Asteroids.csv")))
asteroids <- cbind(asteroids, data.frame(top_left = 0, top_right = 0, bottom_left = 0, bottom_right = 0))
asteroids_N100 <- as.data.frame(read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv")))
asteroids_N100 <- cbind(asteroids_N100, data.frame(top_left = 0, top_right = 0, bottom_left = 0, bottom_right = 0))

data = list(asteroids,asteroids_N100)

cat("Reading in segmentation map data\n")
#segim_orig <- as.matrix(read.csv(paste0("./",loc,"/segim_orig.csv")))

segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))

cat("Generating groupim\n")
groupim <- profoundSegimGroup(segim = segim)

#header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))

#trim=readRDS(paste0("./",loc,"/stacked.rds"))
cat("Loading images as pointers\n")
g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
r_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_u_DMAG.fits"),header=TRUE,ext=1)

cat("Warping r&i frames\n")
r_image=propaneWarp(r_image_input,keyvalues_out=g_image$keyvalues)
i_image=propaneWarp(i_image_input,keyvalues_out=g_image$keyvalues)

# g_image = images[[1]]
# r_image = images[[2]]
# i_image = images[[3]]

wid = 200.0
mulim=22.0
kids=(0.339^2)*(10^(0.4*(0-mulim)))
viking=(0.339^2)*(10^(0.4*(30-mulim)))

for(num in 1:length(data)){
  filt = data[[num]]
  if(num = 1){
    location = "Full_Group_Cutouts"
  }else{
    location = "N100_Group_Cutouts"
  }
######################################################################
cat("Begin iterating through asteroids\n")
for(i in 1:length(filt$groupID)){

  ID=filt$groupID[i]
  cat("Imaging ", ID,"\n")
  
  n = length(which(filt$groupID == ID))
  
  if(n > 1){
    cat(paste0("Multiple asteroids with ID ", ID, " detected.\n"))
    if(paste0(filt$Colour[i],ID,".png") %in% list.files(paste0("./",loc,"/",location,"/")) == TRUE){
      cat(paste0("Already imaged asteroid ", filt$Colour[i], ID, "\n"))
      next
    }else{
      
    galpos=filt[filt$groupID == ID, c("xmax","ymax")]
    
    galradec=filt[filt$groupID == ID, c("RAcen", "Deccen")]
    
    galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
    
    box=c(2*wid,2*wid)
    cutim_g=g_image[galpos,box=box]
    cutim_r=r_image[galpos,box=box]
    cutim_i=i_image[galpos,box=box]
    
    cutgroup_dilate=magcutout(image = groupim$groupim, header=g_image$header, loc=as.numeric(galpos),box=box,loc.type="image")
    
    decoff=2*(wid*0.339/3600.0)
    raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
    
    cat(paste0("Printing ", filt$Colour[i], ID, ".png\n"))
    png(filename=paste0("./",loc,"/",location,"/",filt$Colour[i],ID,".png"))
    
    }
    
  }else{
  galpos=filt[filt$groupID == ID, c("xmax","ymax")]
  #galpos=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("xmax","ymax")] 
  
  galradec=filt[filt$groupID == ID, c("RAcen", "Deccen")]
  #galradec=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("RAcen","Deccen")]
  
  galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  box=c(2*wid,2*wid)
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_i=i_image[galpos,box=box]
  
  cutgroup_dilate=magcutout(image = groupim$groupim, header=g_image$header, loc=as.numeric(galpos),box=box,loc.type="image")
  
  #cutseg_orig=magcutoutWCS(image = segim_orig, g_image$header , loc=as.numeric(galpos), box=box, loc.type="image")
  #cutseg_dilate=magcutoutWCS(image = segim, g_image$header,loc=as.numeric(galpos),box=box,loc.type="image")
  #cutgroup_dilate=groupim$groupim[galpos,box=box]
  #cutgroup_dilate=magcutoutWCS(trim$pro_detect$group$groupim,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  
  decoff=2*(wid*0.339/3600.0)
  raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
  
  #galgroupIDs=trim$pro_detect$groupstats[trim$pro_detect$groupstats$RAcen > galradec$RAcen - raoff & trim$pro_detect$groupstats$RAcen < galradec$RAcen + raoff & trim$pro_detect$groupstats$Deccen > galradec$Deccen - decoff & trim$pro_detect$groupstats$Deccen < galradec$Deccen + decoff,"groupID"]
  
  if(filt[filt$groupID == ID, "Colour"] == "g"){
    cat("Printing g",ID," postage stamp\n")
    png(filename=paste0("./",loc,"/",location,"/g",ID,".png"))
    
  }
  if(filt[filt$groupID == ID, "Colour"] == "r"){
    cat("Printing r",ID," postage stamp\n")
    png(filename=paste0("./",loc,"/",location,"/r",ID,".png"))
    
  }
  if(filt[filt$groupID == ID, "Colour"] == "i"){
    cat("Printing i",ID," postage stamp\n")
    png(filename=paste0("./",loc,"/",location,"/i",ID,".png"))
  
  }
  
  }
  par(mfrow=c(1,1),mar=c(3,3,2,2))
  
  
  locut = c(median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE),median(cutim_i$imDat,na.rm=TRUE))
  if(locut[[1]] > kids){
    locut[[1]] = kids
  }
  if(locut[[2]] > kids){
    locut[[2]] = kids
  }
  if(locut[[3]] > kids){
    locut[[3]] = kids
  }
  #locut = c(kids, kids, kids)
  
  cat("Time to start printing images!\n")
  Rwcs_imageRGB(R=cutim_r,G=cutim_g,B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues,Bkeyvalues = i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
  
  #contplot(galgroupIDs,cutseg_dilate$image,"purple",wid,2)
  #contplot(galgroupIDs,cutgroup_dilate$image, "skyblue", target = FALSE)
  #contplot(segID,cutseg_dilate$image,"deeppink",wid,3)?
  
  if(length(which(filt$groupID == ID)) > 1){
    colour = rainbow(5)
    
    
      if(filt$Colour[i] == "g"){
        colour = "green"
      }
      if(filt$Colour[i] == "r"){
        colour = "red"
      }
      if(filt$Colour[i] == "i"){
        colour = "blue"
      }

      cat("Printing Double Up Asteroid. GroupID: ", ID,"\n\n")
      contplot(filt, i, cutgroup_dilate$image, colour, target = TRUE)
      text(1,2*wid-50, label=paste0("ID=",filt$Colour[i],ID), colour, cex=2.0, pos=4)

  }else{
  if(filt[filt$groupID == ID, "Colour"] == "g"){
    cat("Printing green asteroid. GroupID: ", ID, "\n\n")
    contplot(filt, i, cutgroup_dilate$image, "green", target = TRUE)
    text(1,2*wid-50,label=paste0("ID=G",ID),col="green",cex=2.0,pos=4)
  }
  
  if(filt[filt$groupID == ID, "Colour"] == "r"){
    cat("Printing red asteroid. GroupID: ", ID, "\n\n")
    contplot(filt, i, cutgroup_dilate$image, "red", target = TRUE)
    text(1,2*wid-50,label=paste0("ID=R",ID),col="red",cex=2.0,pos=4)
  }
  
  if(filt[filt$groupID == ID, "Colour"] == "i"){
    cat("Printing blue asteroid. GroupID: ", ID, "\n\n")
    contplot(filt, i, cutgroup_dilate$image, "blue", target =TRUE)
    text(1,2*wid-50,label=paste0("ID=B",ID),col="blue",cex=1.5,pos=4)
  }}

  dev.off()


}
}
}