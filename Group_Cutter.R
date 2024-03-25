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

}
dir.create(paste0("./",loc,"/Group_Cutouts/"))


  
  
#Read in asteroid data
cat("Reading in asteroid data\n")
asteroids <- as.data.frame(read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv")))
#asteroids = as.data.frame(read.csv(paste0("./", loc, "/", loc,"_Filtered_Asteroids.csv")))
asteroids <- cbind(asteroids, data.frame(tl_RA = 0, tl_Dec = 0, tr_RA = 0, tr_Dec = 0, bl_RA = 0, bl_Dec = 0, br_RA = 0, br_Dec = 0))


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
i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"),header=TRUE,ext=1)


g_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
r_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
i_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))


cat("Warping r&i frames\n")
r_image=propaneWarp(r_image_input,keyvalues_out= g_image_header$keyvalues)
i_image=propaneWarp(i_image_input,keyvalues_out= g_image_header$keyvalues)

# g_image = images[[1]]
# r_image = images[[2]]
# i_image = images[[3]]

wid = 200.0
mulim=22.0
kids=(0.339^2)*(10^(0.4*(0-mulim)))
viking=(0.339^2)*(10^(0.4*(30-mulim)))


astcheck = data.frame()
colours = c("green", "red", "blue")
for(i in 1:3){
  co = colours[i]
  #Read in asteroid data
  cat("Reading in MPC Asteroid data\n")
  astcheck = rbind(astcheck, read.table(paste0("./",loc,"/",loc,"_MPC_",co,".txt"), col.names = c("ID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt")))
}



######################################################################
cat("Begin iterating through asteroids\n")
for(i in 1:length(asteroids$groupID)){

  ID=asteroids$groupID[i]
  cat("Imaging ", ID,"\n")
  
  n = length(which(asteroids$groupID == ID))
  
  if(n > 1){
    cat(paste0("Multiple asteroids with ID ", ID, " detected.\n"))
    if(paste0(asteroids$Colour[i],ID,".png") %in% list.files(paste0("./",loc,"/Group_Cutouts/")) == TRUE){
      cat(paste0("Already imaged asteroid ", asteroids$Colour[i], ID, "\n"))
      next
    }else{
      
    #galpos=asteroids[asteroids$groupID == ID, c("xmax","ymax")]
    
    galradec = asteroids[i, c("RAcen", "Deccen")]
    
    if(asteroids[i, "Colour"] == "g"){
      image_header = g_image_header
    }
    if(asteroids[i, "Colour"] == "r"){
      image_header = r_image_header
    }
    if(asteroids[i, "Colour"] == "i"){
      image_header = i_image_header
    }
    
    galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=image_header$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
    
    box=c(2*wid,2*wid)
    cutim_g=g_image[galpos,box=box]
    cutim_r=r_image[galpos,box=box]
    cutim_i=i_image[galpos,box=box]
    
    cutgroup_dilate=magcutout(image = groupim$groupim, header=image_header$header, loc=as.numeric(galpos),box=box,loc.type="image")
    
    decoff=2*(wid*0.339/3600.0)
    raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
    
    cat(paste0("Printing ", asteroids$Colour[i], ID, ".png\n"))
    png(filename=paste0("./",loc,"/Group_Cutouts/",asteroids$Colour[i],ID,".png"))
    
    }
    
  }else{
  #galpos=asteroids[asteroids$groupID == ID, c("xmax","ymax")]
  #galpos=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("xmax","ymax")] 
  
  galradec = asteroids[i, c("RAcen", "Deccen")]
  #galradec=trim$pro_detect$groupstats[trim$pro_detect$groupstats$groupID==groupID, c("RAcen","Deccen")]
  
  if(asteroids[i, "Colour"] == "g"){
    image_header = g_image_header
  }
  if(asteroids[i, "Colour"] == "r"){
    image_header = r_image_header
  }
  if(asteroids[i, "Colour"] == "i"){
    image_header = i_image_header
  }
  
  galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen,Dec=galradec$Deccen,keyvalues=image_header$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  box=c(2*wid,2*wid)
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_i=i_image[galpos,box=box]
  
  cutgroup_dilate=magcutout(image = groupim$groupim, header=image_header$header, loc=as.numeric(galpos),box=box,loc.type="image")
  
  #cutseg_orig=magcutoutWCS(image = segim_orig, g_image_header , loc=as.numeric(galpos), box=box, loc.type="image")
  #cutseg_dilate=magcutoutWCS(image = segim, g_image_header,loc=as.numeric(galpos),box=box,loc.type="image")
  #cutgroup_dilate=groupim$groupim[galpos,box=box]
  #cutgroup_dilate=magcutoutWCS(trim$pro_detect$group$groupim,trim$pro_detect$header,loc=as.numeric(galpos),box=box,loc.type="image")
  
  decoff=2*(wid*0.339/3600.0)
  raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
  
  cat("Printing ",asteroids[i,"Colour"],ID," postage stamp\n")
  png(filename=paste0("./",loc,"/Group_Cutouts/",asteroids[i,"Colour"],ID,".png"))
  }
  
  
  nontargetID=asteroids[asteroids$RAcen > galradec$RAcen - raoff & asteroids$RAcen < galradec$RAcen + raoff & asteroids$Deccen > galradec$Deccen - decoff & asteroids$Deccen < galradec$Deccen + decoff,"groupID"]
  
 
  
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
  # locut = c(kids, kids, kids)
  
  cat("Time to start printing images!\n")
  Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image_header$keyvalues, Gkeyvalues = g_image_header$keyvalues, Bkeyvalues = i_image_header$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
  
  #contplot(nontargetID,cutseg_dilate$image,"purple",wid,2)
  contplot(nontargetID, i=NULL, cutgroup_dilate$image, "skyblue", header = image_header$header)
  
  MPC_Asteroids = astcheck[astcheck$RA >galradec$RAcen - raoff & astcheck$RA < galradec$RAcen + raoff & astcheck$Dec > galradec$Deccen - decoff  & astcheck$Dec < galradec$Deccen + decoff, c("RA","Dec")]
  
  if(length(MPC_Asteroids$RA) != 0){
  MPC_xy = data.table()
  for(k in 1:length(MPC_Asteroids$RA)){
    xy = radec2xy(MPC_Asteroids$RA[k], MPC_Asteroids$Dec[k], header = image_header$keyvalues)
    MPC_xy = rbind(MPC_xy, data.table(x= xy[[1]], y=xy[[2]]))
  }
  MPC_Asteroids = cbind(MPC_Asteroids, MPC_xy)
  
  points(MPC_Asteroids$x, MPC_Asteroids$y, pch = 11, col = rainbow(100))
  }
  
  #contplot(segID,cutseg_dilate$image,"deeppink",wid,3)?
  
  if(length(which(asteroids$groupID == ID)) > 1){
    colour = rainbow(5)
    
    
      if(asteroids$Colour[i] == "g"){
        colour = "green"
      }
      if(asteroids$Colour[i] == "r"){
        colour = "red"
      }
      if(asteroids$Colour[i] == "i"){
        colour = "blue"
      }

      cat("Printing Double Up Asteroid. GroupID: ", ID,"\n\n")
      asteroids = try(contplot(asteroids, i, cutgroup_dilate$image, colour))
      
      cen_xy = radec2xy(asteroids[asteroids$groupID == ID, "RAcen"], asteroids[asteroids$groupID == ID, "Deccen"], header = image_header$header)
      radius = 60/getpixscale(image_header$hdr)
      draw.circle(cen_xy[1], cen_xy[2], radius ,col=c(0,0,0,0.33))
      
      try(text(1,2*wid-50, label=paste0("ID=",asteroids$Colour[i],ID), colour, cex=2.0, pos=4))

  }else{
  if(asteroids[asteroids$groupID == ID, "Colour"] == "g"){
    cat("Printing green asteroid. GroupID: ", ID, "\n")
    asteroids = try(contplot(asteroids, i, cutgroup_dilate$image, "green"))
    
    cen_xy = radec2xy(asteroids[asteroids$groupID == ID, "RAcen"], asteroids[asteroids$groupID == ID, "Deccen"], header = image_header$header)
    radius = 60/getpixscale(image_header$hdr)
    draw.circle(cen_xy[1], cen_xy[2], radius ,col=c(255,0,0,0.33))    
    
    try(text(1,2*wid-50,label=paste0("ID=G",ID),col="green",cex=2.0,pos=4))
  }
  
  if(asteroids[asteroids$groupID == ID, "Colour"] == "r"){
    cat("Printing red asteroid. GroupID: ", ID, "\n")
    asteroids = try(contplot(asteroids, i, cutgroup_dilate$image, "red"))
    
    cen_xy = radec2xy(asteroids[asteroids$groupID == ID, "RAcen"], asteroids[asteroids$groupID == ID, "Deccen"], header = image_header$header)
    radius = 60/getpixscale(image_header$hdr)
    draw.circle(cen_xy[1], cen_xy[2], radius ,col=c(255,0,0,0.33))
    
    try(text(1,2*wid-50,label=paste0("ID=R",ID),col="red",cex=2.0,pos=4))
  }
  
  if(asteroids[asteroids$groupID == ID, "Colour"] == "i"){
    cat("Printing blue asteroid. GroupID: ", ID, "\n")
    asteroids = try(contplot(asteroids, i, cutgroup_dilate$image, "blue"))
    
    cen_xy = radec2xy(asteroids[asteroids$groupID == ID, "RAcen"], asteroids[asteroids$groupID == ID, "Deccen"], header = image_header$header)
    radius = 60/getpixscale(image_header$hdr)
    draw.circle(cen_xy[1], cen_xy[2], radius ,col=c(0,0,255,0.33))
    
    try(text(1,2*wid-50,label=paste0("ID=B",ID),col="blue",cex=1.5,pos=4))
  }}
  
  
  dev.off()


}
cat("Writing out data with top & bottom locations\n")
write.csv(asteroids, file=paste0("./", loc,"/",loc,"_Asteroids.csv"))

}
