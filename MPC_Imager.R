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
library(ProPane)
source("./R_files/fastcutout.r")

#
Group_Cutter <- function(loc){
  `%notin%`<-Negate(`%in%`)
  
  
  #Make a directory to save the cutouts
  if("Group_Cutouts" %in% list.dirs(paste0("./",loc))){
    dir_delete(paste0("./",loc,"/MPC_Images/"))
    
  }
  dir.create(paste0("./",loc,"/MPC_Images/"))
  
  
  
  for(co in c("green","red","blue")){
    
  #Read in asteroid data
  cat("Reading in asteroid data\n")
  astcheck = read.table(paste0("./",loc,"/",loc,"_MPC_",co,".txt"), header = FALSE, na.strings = " ", col.names = c("ID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt"))
  
  cat("Reading in segmentation map data\n")

  segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
  
  cat("Generating groupim\n")
  groupim <- profoundSegimGroup(segim = segim)
  

  cat("Loading images as pointers\n")
  g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
  r_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
  i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"),header=TRUE,ext=1)
  
  cat("Warping r&i frames\n")
  r_image=propaneWarp(r_image_input,keyvalues_out= r_image_input$keyvalues)
  i_image=propaneWarp(i_image_input,keyvalues_out= i_image_input$keyvalues)
  
  wid = 200.0
  mulim=22.0
  kids=(0.339^2)*(10^(0.4*(0-mulim)))
  viking=(0.339^2)*(10^(0.4*(30-mulim)))
  
  ######################################################################
  cat("Begin iterating through astcheck\n")
  for(i in 1:length(astcheck$ID)){
    
    ID=astcheck$ID[i]
    cat("Imaging", ID,"\n")
      
    galradec = astcheck[i, c("RA", "Dec")]

    galpos=as.integer(Rwcs_s2p(RA=galradec$RA,Dec=galradec$Dec, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))

    box=c(2*wid,2*wid)
    cutim_g=g_image[galpos,box=box]
    cutim_r=r_image[galpos,box=box]
    cutim_i=i_image[galpos,box=box]
    
    cutgroup_dilate=magcutout(image = groupim$groupim, header=g_image$header, loc=as.numeric(galpos),box=box,loc.type="image")
    
    decoff=2*(wid*0.339/3600.0)
    raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
    
    groupID=astcheck[astcheck$RAcen > galradec$RAcen - raoff & astcheck$RAcen < galradec$RAcen + raoff & astcheck$Deccen > galradec$Deccen - decoff & astcheck$Deccen < galradec$Deccen + decoff,"groupID"]
    
    cat("Printing ",ID," postage stamp\n")
    png(filename=paste0("./",loc,"/MPC_Images/",ID,".png"))
      

    par(mfrow=c(1,1),mar=c(3,3,2,2))
    locut = c(kids, kids, kids)
    
    cat("Time to start printing images!\n")
    Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues, Bkeyvalues = i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
    
    #contplot(groupID,cutseg_dilate$image,"purple",wid,2)
    #contplot(groupID, i=NULL, cutgroup_dilate$image, "skyblue", header = image_header)
    groupimage = cutgroup_dilate$image
    groupIDs = which(groupimage != 0)
    
    for(groupID in groupimage){
      
      xrun=1:(dim(groupimage)[1]-1)
      yrun=1:(dim(groupimage)[2]-1)
      
      groupimage_lb=groupimage[xrun,yrun]
      groupimage_lt=groupimage[xrun+1,yrun]
      groupimage_rt=groupimage[xrun+1,yrun+1]
      groupimage_rb=groupimage[xrun,yrun+1]
      
      groupimage_temp = (groupimage_lb == groupimage_lt) & (groupimage_rt == groupimage_rb) & (groupimage_lb == groupimage_rb) & (groupimage_lt == groupimage_rt)
      
      groupimage_edge=matrix(0,dim(groupimage)[1],dim(groupimage)[2])
      
      groupimage_edge[xrun,yrun]=groupimage_edge[xrun,yrun]+groupimage_temp
      groupimage_edge[xrun+1,yrun]=groupimage_edge[xrun+1,yrun]+groupimage_temp
      groupimage_edge[xrun+1,yrun+1]=groupimage_edge[xrun+1,yrun+1]+groupimage_temp
      groupimage_edge[xrun,yrun+1]=groupimage_edge[xrun,yrun+1]+groupimage_temp
      
      groupimage[groupimage_edge==4]=0
      
      magimage(groupimage,col=c(NA,rep("skyblue",max(groupimage))),magmap=FALSE,add=TRUE,sparse=1, lwd = 0.5)
      
    }
    

    pix_loc = radec2xy(astcheck$RA, astcheck$Dec, header = g_image$header, col = go)
    points(x = pix_loc[[1]], y = pix_loc[[2]], add = TRUE, col = co, pch = 11)
    text(1,2*wid-50, label=paste0("MPC ID=",ID), col = co, cex=2.0, pos=4, add = TRUE)
      
    dev.off()
  }
  }
}
  
