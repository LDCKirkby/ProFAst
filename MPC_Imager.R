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
bruh = commandArgs()
num_args = length(bruh)
i = bruh[[num_args]]



Group_Cutter <- function(loc){
  `%notin%`<-Negate(`%in%`)
  
  #Make a directory to save the cutouts
  if("Group_Cutouts" %in% list.dirs(paste0("./",loc))){
    dir_delete(paste0("./",loc,"/MPC_Images/"))
  }
  dir.create(paste0("./",loc,"/MPC_Images/"))
  
  cat("Reading in segmentation map data\n")
  
  segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
  
  cat("Generating groupim\n")
  groupim <- profoundSegimGroup(segim = segim)
  
  cat("Loading images as pointers\n")
  g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
  r_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
  i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"),header=TRUE,ext=1)
  
  # g_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
  # r_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
  # i_image_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
  # 
  cat("Warping r&i frames\n")
  # r_image=propaneWarp(r_image_input,keyvalues_out= g_image$keyvalues)
  # i_image=propaneWarp(i_image_input,keyvalues_out= g_image$keyvalues)
  
  wid = 200.0
  mulim=22.0
  kids=(0.339^2)*(10^(0.4*(0-mulim)))
  viking=(0.339^2)*(10^(0.4*(30-mulim)))
  
  
  colours = c("green", "red", "blue")
  headers = c(g_image$header, r_image$header, i_image$header)
  image = c(g_image, r_image, i_image)
  keyvaluess = c(g_image$keyvalues, r_image$keyvalues, i_image$keyvalues)
  for(k in 1:3){
    co = colours[k]
    header = headers[k]
    keyvalues = keyvaluess[k]
    #Read in asteroid data
    cat("Reading in asteroid data\n")
    astcheck = read.table(paste0("./",loc,"/",loc,"_MPC_",co,".txt"), col.names = c("ID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt"))
  

  ######################################################################
  cat("Begin iterating through astcheck\n")
  for(i in 1:length(astcheck$ID)){
    
    ID=astcheck$ID[i]
    cat("Imaging", ID,"\n")
      
    galradec = astcheck[i, c("RA", "Dec")]

    galpos=as.integer(Rwcs_s2p(RA=galradec$RA,Dec=galradec$Dec, keyvalues=header$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))

    box=c(2*wid,2*wid)
    cutim_g=g_image[galpos,box=box]
    cutim_r=r_image[galpos,box=box]
    cutim_i=i_image[galpos,box=box]
    
    cutgroup_dilate=magcutout(image = groupim$groupim, header=header$header, loc=as.numeric(galpos),box=box,loc.type="image")
    
    decoff=2*(wid*0.339/3600.0)
    raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
    
    groupimage = cutgroup_dilate$image
    groupIDs = which(cutgroup_dilate$image != 0)
    #groupID=astcheck[astcheck$RA > galradec$RAcen - raoff & astcheck$RA < galradec$RAcen + raoff & astcheck$Dec > galradec$Deccen - decoff & astcheck$Deccen < galradec$Deccen + decoff,"groupID"]
    
    cat("Printing ",ID," postage stamp\n")
    png(filename=paste0("./",loc,"/MPC_Images/",ID,".png"))
      

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
    Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues, Bkeyvalues = i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
    
    #contplot(groupID, i=NULL, cutgroup_dilate$image, "skyblue", header = image_header)
    
    # for(groupID in groupimage){
    #   
    #   xrun=1:(dim(groupimage)[1]-1)
    #   yrun=1:(dim(groupimage)[2]-1)
    #   
    #   groupimage_lb=groupimage[xrun,yrun]
    #   groupimage_lt=groupimage[xrun+1,yrun]
    #   groupimage_rt=groupimage[xrun+1,yrun+1]
    #   groupimage_rb=groupimage[xrun,yrun+1]
    #   
    #   groupimage_temp = (groupimage_lb == groupimage_lt) & (groupimage_rt == groupimage_rb) & (groupimage_lb == groupimage_rb) & (groupimage_lt == groupimage_rt)
    #   
    #   groupimage_edge=matrix(0,dim(groupimage)[1],dim(groupimage)[2])
    #   
    #   groupimage_edge[xrun,yrun]=groupimage_edge[xrun,yrun]+groupimage_temp
    #   groupimage_edge[xrun+1,yrun]=groupimage_edge[xrun+1,yrun]+groupimage_temp
    #   groupimage_edge[xrun+1,yrun+1]=groupimage_edge[xrun+1,yrun+1]+groupimage_temp
    #   groupimage_edge[xrun,yrun+1]=groupimage_edge[xrun,yrun+1]+groupimage_temp
    #   
    #   groupimage[groupimage_edge==4]=0
    #   
    #   magimage(groupimage,col=c(NA,rep("skyblue",max(groupimage))),magmap=FALSE,add=TRUE,sparse=1, lwd = 0.5)
    #   
    # }
    

    pix_loc = radec2xy(astcheck$RA, astcheck$Dec, header = header$header)
    points(pix_loc, add = TRUE, col = co, pch = 11)
    text(1,2*wid-50, label=paste0("MPC ID=",ID), col = co, cex=2.0, pos=4, add = TRUE)
      
    dev.off()
  }
  }
}

Group_Cutter(i)

