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
library(gsubfn)
library(fs)
# source("./R_files/fastcutout.r")

#
args = commandArgs(trailingOnly=TRUE)
loc = args[[1]]

wid <- 200.0
box<-c(2*wid,2*wid)
mulim<-22.0
kids<-(0.339^2)*(10^(0.4*(0-mulim)))
viking<-(0.339^2)*(10^(0.4*(30-mulim)))

Group_Cutter <- function(loc, images){
`%notin%`<-Negate(`%in%`)

#Make a directory to save the cutouts
if(dir_exists(paste0("./",loc,"Group_Cutouts"))){
  cat("Group_Cutouts already exists\n")
  dir_delete(paste0("./",loc,"/Group_Cutouts/"))
}
dir.create(paste0("./",loc,"/Group_Cutouts/"))

cat("Reading in asteroid data\n")
asteroids = read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
asteroids <- cbind(asteroids, data.frame(tl_RA = 0, tl_Dec = 0, tr_RA = 0, tr_Dec = 0, bl_RA = 0, bl_Dec = 0, br_RA = 0, br_Dec = 0, top_RA = 0, top_Dec = 0, bot_RA = 0, bot_Dec = 0))

# if(missing(images)){
#   cat("Images not supplied")
#   list[groupim, g_image, r_image, i_image, g_hdr, r_hdr, i_hdr] = Data_Reader(loc)
# }else{
#   list[groupim,g_image,r_image,i_image,g_hdr,r_hdr,i_hdr] = Data_Reader(loc,images)
# }

Data_Reader(loc)

cat(length(asteroids$groupID), " asteroids to image\n")
for(ID in asteroids$groupID){
  #Makes sure we don't image the same object twice
  done = list.files(path = paste0("./",loc,"/Group_Cutouts/"))
  for(file in done){
    if(grepl(ID, file) == TRUE){
      next
    }
  }
  
  i = which(asteroids$groupID == ID)
  colour = asteroids[asteroids$groupID == ID, "Colour"]
  cat(ID,i,colour,"\n")
  if(grepl(colour,"g") == TRUE){
    image_header = g_image$header
    keyvalues = g_image$keyvalues
    hdr = g_hdr
    paint = "green"
    Cutout(asteroids, ID, colour, loc, keyvalues)
    Edge_Finder(ID, colour, groupcut)      
    cat("Printing image of ", colour, ID, "\n")
    Image_Maker(ID, colour, groupcut, locations)
  }else if(grepl(colour,"r") == TRUE){
    image_header = r_image$header
    keyvalues = r_image$keyvalues
    hdr = r_hdr
    paint = "red"
    Cutout(asteroids, ID, colour, loc, keyvalues)
    Edge_Finder(ID, colour, groupcut)
    cat("Printing image of ", colour, ID, "\n")
    Image_Maker(ID, colour, groupcut, locations)
  }else if(grepl(colour,"i") == TRUE){
    image_header = i_image$header
    keyvalues = i_image$keyvalues
    hdr = i_hdr
    paint = "blue"
    Cutout(asteroids, ID, colour, loc, keyvalues)
    Edge_Finder(ID, colour, groupcut)
    cat("Printing image of ", colour, ID, "\n")
    Image_Maker(ID, colour, groupcut, locations)
  }
}
    cat("Writing out data with top & bottom locations\n")
    write.csv(asteroids, file=paste0("./", loc,"/",loc,"_Asteroids.csv"))
}

Data_Reader <- function(loc, images){
  #cat("Reading in asteroid data\n")
  #asteroids = as.data.frame(read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv")))
  #asteroids = as.data.frame(read.csv(paste0("./", loc, "/", loc,"_Filtered_Asteroids.csv")))
  #asteroids <- cbind(asteroids, data.frame(tl_RA = 0, tl_Dec = 0, tr_RA = 0, tr_Dec = 0, bl_RA = 0, bl_Dec = 0, br_RA = 0, br_Dec = 0, top_RA = 0, top_Dec = 0, bot_RA = 0, bot_Dec = 0))
  
  cat("Reading in segmentation map data\n")
  segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
  cat("Generating groupim\n")
  groupim = profoundSegimGroup(segim = segim)
  
  if(missing(images) == TRUE){
  cat("Loading images as pointers\n")
  g_image = Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
  r_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
  i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"),header=TRUE,ext=1)
  cat("Warping r&i frames\n")
  r_image=propaneWarp(r_image_input,keyvalues_out= g_image$keyvalues)
  i_image=propaneWarp(i_image_input,keyvalues_out= g_image$keyvalues)
  }else{
    g_image = images[1]
    r_image = images[2]
    i_image = images[3]
  }
  g_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
  r_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
  i_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))

  assign("groupim", groupim, envir = .GlobalEnv)
  assign("g_image", g_image, envir = .GlobalEnv)
  assign("r_image", r_image, envir = .GlobalEnv)
  assign("i_image", i_image, envir = .GlobalEnv)
  assign("g_hdr", r_hdr, envir = .GlobalEnv)
  assign("r_hdr", r_hdr, envir = .GlobalEnv)
  assign("i_hdr", i_hdr, envir = .GlobalEnv)
  
  #return(list(groupim, g_image, r_image, i_image, g_hdr, r_hdr, i_hdr))
  
  }

Cutout <- function(asteroids, ID, colour, loc, keyvalues){
  # galpos=asteroids[asteroids$groupID == ID, c("xmax","ymax")]
  galradec = asteroids[asteroids$groupID == ID, c("RAcen", "Deccen")]
  galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen, Dec=galradec$Deccen, keyvalues=keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_i=i_image[galpos,box=box]
  
  cat("Making cutgroup_dilate")
  cutgroup_dilate=magcutout(image = groupim$groupim, loc=as.numeric(galpos),box=box,loc.type="image")
  
  decoff=2*(wid*0.339/3600.0)
  raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
  
  assign("groupcut", cutgroup_dilate, envir = .GlobalEnv)
  
  #return(cutgroup_dilate)
}

Edge_Finder <- function(ID, groupimage){
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
  
  obj_points <- which(groupimage == ID, arr.ind = TRUE)
  
  top_right <- obj_points[which.max(obj_points[, 1] + obj_points[, 2]), ]
  asteroids$tr_RA[i] = xy2radec(top_right, hdr)[1]
  asteroids$tr_Dec[i] = xy2radec(top_right, hdr)[2]
  
  top_left <- obj_points[which.min(obj_points[, 1] - obj_points[, 2]), ]
  asteroids$tl_RA[i] = xy2radec(top_left, hdr)[1]
  asteroids$tl_Dec[i] = xy2radec(top_left, hdr)[2]
  
  bottom_right <- obj_points[which.max(obj_points[, 1] - obj_points[, 2]), ]
  asteroids$br_RA[i] = xy2radec(bottom_right, hdr)[1]
  asteroids$br_Dec[i] = xy2radec(bottom_right, hdr)[2]
  
  bottom_left <- obj_points[which.min(obj_points[, 1] + obj_points[, 2]), ]
  asteroids$bl_RA[i] = xy2radec(bottom_left, hdr)[1]
  asteroids$bl_Dec[i] = xy2radec(bottom_left, hdr)[2]
  
  ave_top <- c((top_right[[1]] + bottom_right[[1]])/2 , (top_right[[2]] + bottom_right[[2]])/2)
  asteroids$top_RA[i] = xy2radec(ave_top, hdr)[1]
  asteroids$top_Dec[i] = xy2radec(ave_top, hdr)[2]
  
  ave_bottom <- c((top_left[[1]] + bottom_left[[1]])/2 , (top_left[[2]] + bottom_left[[2]])/2)
  asteroids$bot_RA[i] = xy2radec(ave_bottom, hdr)[1]
  asteroids$bot_Dec[i] = xy2radec(ave_bottom, hdr)[2]
  
  x = c(ave_top[[1]],ave_bottom[[1]])
  y = c(ave_top[[2]],ave_bottom[[2]])
  locations = cbind(x,y)
  assign("locations", locations, envir = .GlobalEnv)
}
  
Image_Maker <- function(ID, colour, groupcut, locations){
  
  cat("Printing ",colour,ID," postage stamp\n")
  png(filename=paste0("./",loc,"/Group_Cutouts/",colour,ID,".png"))
  
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
  
  cat("Time to start printing images!\n")
  Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues, Bkeyvalues = i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
  
  magimage(groupimage,col=c(NA,rep(skyblue,max(groupimage))),magmap=FALSE,add=TRUE,sparse=1)
  points(locations, col=c("#FFA500", "#05ffa1"), add=TRUE, pch = 4, lwd = 3)
  legend(x ="topright", legend = c("Right Midpoint", "Left Midpoint"), pch = c(3,3,3,3), col = c("#FFA500", "#05ffa1"))
  
  magimage(groupimage[groupimage == ID], col=c(NA,rep(paint,max(groupimage))),magmap=FALSE,add=TRUE,sparse=1)
  text(1,2*wid-50, label=paste0("ID=",colour,ID), paint, cex=2.0, pos=4)
  
  dev.off()
}

Group_Cutter(loc)
# astcheck = data.frame()
# colours = c("green", "red", "blue")
# for(i in 1:3){
#   co = colours[i]
#   #Read in asteroid data
#   cat("Reading in MPC Asteroid data\n")
#   ast = cbind(read.table(paste0("./",loc,"/",loc,"_MPC_",co,".txt"), col.names = c("ID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt")), co)
#   colnames(ast) = c("ID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt", "Colour")
#   astcheck = rbind(astcheck, ast)
# }



######################################################################

  
  # MPC_Asteroids = astcheck[astcheck$RA >galradec$RAcen - raoff & astcheck$RA < galradec$RAcen + raoff & astcheck$Dec > galradec$Deccen - decoff  & astcheck$Dec < galradec$Deccen + decoff, c("RA","Dec")]
  # 
  # cat("Adding MPC asteroid points\n")
  # if(length(MPC_Asteroids$RA) != 0){
  # MPC_xy = data.table()
  # for(k in 1:length(MPC_Asteroids$RA)){
  #   cat("Adding MPC ", k, " asteroid points\n")
  #   xy = radec2xy(MPC_Asteroids$RA[k], MPC_Asteroids$Dec[k], header = image_header)
  #   MPC_xy = rbind(MPC_xy, data.table(x= xy[[1]], y=xy[[2]]))
  # }
  # MPC_Asteroids = cbind(MPC_Asteroids, MPC_xy)
  # 
  # points(MPC_Asteroids$x, MPC_Asteroids$y, pch = 11, col = rainbow(100))
  # }
  
