library(celestial)
library(devtools)
library(Cairo)
library(Rfits)
library(Rwcs)
library(ProFound)
library(magicaxis)
library(data.table)
library(plotrix)
require(foreign)
require(MASS)
library(ProPane)
library(gsubfn)
library(fs)
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")
showtext_auto()


args = commandArgs(trailingOnly=TRUE)
loc = args[[1]]


Group_Cutter <- function(loc, images){
  
wid <- 200.0
box<-c(2*wid,2*wid)
mulim<-22.0
kids<-(0.339^2)*(10^(0.4*(0-mulim)))
viking<-(0.339^2)*(10^(0.4*(30-mulim)))

assign("wid", wid, envir = .GlobalEnv)
assign("box", box, envir = .GlobalEnv)
assign("mulim", mulim, envir = .GlobalEnv)
assign("kids", kids, envir = .GlobalEnv)
assign("viking", viking, envir = .GlobalEnv)
assign("loc", loc, envir = .GlobalEnv)

cat("**************************\n")
cat("Reading in data\n")
asteroids = read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
asteroids <- cbind(asteroids, data.frame(tl_RA = 0, tl_Dec = 0, tr_RA = 0, tr_Dec = 0, bl_RA = 0, bl_Dec = 0, br_RA = 0, br_Dec = 0, top_RA = 0, top_Dec = 0, bot_RA = 0, bot_Dec = 0))
assign("asteroids", asteroids, envir = .GlobalEnv)

#Make a directory to save the cutouts
if(dir_exists(paste0("./",loc,"Group_Cutouts")) == TRUE){
  cat("Group_Cutouts already exists\n")
  dir_delete(paste0("./",loc,"/Group_Cutouts/"))
}
dir.create(paste0("./",loc,"/Group_Cutouts/"))

if(missing(images)){
  cat("Images not supplied\n")
  Data_Reader(loc)
}else{
  assign("images", images, envir = .GlobalEnv)
  Data_Reader(loc,images)
}
cat("**************************\n")

Edger(segim)
Edger(groupim)

cat("**************************\n")
  
cat("Time to start printing images!\n")
for(i in 1:length(asteroids$segID)){
  cat("\n**************************\n")
  
  assign("i", i, envir = .GlobalEnv)
  target = asteroids[i,]

  #i = which(asteroids$segID == segID)[1]
  #segID = asteroids$segID[[i]]
  groupID = target$groupID
  assign("groupID", groupID, envir = .GlobalEnv)
  segID = target$segID
  assign("segID", segID, envir = .GlobalEnv)
  
  colour = target$Colour

  cat("Imaging groupID:", groupID, ", segID:",segID, ", i:", i, ", colour:", colour,"\n")
  
  error = 0

  if(grepl(colour,"g") == TRUE){
    image_header = g_image$header
    keyvalues = g_image$keyvalues
    hdr = g_hdr$hdr
    groupcol = "seagreen2"
    segcol = "green"
    list[target, locations, status] <- Top_bottom(groupim, target, groupID, hdr)
    if(status == -1){
      groupim = segim
    }
    list[target, locations, status] <- Top_bottom(segim, target, segID, hdr)
    Cutout(target, keyvalues, i)
    Image_Maker(segID, groupID, colour, locations, groupcol, segcol)
    
    
  }else if(grepl(colour,"r") == TRUE){
    image_header = r_image$header
    keyvalues = r_image$keyvalues
    hdr = r_hdr$hdr
    groupcol = "firebrick2"
    segcol = "firebrick4"
    list[target, locations, status] <- Top_bottom(groupim, target, groupID, hdr)
    if(status == -1){
      groupim = segim
    }
    list[target, locations, status] <- Top_bottom(segim, target, segID, hdr)
    Cutout(target, keyvalues, i)
    Image_Maker(segID, groupID, colour, locations, groupcol, segcol)
    
  }else if(grepl(colour,"i") == TRUE){
    image_header = i_image$header
    keyvalues = i_image$keyvalues
    hdr = i_hdr$hdr
    groupcol = "skyblue"
    segcol = "blue"
    list[target, locations, status] <- Top_bottom(groupim, target, groupID, hdr)
    if(status == -1){
      groupim = segim
    }
    list[target, locations, status] <- Top_bottom(segim, target, segID, hdr)
    Cutout(target, keyvalues, i)
    Image_Maker(segID, groupID, colour, locations, groupcol, segcol)
    
  }
  asteroids[i,] = target
  cat("**************************\n")
  
}
    cat("Writing out data with top & bottom locations\n")
    write.csv(asteroids, file=paste0("./", loc,"/",loc,"_Asteroids.csv"))
}



Data_Reader <- function(loc, images){

  cat("Reading in segmentation map data\n")
  segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
  cat("Generating groupim\n")
  groupim = profoundSegimGroup(segim = segim)
  
  if(missing(images) == TRUE){
  cat("Loading images as pointers\n")
  g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
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
  
  assign("segim", segim, envir = .GlobalEnv)
  assign("groupim", groupim$groupim, envir = .GlobalEnv)
  assign("g_image", g_image, envir = .GlobalEnv)
  assign("r_image", r_image, envir = .GlobalEnv)
  assign("i_image", i_image, envir = .GlobalEnv)
  assign("g_hdr", r_hdr, envir = .GlobalEnv)
  assign("r_hdr", r_hdr, envir = .GlobalEnv)
  assign("i_hdr", i_hdr, envir = .GlobalEnv)
  
  #return(list(groupim, g_image, r_image, i_image, g_hdr, r_hdr, i_hdr))
  
  }



Edger <- function(input_image){
  name = deparse(substitute(input_image))
  
  cat("Finding the edges of ",name," segmentation masks\n")
  image = input_image
  xrun=1:(dim(image)[1]-1)
  yrun=1:(dim(image)[2]-1)
  
  image_lb=image[xrun,yrun]
  image_lt=image[xrun+1,yrun]
  image_rt=image[xrun+1,yrun+1]
  image_rb=image[xrun,yrun+1]
  
  image_temp = (image_lb == image_lt) & (image_rt == image_rb) & (image_lb == image_rb) & (image_lt == image_rt)
  
  image_edge=matrix(0,dim(image)[1],dim(image)[2])
  
  image_edge[xrun,yrun]=image_edge[xrun,yrun]+image_temp
  image_edge[xrun+1,yrun]=image_edge[xrun+1,yrun]+image_temp
  image_edge[xrun+1,yrun+1]=image_edge[xrun+1,yrun+1]+image_temp
  image_edge[xrun,yrun+1]=image_edge[xrun,yrun+1]+image_temp
  
  image[image_edge==4]=0
  
  rm(image_edge, image_temp, image_lb, image_lt, image_rt, image_rb)
  
  assign(name, image, envir = .GlobalEnv)
}



Top_bottom <- function(image, ast, ID, hdr){
  
  name = deparse(substitute(image))
  
  cat("Identifying key points of object in", name, "\n")
  `%notin%`<-Negate(`%in%`)
  asteroid_image = image
  asteroid_image[asteroid_image%notin%ID]=0
  
  obj_points <- which(asteroid_image == ID, arr.ind = TRUE)
  
  if(length(obj_points) < 2){
    assign("asteroid_image", asteroid_image, envir = .GlobalEnv)
    #assign("locations", c(0,0), envir = .GlobalEnv)
    cat("No outline found for ", ID," in ", name, "\n")
    return(list(ast, list(c(0,0),c(0,0)), -1))
  }
  
  top_right <- obj_points[which.max(obj_points[, 1] + obj_points[, 2]), ]
  ast$tr_RA = xy2radec(top_right[[1]],top_right[[2]], hdr)[1]
  ast$tr_Dec = xy2radec(top_right[[1]],top_right[[2]], hdr)[2]
  
  top_left <- obj_points[which.min(obj_points[, 1] - obj_points[, 2]), ]
  ast$tl_RA = xy2radec(top_left[[1]], top_left[[2]], hdr)[1]
  ast$tl_Dec = xy2radec(top_left[[1]],top_left[[2]], hdr)[2]
  
  bottom_right <- obj_points[which.max(obj_points[, 1] - obj_points[, 2]), ]
  ast$br_RA = xy2radec(bottom_right[[1]], bottom_right[[2]], hdr)[1]
  ast$br_Dec = xy2radec(bottom_right[[1]],bottom_right[[2]], hdr)[2]
  
  bottom_left <- obj_points[which.min(obj_points[, 1] + obj_points[, 2]), ]
  ast$bl_RA = xy2radec(bottom_left[[1]], bottom_left[[2]], hdr)[1]
  ast$bl_Dec = xy2radec(bottom_left[[1]], bottom_left[[2]], hdr)[2]
  
  ave_top <- c((top_right[[1]] + bottom_right[[1]])/2 , (top_right[[2]] + bottom_right[[2]])/2)
  ast$top_RA = xy2radec(ave_top[[1]], ave_top[[2]], hdr)[1]
  ast$top_Dec = xy2radec(ave_top[[1]],ave_top[[2]], hdr)[2]
  
  ave_bottom <- c((top_left[[1]] + bottom_left[[1]])/2 , (top_left[[2]] + bottom_left[[2]])/2)
  ast$bot_RA = xy2radec(ave_bottom[[1]],ave_bottom[[2]], hdr)[1]
  ast$bot_Dec = xy2radec(ave_bottom[[1]],ave_bottom[[2]], hdr)[2]
  
  cen_flux = c(asteroids$xcen, asteroids$ycen)
  max_flux = c(asteroids$xmax, asteroids$ymax)
  
  x = c(top_right[[1]], top_left[[1]], bottom_right[[1]], bottom_left[[1]], ave_top[[1]], ave_bottom[[1]], cen_flux[[1]], max_flux[[1]])
  y = c(top_right[[2]], top_left[[2]], bottom_right[[2]], bottom_left[[2]], ave_top[[2]], ave_bottom[[2]], cen_flux[[2]], max_flux[[2]])
  locations = cbind(x,y)
  
  assign(paste0("ast_",name), asteroid_image, envir = .GlobalEnv)
  #assign("locations", locations, envir = .GlobalEnv)
  
  return(list(ast,locations,1))
}



Cutout <- function(target, keyvalues, i){
  # galpos=asteroids[asteroids$segID == segID, c("xmax","ymax")]
  galradec = target[c("RAcen", "Deccen")]
  #galradec = asteroids[i , c("RAcen", "Deccen")]
  galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen, Dec=galradec$Deccen, keyvalues=keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  cutim_g=g_image[galpos,box=box]
  cutim_r=r_image[galpos,box=box]
  cutim_i=i_image[galpos,box=box]
  
  cat("Making cut images\n")
  segimcut=magcutout(image = segim, loc=as.numeric(galpos),box=box,loc.type="image")
  groupcut=magcutout(image = groupim, loc=as.numeric(galpos),box=box,loc.type="image")
  ast_segimcut=magcutout(image = ast_segim, loc=as.numeric(galpos),box=box,loc.type="image")
  ast_groupcut=magcutout(image = ast_groupim, loc=as.numeric(galpos),box=box,loc.type="image")
  
  assign("cutim_g", cutim_g, envir = .GlobalEnv)
  assign("cutim_r", cutim_r, envir = .GlobalEnv)
  assign("cutim_i", cutim_i, envir = .GlobalEnv)
  assign("segimcut",segimcut, envir = .GlobalEnv)
  assign("groupcut",groupcut,envir = .GlobalEnv)
  assign("ast_segimcut", ast_segimcut, envir = .GlobalEnv)
  assign("ast_groupcut", ast_groupcut, envir = .GlobalEnv)
}

  

Image_Maker <- function(segID, groupID, colour, locations, groupcol, segcol){
  
  cat("Printing ",colour,segID," postage stamp\n")
  png(filename=paste0("./",loc,"/Group_Cutouts/",colour,segID,".png"), family = "")
  
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
  
  Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues, Bkeyvalues = i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
  
  cat("Adding segment outlines\n")
  magimage(segimcut$image,col=c(NA,rep("moccasin",max(segimcut$image))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)
  magimage(groupcut$image,col=c(NA,rep("peru",max(groupcut$image))),magmap = FALSE,add=TRUE,sparse=1,lwd=1)
  
  magimage(ast_segimcut$image,col=c(NA,rep(segcol, max(ast_segimcut$image))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)
  magimage(ast_groupcut$image,col=c(NA,rep(groupcol, max(ast_groupcut$image))),magmap=FALSE,add=TRUE,sparse=1,lwd=1)
  
  cat("Adding max & min points\n")
  points(locations, col=c("orangered" , "orange", "sienna1", "darkviolet", "mediumorchid" , "darkmagenta", "hotpink", "gold"), pch = 4, lwd = 3)
  
  legend(x ="topright", legend = c("Top Right", "Bottom Right", "Right Midpoint", "Top Left", "Bottom Left", "Left Midpoint", "Center of Flux", "Max Flux"), pch = c(3,3,3,3), col = c("orangered" , "orange", "sienna1", "darkviolet", "mediumorchid" , "darkmagenta", "hotpink", "gold"))
  
  text(1,2*wid-50, col=groupcol, label=paste0("segID=",groupcol,segID), cex=2.0, pos=4, family = "")
  
  dev.off()
}



Group_Cutter(loc)
warnings()

# 
# tryCatch({Group_Cutter(loc)}, error = function(e) {print(paste("Error:", e))})
# warnings()
# astcheck = data.frame()
# colours = c("green", "red", "blue")
# for(i in 1:3){
#   co = colours[i]
#   #Read in asteroid data
#   cat("Reading in MPC Asteroid data\n")
#   ast = cbind(read.table(paste0("./",loc,"/",loc,"_MPC_",co,".txt"), col.names = c("segID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt")), co)
#   colnames(ast) = c("segID", "RA", "Dec", "mag", "dRA/dt", "dDec/dt", "Colour")
#   astcheck = rbind(astcheck, ast)
# }



######################################################################

  # decoff=2*(wid*0.339/3600.0)
  # raoff=2*(wid*0.339/3600.0)/cos(galradec$Deccen*0.01745329)
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
  
