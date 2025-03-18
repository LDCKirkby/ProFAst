library(peakRAM,quietly = TRUE)
library(xtable, quietly = TRUE)
library(dst,quietly=TRUE)
library(celestial,quietly = TRUE)
library(devtools,quietly = TRUE)
library(Cairo,quietly = TRUE)
library(Rfits,quietly = TRUE)
library(Rwcs,quietly = TRUE)
library(ProFound,quietly = TRUE)
library(magicaxis,quietly = TRUE)
library(data.table,quietly = TRUE)
library(plotrix,quietly = TRUE)
require(foreign,quietly = TRUE)
require(MASS,quietly = TRUE)
library(ProPane, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(gsubfn, quietly = TRUE)
library(fs, quietly = TRUE)
library(showtext, quietly = TRUE)
library(png)
library(Matrix)
source("./ProFAst/Core/MPC_Formatter.R")

Edger <- function(segimcut, ID){
  image = segimcut$image
  
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
  image[is.na(image)] <- 0
  
  image[image%notin%ID]=0
  return(image)
}

args = commandArgs(trailingOnly = TRUE)
loc = as.character(args[[1]])

asteroids = read.csv(paste0("./",loc,"/",loc,"_no_dupes.csv"))
stopifnot(length(asteroids$segID) >= 1)

#Checks to see if linear fit has already been done on the field
if(dir.exists(file.path(paste0("./",loc,"/"),paste0("Linear_Fits"))) == TRUE){
  #If it has, checks to see it completed fully, fitting all asteroids
  done = list.files(file.path(paste0("./",loc,"/"),paste0("Linear_Fits/MPC_Format")), pattern = ".", all.files = FALSE, recursive = TRUE, full.names = TRUE)
  cat(length(done), "files already exist, ", length(asteroids$segID), " asteroids exist for the field.\n")
  #Exits if all have been done previously
  if(length(done) == length(asteroids$segID)){
      stopifnot(dir.exists(file.path(paste0("./",loc,"/"),paste0("Linear_Fits"))) == FALSE)
  }
}

dir_create("./",loc,"/Linear_Fits")
dir_create("./",loc,"/Linear_Fits/MPC_Format")
dir_create("./",loc,"/Linear_Fits/Fit_Images")

cat("***************** Reading in segmentation map data *****************\n")
segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
cat("*****************  Generating groupim ***************** \n")
groupim = profoundSegimGroup(segim = segim)
groupim = groupim$groupim

cat("*****************  Loading images as pointers ***************** \n")
g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)
g_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
r_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"),header=TRUE,ext=1)
r_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
i_image_input= Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"),header=TRUE,ext=1)
i_header = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
cat("*****************  Warping r&i frames ***************** \n")
r_image=propaneWarp(r_image_input,keyvalues_out=g_image$keyvalues)
i_image=propaneWarp(i_image_input,keyvalues_out=g_image$keyvalues)

for(i in 1:length(asteroids$segID)){
  target = asteroids[i,]
  ID = target$segID
  groupID = target$groupID
  colour = target$Colour
  hdr = switch(colour, "g" = g_header$hdr, "r" = r_image$hdr, "i" = i_image$hdr)
  keyvals = switch(colour, "g" = g_image$keyvalues, "r" = r_image$keyvalues, "i" = i_image$keyvalues)

  cat("*****************  Fitting Asteroid ", ID, " *****************\n")
  
  astradec = target[c("RAcen", "Deccen")]
  astpos=as.integer(Rwcs_s2p(RA=astradec$RAcen, Dec=astradec$Deccen, keyvalues=keyvals, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  wid <- 100.0
  box<-c(2*wid,2*wid)
  mulim<-22.0
  kids<-(0.339^2)*(10^(0.4*(0-mulim)))
  viking<-(0.339^2)*(10^(0.4*(30-mulim)))
  
  cutim_g=g_image[astpos,box=box]
  cutim_r=r_image[astpos,box=box]
  cutim_i=i_image[astpos,box=box]
  
  segimcut=magcutout(image = segim, loc=as.numeric(astpos), box=box, loc.type="image")
  groupcut=magcutout(image = groupim, loc=as.numeric(astpos), box=box, loc.type="image")

  obj_points <- which(segimcut$image==ID, arr.ind = TRUE)
  group_points <- which(groupcut$image==groupID, arr.ind = TRUE)

  edged_segimcut <- Edger(segimcut, ID)
  edged_groupcut <- Edger(groupcut, groupID)
  
  y_vals = obj_points[,2]
  x_vals = obj_points[,1]
  
  brightness_vals = switch(colour, 
                           "g" = cutim_g$imDat[obj_points]/ max(cutim_g$imDat[obj_points]),
                           "r" = cutim_r$imDat[obj_points]/ max(cutim_r$imDat[obj_points]),
                           "i" = cutim_i$imDat[obj_points]/ max(cutim_i$imDat[obj_points]))
  brightness_vals[brightness_vals<0] <- 0
  
  # weights_g <- cutim_g$imDat[segimcut$image]/ max(cutim_g$imDat)
  # weights_r <- cutim_r$imDat[segimcut$image]/ max(cutim_r$imDat)
  # weights_i <- cutim_i$imDat[segimcut$image]/ max(cutim_i$imDat)
  #brightness_vals <- droplevels(brightness_vals)
  #x_vals = droplevels(x_vals)
  fit <- lm(y_vals ~ poly(x_vals, 1, raw = TRUE), weights = brightness_vals)
  
  #x_range <- range(x_vals) + c(-0.01,5) # Used to extend the range of x_vals on each side
  
  x_pred <- seq(min(x_vals), max(x_vals), length.out = 10)
  y_pred <- predict(fit, newdata = data.frame(x_vals = x_pred))
  
  RA_vals = c()
  Dec_vals = c()
  for(j in 1:length(x_pred)){
    RA_Dec = xy2radec(x_pred[[j]], y_pred[[j]], header = hdr)
    RA_vals <- append(RA_vals, RA_Dec[[1]][1])
    Dec_vals <- append(Dec_vals, RA_Dec[[2]][1])
  }

  new_IDs = formatter(loc, ID, colour, target$mag, RA_vals, Dec_vals)
  
  png(filename=paste0("./",loc,"/Linear_Fits/Fit_Images/",loc,"_",colour,target$segID,"_linear_fit.png"))
  par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
  
  locut = c(median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE),median(cutim_i$imDat,na.rm=TRUE))
  
  line_col = switch(colour, "g" = "green", "r" = "red", "i" = "blue")

  Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = r_image$keyvalues, Gkeyvalues = g_image$keyvalues, Bkeyvalues = i_image$keyvalues,
                xlab="Right Ascension (deg)",ylab="Declination (deg)", main = paste0("Asteroid ", ID), coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp = FALSE, hersh = FALSE, family="Arial")
  
  magimage(edged_segimcut,col=c(NA,rep(line_col, max(edged_segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)
  magimage(edged_groupcut,col=c(NA,rep("white", max(edged_groupcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=1)

  text(1,2*wid-50, col=line_col, label=paste0("temp_ID=",new_IDs[1]), cex=2.0, pos=4, family="Arial")

  lines(x_pred, y_pred, col = line_col, lwd = 1)
  
  dev.off()
  
  png(filename=paste0("./",loc,"/Linear_Fits/Fit_Images/",loc,"_",colour,target$segID,"_fit.png"))
  par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
  
  magplot(x_vals, y_vals, z=brightness_vals, cex = 2, xlab = "X", ylab = "Y", main = paste0("Linear Fit to asteroid ", new_IDs[1], " with weights colourised"), position = 'bottomright', range=c(0,1))
  lines(x_pred, y_pred, col = line_col, lwd = 3)
  
  dev.off()
}

warnings()
