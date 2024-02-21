library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
require(MASS)
library(dst)
library(Rwcs)
library(ProPane)
library(Rfits)
source("/Users/lkirkby/R_Files/Entropy_Grapher.R")

home = "/Users/lkirkby/"
dirs = list.dirs("/Users/lkirkby/Fits_Files/", full.names = FALSE, recursive = FALSE)

pdf(file=paste0("/Users/lkirkby/stats.pdf"), width = 20, height = 85)
par(mfrow=c(10,2), mar=c(5,5,5,5))

mulim = 22.0
kids=(0.339^2)*(10^(0.4*(0-mulim)))


for(dir in dirs){
  cat("\nAdding ",dir,"\n")
  RA = as.numeric(strsplit(dir, "_")[[1]][[1]])
  Dec = as.numeric(strsplit(dir, "_")[[1]][[2]])
  asteroids = read.csv(paste0(home, dir,"/Filtered_Asteroids.csv"))
  R = Rfits_read_image(paste0(home,"Fits_Files/",dir,"/rx.fits"))
  B = Rfits_read_image(paste0(home,"Fits_Files/",dir,"/i1x.fits"))
  G = Rfits_read_image(paste0(home,"Fits_Files/",dir,"/g.fits"))
  
  
  
  bruh <- entropy_grapher(dir)
  cat("The first object is of dimensions:",dim(bruh[[1]]),"\n")
  cat("The second object is of dimensions:",dim(bruh[[2]]),"\n")
  cat("The third object is of dimensions:",dim(bruh[[3]]),"\n")
  found = bruh$found
  group_stats = bruh$group_stats
  no_ast = bruh$no_ast
  #magimageRGB(x=no_ast$x, y=no_ast$y,R=R,G=G,B=B,xlim = c(RA-0.5, RA + 0.5), ylim = c(Dec - 0.5, Dec + 0.5))

  magimageWCSRGB(R,G,B,Rheader=G$hdr,Gheader=G$hdr,Bheader=G$hdr,xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=c(median(B$imDat,na.rm=TRUE),median(R$imDat,na.rm=TRUE),median(G$imDat,na.rm=TRUE)),hicut=c(kids,kids,kids),type="num",dowarp=FALSE)
  #magplot(asteroids$x, asteroids$y)
  #use Rwcs_s2p
  cat("Converting RA, Dec to x,y coords\n")
  xy=Rwcs_s2p(group_stats$RAcen,group_stats$Deccen,keyvalues=G$keyvalues)
  points(xy, col = "red", ch = 24, lwd = 1)
  
  cat("Creating Entropy vs axrat plot\n")
  plot(no_ast$entropy[1],no_ast$axrat[1], xlim = c(0, 1), ylim = c(0,1), xlab = "Entropy", ylab = "Axial Ratio")
  text(no_ast$entropy[1], no_ast$axrat[1], labels = no_ast$groupID[1], lwd = 1, offset = 0.5, pos = 1)
  for(i in 2:length(no_ast$entropy)){
    points(no_ast$entropy[i],no_ast$axrat[i])
    text(no_ast$entropy[i], no_ast$axrat[i], labels = no_ast$groupID[i], lwd = 1, offset = 0.5, pos = 1)
  }

  
  cat("Adding group stats to plot\n")
  # points(group_stats$entropy[group_stats$colour == "g"], group_stats$axrat[group_stats$colour == "g"], xlim = c(0,1), ylim = c(0,1), col = "green")
  # points(group_stats$entropy[group_stats$colour == "r"], group_stats$axrat[group_stats$colour == "r"], xlim = c(0,1), ylim = c(0,1), col = "red")
  # points(group_stats$entropy[group_stats$colour == "i"], group_stats$axrat[group_stats$colour == "i"], xlim = c(0,1), ylim = c(0,1), col = "blue")
    real = read.csv(paste0("/Users/lkirkby/",dir,"/real.csv"))
    errors = read.csv(paste0("/Users/lkirkby/",dir,"/errors.csv"))
    colnames(real) = c("X", "groupID")
    colnames(errors) = c("X", "groupID")
    for(i in 1:length(group_stats$entropy)){
      if(group_stats$groupID[i] %in% real$groupID){
        points(group_stats$entropy[i], group_stats$axrat[i], col = "firebrick", pch = 24, lwd = 5)
        
      }
      else{points(group_stats$entropy[i], group_stats$axrat[i], col = "green", pch = 4, lwd = 5)
      }
      #points(group_stats$entropy[i], group_stats$axrat[i], col = "firebrick", pch = 24, lwd = 5)
      label = group_stats$groupID[i]
      text(group_stats$entropy[i], group_stats$axrat[i], labels = label, lwd = 1, offset = 0.5, pos = 1)
      
    }
    
  
  # else{
  # for(i in 1:length(group_stats$entropy)){
  #   points(group_stats$entropy[i], group_stats$axrat[i], col = "firebrick", pch = 24, lwd = 5)
  #   label = group_stats$groupID[i]
  #   text(group_stats$entropy[i], group_stats$axrat[i], labels = label, lwd = 1, offset = 1, pos = 1)
  # }
  # }
  #points(group_stats$entropy, group_stats$axrat, xlim = c(0,1), ylim = c(0,1))
  
  
  
  
}

dev.off()