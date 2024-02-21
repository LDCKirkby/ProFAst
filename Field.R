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


dirs = list.dirs("/Users/lkirkby/Fits_Files/", full.names = FALSE, recursive = FALSE)
png(filename = "/Users/lkirkby/Field.png")
cat("Constructing ProPane Frame\n")
frame = propaneFrameFinder(dirlist="/Volumes/WAVES/waves/profound/tiles_0.3Res",rad=180.0)


for(dir in dirs){
  cat("Adding",dir,"\n")
  #stacked = readRDS(paste0("/Users/lkirkby/", dir, "/stacked.rds"))
  RA = as.numeric(strsplit(dir, "_")[[1]][[1]])
  Dec = as.numeric(strsplit(dir, "_")[[1]][[2]])
  magproj(RA, Dec, type='pl', col=rgb(0,0,255/255.5), border='blue', add=TRUE)
  
}

dev.off()