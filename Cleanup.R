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

frames = as.data.frame(dir(path = "/Volumes/WAVES/lkirkby/", pattern = "_"))
len = length(frames[[1]])
print(len)
leng = len - 1
lengt = len -2
frames = frames[- c(lengt,leng, len),]


for(i in 1:length(frames)){
  if(try(read.csv(paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/segimlist.csv"))) == TRUE){
    next
  }
  cat("Reading ",frames[[i]]," rdsfile\n")
  stacked = readRDS(paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/stacked.rds"))
  
  cat("Extracting segmasks\n")
  segimlist = stacked$segimlist
  segim = stacked$pro_detect$segim
  segim_orig = stacked$pro_detect$segim_orig
  # cat("Saving slimmed rds\n")
  # saveRDS(cbind(segimlist,segim,segim_orig), file = paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/slimmed.rds"))
  cat("Saving slimmed segimlist\n")
  print(length(segimlist))
  write.csv(segimlist, paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/segimlist.csv"))
  cat("Saving slimmed segim\n")
  write.csv(segim, paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/segim.csv"))
  cat("Saving slimmed segim_orig\n")
  write.csv(segim_orig, paste0("/Volumes/WAVES/lkirkby/",frames[[i]],"/segim_orig.csv"))
}