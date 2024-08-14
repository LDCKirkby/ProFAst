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
library(foreign,quietly = TRUE)
library(MASS,quietly = TRUE)
library(ProPane, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(gsubfn, quietly = TRUE)
library(fs, quietly = TRUE)
library(showtext, quietly = TRUE)
showtext_auto()

args = commandArgs(trailingOnly = TRUE)
computer = as.character(args[[1]])

if("sabine" == tolower(computer)){
  files = as.data.frame(list.files(path = "/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/", pattern = "_u_"))
}else{
  files = as.data.frame(list.files(path = "/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/", pattern = "_u_"))
}
colnames(files) <- c("RA_Dec")
RA = c()
Dec = c()
for (i in 1:length(files$RA_Dec)){
  RA = append(RA,strsplit(files$RA_Dec[i], split = "_")[[1]][2])
  Dec = append(Dec, strsplit(files$RA_Dec[i], split = "_")[[1]][3])

}

RA_Dec = cbind(RA, Dec)

RA_Dec = as.data.frame(RA_Dec)
colnames(RA_Dec) = c("RA", "Dec")

RA_Dec = RA_Dec[order(RA_Dec$Dec, decreasing = TRUE),]
bearings = c()
for(j in 1:length(RA_Dec$Dec)){
  bearings = append(bearings, paste0(RA_Dec$RA[[j]],"_",RA_Dec$Dec[[j]]))
}
colname(bearings) = c("RA_Dec")
write.csv(bearings, "./bearings.csv", quote = FALSE)