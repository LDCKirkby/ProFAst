source("./R_Files/Pre_Proc.R")
source("./R_Files/Flux_Filter.R")
source("./R_Files/Axrat_Filter.R")
source("./R_Files/N100_Filter.R")
source("./R_Files/Group_Cutter.R")
source("./R_Files/memobj.R")

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
font_add("Arial", "/Library/Fonts/Arial.ttf")


#Uncomment to create ordered heading csv
# bearings = as.data.frame(list.files(path = "/Volumes/WAVES/waves/wavesdata/kids/dr5/preprocessed/", pattern = "_u_"))
# colnames(bearings) <- c("RA_Dec")
# RA = c()
# Dec = c()
# for (i in 1:length(bearings$RA_Dec)){
#   RA = append(RA,strsplit(bearings$RA_Dec[i], split = "_")[[1]][2])
#   Dec = append(Dec, strsplit(bearings$RA_Dec[i], split = "_")[[1]][3])
#   #bearings[i] = 
#   
# }
# # colnames(bearings) <- c("RA", "Dec")
# 
# RA_Dec = cbind(RA, Dec)
# 
# RA_Dec = as.data.frame(RA_Dec)
# colnames(RA_Dec) = c("RA", "Dec")
# 
# RA_Dec = RA_Dec[order(RA_Dec$Dec, decreasing = TRUE),]
# 
# write.csv(RA_Dec, "/Users/lkirkby/bearings.csv")


args = commandArgs(trailingOnly = TRUE)
i = as.numeric(args[[1]])
dir = getwd()
locs = read.delim("./done.txt")
RA_DEC = locs[i,]

cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
# frames <- Pre_Proc(RA_DEC, "Simon")

Flux_Filter(RA_DEC)

Axrat_Filter(RA_DEC)

N100_Filter(RA_DEC)

Group_Cutter(RA_DEC)

warnings()


#Uncomment to remove any unwanted files
#file.remove(paste0("/Volumes/WAVES/lkirkby/",RA_DEC,"/stacked.rds"))
#file.remove(paste0("/Users/lkirkby/",RA_DEC,"/allcati.csv"))
#file.remove(paste0("/Users/lkirkby/",RA_DEC,"/groupcati.csv"))
#file.remove(paste0("/Users/lkirkby/",RA_DEC,"/objectcati.csv"))
#file.remove(paste0("/Users/lkirkby/",RA_DEC,"/Possible_Asteroids.csv"))

# Add after each function call to record ram usage
# peakRAM()
# post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", New_Detect_RAM$Elapsed_Time_sec, 
#                          "\nTotal RAM used during New_Detect for ", RA_DEC," :", New_Detect_RAM$Total_RAM_Used_MiB,
#                          "\nMax RAM used during New_Detect for ", RA_DEC," :", New_Detect_RAM$Peak_RAM_Used_MiB))



