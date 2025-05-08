source("./ProFAst/Core/Pre_Proc.R")
source("./ProFAst/Core/Multi_Detect.R")
source("./ProFAst/Core/Flux_Filter.R")
source("./ProFAst/Core/Axrat_Filter.R")
source("./ProFAst/Core/N100_Filter.R")
source("./ProFAst/Core/Group_Cutter.R")

library(peakRAM,quietly = TRUE)
library(xtable, quietly = TRUE)
library(dst,quietly = TRUE)
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
RA_DEC = gsub("[\r\n]", "", as.character(args[[1]]))
loc = RA_DEC
computer = "simon"

files = list.files(path=RA_DEC, pattern="Possible_Asteroids.csv")
if(length(files) == 0){
cat_groups = read.csv(paste0(RA_DEC,"/",RA_DEC,"_Filtered_Asteroids.csv"))

cat_groups$flux_gt = abs(cat_groups$flux_gt)
cat_groups$flux_rxt = abs(cat_groups$flux_rxt)
cat_groups$flux_i1xt = abs(cat_groups$flux_i1xt)

#Extracting potential asteroids, based on their flux ratio
cat("*********\n")
cat("Beginning asteroid search\n")
green_objects = cbind("Colour" = "g", subset(cat_groups, subset = cat_groups$flux_gt/(cat_groups$flux_rxt + cat_groups$flux_i1xt) >= 1))
red_objects = cbind("Colour" = "r", subset(cat_groups, subset = cat_groups$flux_rxt/(cat_groups$flux_gt + cat_groups$flux_i1xt) >= 1))
blue_objects = cbind("Colour" = "i", subset(cat_groups, subset = cat_groups$flux_i1xt/(cat_groups$flux_gt + cat_groups$flux_rxt) >= 1))

#Old Method, less concise than g/r+i, r/g+i, i/g+r but unsure how other will turn out
# green_objects = cbind(subset(cat_groups, subset = cat_groups$flux_gt/cat_groups$flux_rxt>=8 | cat_groups$flux_gt/cat_groups$flux_i1xt>=8), "Colour" = "g")
# red_objects = cbind(subset(cat_groups, subset = cat_groups$flux_rxt/cat_groups$flux_gt>=8 | cat_groups$flux_rxt/cat_groups$flux_i1xt>=8), "Colour" = "r")
# blue_objects = cbind(subset(cat_groups, subset = cat_groups$flux_i1xt/cat_groups$flux_gt>=8 | cat_groups$flux_i1xt/cat_groups$flux_rxt>=8), "Colour" = "i")

#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(loc, "_")[[1]][[1]])
Dec = as.numeric(strsplit(loc, "_")[[1]][[2]])

#Useful to apply edge buffer since some frames are being artificially grown
cat("Applying edge buffer\n")
red_objects = rbind(red_objects[red_objects$RAcen >= (RA - 0.5 + 0.001) & red_objects$RAcen <= (RA + 0.5 - 0.001) & red_objects$Deccen >= (Dec-0.5 + 0.001) & red_objects$Deccen <= (Dec + 0.5 - 0.001),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA - 0.5 + 0.001) & blue_objects$RAcen <= (RA + 0.5 - 0.001) & blue_objects$Deccen >= (Dec-0.5 + 0.001) & blue_objects$Deccen <= (Dec + 0.5 - 0.001),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA - 0.5 + 0.001) & green_objects$RAcen <= (RA + 0.5 - 0.001) & green_objects$Deccen >= (Dec-0.5 + 0.001) & green_objects$Deccen <= (Dec + 0.5 - 0.001),])

#Bind final lists of objects together
possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
print(length(possible_asteroids$groupID))

cat("Writing to ", paste0("./", loc,"/Possible_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(possible_asteroids, file = paste0("./",loc,"/",loc,"_Possible_Asteroids.csv"), row.names=FALSE)

rm(blue_objects, green_objects, red_objects, possible_asteroids, cat_groups)
gc()
Axrat_Filter(RA_DEC)

}else{
possible_asteroids = read.csv(paste0("./",loc,"/_Possible_Asteroids.csv"), fill = TRUE)
cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= 0.35 | axrat_rxt <= 0.35 | axrat_i1xt <= 0.35)
filtered_asteroids = filtered_asteroids[,c(2,251,1,3:250,252)]
filtered_asteroids = setorder(filtered_asteroids, "groupID")

cat("*********\n")
cat("Filtered to ", length(filtered_asteroids$axrat_gt), "potential asteroids\n")
cat("*********\n\n")

cat("*********\n")
cat("Writing to ", paste0("./", loc,"/",loc,"Filtered_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(filtered_asteroids, file = paste0("./",loc,"/",loc,"_Filtered_Asteroids.csv"), row.names=FALSE)

rm(possible_asteroids, filtered_asteroids) 
gc()
}


N100_Filter(RA_DEC)

Group_Cutter(RA_DEC, computer)