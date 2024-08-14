source("./R_Files/Pre_Proc.R")
source("./R_Files/New_Detect.R")
source("./R_Files/Flux_Filter.R")
source("./R_Files/Axrat_Filter.R")
source("./R_Files/N100_Filter.R")
source("./R_Files/Group_Cutter.R")

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
RA_DEC = gsub("[\r\n]", "", as.character(args[[1]]))
computer = as.character(args[[2]])

if(tolower(computer) == "sabine"){
  font_add("Arial", "/Users/lukekirkby/Library/Fonts/Arial.ttf")
}else if(tolower(computer) == "simon"){
  font_add("Arial", "/Users/lkirkby/Library/Fonts/Arial.ttf")
}else{
  font_add("Arial", "/Library/Fonts/Arial.ttf")
}

dir = getwd()

  cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
  frames <- Pre_Proc(RA_DEC, computer)

  New_Detect(RA_DEC, frames)

  Flux_Filter(RA_DEC)

  Axrat_Filter(RA_DEC)
  
  N100_Filter(RA_DEC)

  Group_Cutter(RA_DEC, computer)
  
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
  
  
  
