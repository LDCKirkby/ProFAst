library(stringr)
args = commandArgs(trailingOnly = TRUE)
RA_Dec = gsub("[\r\n]", "", as.character(args[[1]]))

filtered_data = read.csv(paste0(RA_Dec,"/",RA_Dec,"_N100_Filtered_Asteroids.csv"))
visual_asteroids = read.csv(paste0(RA_Dec,"/",RA_Dec,"_clean_asteroids.csv"), header=TRUE)
asteroid_IDS = str_sub(visual_asteroids$segID, 2, -1)
verified_asteroids = subset(filtered_data, filtered_data$segID %in% asteroid_IDS)

write.csv(verified_asteroids, file=paste0(RA_Dec,"/",RA_Dec,"_Verified.csv"))
