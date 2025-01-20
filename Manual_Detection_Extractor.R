

args = commandArgs(trailingOnly = TRUE)
RA_DEC = gsub("[\r\n]", "", as.character(args[[1]]))

filtered_data = read.csv(paste0(RA,"_",Dec,"/",RA,"_",Dec,"_N100_Filtered_Asteroids.csv"))
visual_asteroids = read.csv(paste0(RA,"_",Dec,"/",RA,"_",Dec,"_real_asteroids.csv"), header=TRUE)
verified_asteroids = subset(filtered_data, filtered_data$segID %in% visual_asteroids$segID)

write.csv(verified_asteroids, file=paste0(RA,"_",Dec,"/",RA,"_",Dec,".csv"))
