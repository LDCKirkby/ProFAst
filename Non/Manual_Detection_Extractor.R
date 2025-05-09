library(stringr)
args = commandArgs(trailingOnly = TRUE)
RA_Dec = gsub("[\r\n]", "", as.character(args[[1]]))

filtered_data = read.csv(paste0(RA_Dec,"/",RA_Dec,"_N100_Filtered_Asteroids.csv"))
visual_asteroids = read.csv(paste0(RA_Dec,"/",RA_Dec,"_final_asteroids.csv"), header=TRUE)
asteroid_IDS = str_sub(visual_asteroids$segID, 2, -1)

#Check for incorrectly coloured asteroids
wrong_g = read.csv(paste0(RA_Dec,"/",RA_Dec,"_wrong_g.csv"), header = TRUE)
wrong_r = read.csv(paste0(RA_Dec,"/",RA_Dec,"_wrong_r.csv"), header = TRUE)
wrong_i = read.csv(paste0(RA_Dec,"/",RA_Dec,"_wrong_i.csv"), header = TRUE)

g_IDS = str_sub(wrong_g$segID, 2, -1)
r_IDS = str_sub(wrong_r$segID, 2, -1)
i_IDS = str_sub(wrong_i$segID, 2, -1)

if(length(g_IDS) != 0){
filtered_data[filtered_data$segID %in% g_IDS,]$Colour = "g"
}
if(length(r_IDS) != 0){
filtered_data[filtered_data$segID %in% r_IDS,]$Colour = "r"
}
if(length(i_IDS) != 0){
filtered_data[filtered_data$segID %in% i_IDS,]$Colour = "i"
}

verified_asteroids = subset(filtered_data, filtered_data$segID %in% asteroid_IDS | filtered_data$segID %in% g_IDS |
                            filtered_data$segID %in% r_IDS | filtered_data$segID %in% i_IDS)
write.csv(verified_asteroids, file=paste0(RA_Dec,"/",RA_Dec,"_FINAL.csv"))

