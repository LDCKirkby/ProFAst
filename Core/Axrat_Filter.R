Axrat_Filter <- function(loc){

possible_asteroids = read.csv(paste0("./",loc,"/",loc,"_Possible_Asteroids.csv"), fill = TRUE)
cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= 0.35 | axrat_rxt <= 0.35 | axrat_i1xt <= 0.35)
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
