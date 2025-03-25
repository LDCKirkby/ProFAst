N100_Filter <- function(loc){
  
  cat("*********\n")
  cat("Applying N100 filter\n")
  cat("*********\n\n")
  
  filtered_asteroids = read.csv(paste0("./", loc,"/",loc,"_Filtered_Asteroids.csv"))

  #N100 filter, filter done in all bands for completeness, even though they are they are all the same value
  cat("*********\n")
  cat("Performing small N100 filter\n")
  large_pass = subset(filtered_asteroids, N100 >= 150 | N100_i1xt >= 150 | N100_rxt >= 150 | N100_gt >= 150)
  
  cat("Removed ", length(filtered_asteroids$groupID) - length(large_pass$groupID), " small errors\n")
  
  cat("Performing large N100 filter\n")
  N100_filtered_asteroids = subset(large_pass, N100 <= 2250 | N100_i1xt <= 2250 | N100_rxt <= 2250 | N100_gt <= 2250)
  
  cat("Removed ", length(large_pass$groupID) - length(N100_filtered_asteroids$groupID), " large errors\n")
  
  cat("Final number of ", length(N100_filtered_asteroids$groupID), " possible asteroids\n")
  
  write.csv(N100_filtered_asteroids, file = paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
  cat("Writing to " ,paste0("./", loc,"/",loc,"_N100_Filtered_Asteroids.csv"),"\n")
  cat("*********\n\n")
  
  rm(N100_filtered_asteroids, filtered_asteroids, large_pass) 
  gc()
}