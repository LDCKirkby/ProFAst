#' N100_filter
#' @description${1:Filter sources in data frame based on N100 value (pixel size). .}
#' @param${1:RA_DEC} ${2:Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).}
#' @param${1:N100_lower} ${2:Small pixel count cutoff. Objects with fewer pixels than the threshold are assumed to be errors and excluded.}
#' @param${1:N100_upper} ${2:Large pixel count cutoff. Objects with more pixels than the threshold are assumed to be errors and excluded.}
#'
#' @export
#'
N100_Filter <- function(RA_DEC, N100_lower=150, N100_upper=2250){
  
  cat("*********\n")
  cat("Applying N100 filter\n")
  cat("*********\n\n")
  
  filtered_asteroids = read.csv(paste0("./", RA_DEC,"/",RA_DEC,"_Filtered_Asteroids.csv"))
  #Npix is the entire size of the group segment. Should use that but I only just found it
  #Idk if i can be bothered adding it tbh
  #N100 filter, filter done in all bands for completeness, even though they are they are all the same value
  cat("*********\n")
  cat("Performing small N100 filter\n")
  large_pass = subset(filtered_asteroids, N100 >= N100_lower)
  
  cat("Removed ", length(filtered_asteroids$groupID) - length(large_pass$groupID), " small errors\n")
  
  cat("Performing large N100 filter\n")
  N100_filtered_asteroids = subset(large_pass, N100 <= N100_upper)
  
  cat("Removed ", length(large_pass$groupID) - length(N100_filtered_asteroids$groupID), " large errors\n")
  
  cat("Final number of ", length(N100_filtered_asteroids$groupID), " possible asteroids\n")
  
  write.csv(N100_filtered_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_N100_Filtered_Asteroids.csv"), row.names=FALSE)
  cat("Writing to " ,paste0("./", RA_DEC,"/",RA_DEC,"_N100_Filtered_Asteroids.csv"),"\n")
  cat("*********\n\n")
  
  rm(N100_filtered_asteroids, filtered_asteroids, large_pass) 
  gc()
}