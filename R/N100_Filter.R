#' N100_filter
#' @description Filter sources in data frame based on N100 value (pixel size).
#' @param RA_DEC Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param N100_lower Small pixel count cutoff. Objects with fewer pixels than the threshold are assumed to be errors and excluded.
#' @param N100_upper Large pixel count cutoff. Objects with more pixels than the threshold are assumed to be errors and excluded.
#' @param filtered_data List; Filtered astronometric data. If not supplied will look for appropriate file in working directory.
#' 
#' @return Data frame containing all N100 filtered sources.
#' @export
#'
N100_Filter <- function(RA_DEC, N100_lower=150, N100_upper=2250, filtered_data=NULL){
  if(is.null(filtered_data)){
    filtered_data = utils::read.csv(paste0("./", RA_DEC,"/",RA_DEC,"_Axrat_Filtered_Objects.csv"))
  }
  cat("*********\n")
  cat("Applying N100 filter\n")
  cat("*********\n\n")
  
  #Npix is the entire size of the group segment. Should use that but I only just found it
  #Idk if i can be bothered adding it tbh
  cat("*********\n")
  cat("Performing small N100 filter\n")
  large_pass = subset(filtered_data, subset = filtered_data$Npix >= N100_lower)
  
  cat("Removed ", length(filtered_data$groupID) - length(large_pass$groupID), " small errors\n")
  
  cat("Performing large N100 filter\n")
  N100_filtered_asteroids = subset(large_pass, subset = large_pass$Npix <= N100_upper)
  
  cat("Removed ", length(large_pass$groupID) - length(N100_filtered_asteroids$groupID), " large errors\n")
  
  cat("Final number of ", length(N100_filtered_asteroids$groupID), " possible asteroids\n")
  
  utils::write.csv(N100_filtered_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_Auto_Filtered_Asteroids.csv"), row.names=FALSE)
  cat("Writing to " ,paste0("./", RA_DEC,"/",RA_DEC,"_Auto_Filtered_Asteroids.csv"),"\n")
  cat("*********\n\n")

  return(N100_filtered_asteroids)
}