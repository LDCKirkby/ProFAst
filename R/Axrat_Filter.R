#' Axrat_filter
#' @description Filter sources in data frame based on axial ratio value.
#' @param RA_DEC String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param axrat_value Numeric scalar; Filter Cutoff Value. Sources with an axial ratio lower than the value are assumed to not be asteroids and are removed.
#' @param savepassthru Logical; should intermediate files be saved to directory? Can greatly increase size on disk but useful to see which objects are being filtered out.
#' 
#' @return Data frame containing all axrat filtered sources.
#' @export
#'
Axrat_Filter <- function(RA_DEC, axrat_value=0.35, savepassthru=FALSE){

possible_asteroids = read.csv(paste0("./",RA_DEC,"/",RA_DEC,"_Possible_Asteroids.csv"), fill = TRUE)
cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= axrat_value | axrat_rxt <= axrat_value | axrat_i1xt <= axrat_value)
filtered_asteroids = data.table::setorder(filtered_asteroids, "groupID")

cat("*********\n")
cat("Filtered to ", length(filtered_asteroids$axrat_gt), "potential asteroids\n")
cat("*********\n\n")

cat("*********\n")
cat("Writing to ", paste0("./", RA_DEC,"/",RA_DEC,"Filtered_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(filtered_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_Filtered_Asteroids.csv"), row.names=FALSE)

rm(possible_asteroids, filtered_asteroids) 
gc()
}
