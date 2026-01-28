#' Axrat_filter
#' @description${1:Filter sources in data frame based on axial ratio value.}
#' @param${1:RA_DEC} ${2:String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).}
#' @param${1:axrat_value} ${2:Numeric scalar; Filter Cutoff Value. Sources with an axial ratio lower than the value are assumed to not be asteroids and are removed.}
#' @param${1:savepassthru} ${2:Logical; }
#' 
#' @export
#'
Axrat_Filter <- function(RA_DEC, axrat_value=0.35, savepassthru=FALSE){

possible_asteroids = read.csv(paste0("./",RA_DEC,"/",RA_DEC,"_Possible_Asteroids.csv"), fill = TRUE)
cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= axrat_value | axrat_rxt <= axrat_value | axrat_i1xt <= axrat_value)
filtered_asteroids = setorder(filtered_asteroids, "groupID")

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
