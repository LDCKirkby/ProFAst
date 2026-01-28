#' ProFAst
#' @description${1:Full execution of detection and filter runs. Extra input paramters are passed through to other function calls to adjust detection or filter processes.}
#' @param${1:RA_DEC} ${2:Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).}
#' @param${1:image_directory} ${2:Location of input fits files.}
#' @param${1:skycut} ${2:Numeric scalar; the lowest threshold to make on the image in units of the skyRMS. Passed to profoundMultiBand.}
#' @param${1:pixcut} ${2:Integer scalar; the number of pixels required to identify an object. Passed to profoundMultiBand.}
#' @param${1:smooth} ${2:Logical; should smoothing be done on the target image? Passed to profoundMultiBand.}
#' @param${1:sigma} ${2:Numeric scalar; standard deviation of the blur used when smooth=TRUE. Passed to profoundMultiBand}
#' @param${1:reltol} ${2:Numeric scalar; only relevant for watershed='ProFound'. A modifier to the tolerance, modifying it by the ratio of the segment peak flux divided by the saddle point flux to the power reltol. The default means the reltol has no effect since this modifier becomes 1. A larger value of reltol means segments are more aggressively merged together. Can be (and often should be in practice) negative. The effect of using reltol and setting to negative is that the central brighter parts of galaxies are kept together in a single segment, and deblending is more common on the outskirts (where it should have less effect on the overall flux). The principle is that we need to be very confident a bright source needs to be split near its peak flux, but can be more aggressive in the outskirts. Passed to profoundMultiBand.}
#' @param${1:tolerance} ${2:Numeric scalar; the minimum height of the object in the units of skyRMS between its highest point (seed) and the point where it contacts another object (checked for every contact pixel). If the height is smaller than the tolerance, the object will be combined with one of its neighbours, which is the highest. The range 1-5 offers decent results usually. Passed to profoundMultiBand.}
#' @param${1:ext} ${Numeric scalar; radius of the neighbourhood in pixels for the detection of neighbouring objects. Higher value smooths out small objects. Passed to profoundMultiBand.}

#' 
#'
#' @return Result of calculation
#' @export
#'
ProFAst <- function(RA_DEC, image_directory,
                     skycut = 0.6,
                     pixcut = 15, 
                     smooth = TRUE, 
                     sigma = 2, 
                     reltol=-10, 
                     tolerance =1, 
                     ext=7, 
                     flux_value=1, 
                     edge_buffer=0.001, 
                     axrat_value=0.35, 
                     N100_lower=150, 
                     N100_upper=2250,
                     save_temp_files=FALSE){

dir = getwd()
cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
frames <- Pre_Proc(RA_DEC, image_directory)

MultiDetect(RA_DEC, frames, skycut, pixcut, smooth, sigma, reltol, tolerance, ext)

Flux_Filter(RA_DEC, flux_value, edge_buffer)

Axrat_Filter(RA_DEC, axrat_value)

N100_Filter(RA_DEC, N100_lower, N100_upper)

Group_Cutter(RA_DEC, image_directory)

warnings()
}
#Uncomment to remove any unwanted files
#file.remove(paste0("./",RA_DEC,"/stacked.rds"))
#file.remove(paste0("./",RA_DEC,"/allcati.csv"))
#file.remove(paste0("./",RA_DEC,"/groupcati.csv"))
#file.remove(paste0("./",RA_DEC,"/objectcati.csv"))