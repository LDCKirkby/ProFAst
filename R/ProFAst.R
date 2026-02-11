#' ProFAst
#' @description Full execution of detection and filter runs. Extra input paramters are passed through to other function calls to adjust detection or filter processes.
#' @param RA_DEC Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param image_directory Location of input fits files.
#' @param savepassthru Logical; should intermediate files be saved to directory? Can greatly increase size on disk but useful to see which objects are being filtered out.
#' @param skycut Numeric scalar; the lowest threshold to make on the image in units of the skyRMS. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param pixcut Integer scalar; the number of pixels required to identify an object. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param smooth Logical; should smoothing be done on the target image? Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param sigma Numeric scalar; standard deviation of the blur used when smooth=TRUE. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param reltol Numeric scalar; only relevant for watershed='ProFound'. A modifier to the tolerance, modifying it by the ratio of the segment peak flux divided by the saddle point flux to the power reltol. The default means the reltol has no effect since this modifier becomes 1. A larger value of reltol means segments are more aggressively merged together. Can be (and often should be in practice) negative. The effect of using reltol and setting to negative is that the central brighter parts of galaxies are kept together in a single segment, and deblending is more common on the outskirts (where it should have less effect on the overall flux). The principle is that we need to be very confident a bright source needs to be split near its peak flux, but can be more aggressive in the outskirts. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param tolerance Numeric scalar; the minimum height of the object in the units of skyRMS between its highest point (seed) and the point where it contacts another object (checked for every contact pixel). If the height is smaller than the tolerance, the object will be combined with one of its neighbours, which is the highest. The range 1-5 offers decent results usually. Passed to \code{\link{ProFound::profoundMultiBand}}
#' @param ext Numeric scalar; radius of the neighbourhood in pixels for the detection of neighbouring objects. Higher value smooths out small objects. Passed to \code{\link{ProFound::profoundMultiBand}}.
#' @param flux_value Numeric scalar; Flux filter cutoff value. Determined by dividing query filter flux by sum of fluxes in other bands , i.e. (g/r+i). Sources with an flux ratio lower than the value are assumed to not be asteroids and are removed.
#' @param edge_buffer Numeric scalar; Edge boundary value within which positive hits are ignored. Useful if images are artificially extended as is done in \code{\link{PreProc}}.
#' @param axrat_value Numeric scalar; Axial ratio filter cutoff value. Sources with an axial ratio lower than the value are assumed to not be asteroids and are removed. See \code{\link{ProFound}} documentation on how axrat value is determined.
#' @param N100_lower Numeric scalar; N100 filter lower cutoff value. Sources with a N100 pixel count lower than the value are assumed too small to be asteroids and are removed. See \code{\link{ProFound}} documentation on how N100 value is calculated.
#' @param N100_upper Numeric scalar; N100 filter upper cutoff calue. Sources with a N100 pixel count greater than the value are assumed too large to be asteroids and are removed. See \code{\link{ProFound}} documentation on how N100 value is calculated.
#'
#' @export
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
                     savepassthru=FALSE){

cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
frames <- Pre_Proc(RA_DEC, image_directory, savepassthru)

data <- MultiDetect(RA_DEC, frames, skycut, pixcut, smooth, sigma, reltol, tolerance, ext, savepassthru)

flux_filtered <- Flux_Filter(RA_DEC, flux_value, edge_buffer, savepassthru)

axrat_filtered <- Axrat_Filter(RA_DEC, axrat_value, savepassthru)

N100_filtered <- N100_Filter(RA_DEC, N100_lower, N100_upper, savepassthru)

Group_Cutter(RA_DEC, image_directory)

warnings()
}