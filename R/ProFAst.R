#' ProFAst
#' @description${1:Full execution of detection and filter runs. Extra input paramters are passed through to other function calls to adjust detection or filter processes.}
#' @param${1:RA_DEC} ${2:Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).}
#' @param${1:image_directory} ${2:Location of input fits files.}
#'
#' @return Result of calculation
#' @export
#'
ProFAst <- function(RA_DEC, image_directory, skycut = 0.6, pixcut = 15, smooth = TRUE, sigma = 2, reltol=-10, tolerance =1, ext=7, flux_value=1, edge_buffer=0.001, axrat_value=0.35, N100_lower=150, N100_upper=2250){

dir = getwd()
cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
frames <- Pre_Proc(RA_DEC, image_directory)

Multi_Detect(RA_DEC, frames, skycut, pixcut, smooth, sigma, reltol, tolerance, ext)

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