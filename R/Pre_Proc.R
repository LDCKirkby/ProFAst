#' PreProc
#' @description${1:Function that does necessary preprocessing for \code{\link{Multi_Detect}}. Used to resize all images to same size, returning pixel matched images.}
#' @param${1:RA_DEC} ${2:String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).}
#' @param${1:image_directory} ${2:String; absolute folder location from which to read fits files.}
#' @param${1:savepassthru} ${2:Logical; should adjusted pixel matched images be saved to directory? Can greatly increase size on disk but useful if you want to import into other programs.}
#'
#' @return List containing 3 pixel matched images
#' @export
#'
Pre_Proc <- function(RA_DEC, image_directory, savepassthru=FALSE){

  cat("Loading images from ",image_directory,"\n")
  g=Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*g*.fits"))), header=TRUE, ext=1)
  r=Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*r*.fits"))), header=TRUE, ext=1)
  i1=Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*i1*.fits"))), header=TRUE, ext=1)
  
  cat("Resizing images\n")
  rx=propaneWarp(r,keyvalues_out=g$keyvalues)
  remove(r)

  i1x=propaneWarp(i1,keyvalues_out=g$keyvalues)
  remove(i1)
  return(list(g, rx, i1x))
}