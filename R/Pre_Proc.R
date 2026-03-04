#' PreProc
#' @description Function that does necessary preprocessing for \code{\link{ProFAst::MultiDetect}}. Used to resize all images to same size, returning pixel matched images.
#' @param RA_DEC String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param image_directory String; absolute folder location from which to read fits files.
#' @param savepassthru Logical; should adjusted pixel matched images be saved to directory? Can greatly increase size on disk but useful if you want to import into other programs.
#'
#' @return List containing 3 pixel matched images
#' @export
#'
Pre_Proc <- function(RA_DEC, image_directory, savepassthru=FALSE){

  if(image_directory=="."){
    cat("Loading images from working directory.\n")
  }else{
  cat("Loading images from ",image_directory,"\n")
  }
  g=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*g*.fits"))), header=TRUE, ext=1)
  r=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*r*.fits"))), header=TRUE, ext=1)
  i1=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*i1*.fits"))), header=TRUE, ext=1)
  
  cat("Resizing images (1/2)\n")
  rx=ProPane::propaneWarp(r,keyvalues_out=g$keyvalues)
  cat("Resizing images (2/2)\n")
  i1x=ProPane::propaneWarp(i1,keyvalues_out=g$keyvalues)
  cat("Resizing done.\n")

  if(savepassthru == TRUE){
    Rfits::Rfits_write(rx, paste0(image_directory,"/rx.fits"))
    Rfits::Rfits_write(i1x, paste0(image_directory,"/i1x.fits"))
  }
  remove(r)
  remove(i1)
  return(list(g, rx, i1x))
}