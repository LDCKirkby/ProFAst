#' PreProc
#' @description Function that does necessary preprocessing for \code{\link[ProFAst]{MultiDetect}}. Used to resize all images to same size, returning pixel matched images.
#' @param RA_DEC String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param image_directory String; absolute folder location from which to read fits files.
#' @param savepassthru Logical; should adjusted pixel matched images be saved to directory? Can greatly increase size on disk but useful if you want to import into other programs.
#' @param imagenumber Numeric scalar; Number of input images to analyse. ProFAst will default to 3 input fields.
#' @param colours Character vector; List containing detection bands for input fields. ProFAst will default to looking for g,r & i bands unless told otherwise.
#' 
#' @return List containing 3 pixel matched images
#' @export
#'
Pre_Proc <- function(RA_DEC, image_directory, savepassthru=FALSE, imagenumber=3, colours=list("g","r","i")){

  if(image_directory=="."){
    cat("Loading images from working directory.\n")
  }else{
  cat("Loading images from ",image_directory,"\n")
  }

  for(i in 1:imagenumber){
    imcount = colours[[i]]
    tempimage = Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*",imcount,"*.fits"))), header=TRUE, ext=1)
    assign(paste0(imcount), tempimage)
  }
  # im1=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*",colours[[1]],"*.fits"))), header=TRUE, ext=1)
  # im2=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*",colours[[2]],"*.fits"))), header=TRUE, ext=1)
  # im3=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*",colours[[3]],"*.fits"))), header=TRUE, ext=1)
  shapedimages=list(get(paste0(colours[[1]])))
  print(shapedimages)
  for(i in 2:imagenumber){
    cat("Resizing images (",i-1,"/",imagenumber-1,")\n")
    cat("Removing old image:", paste0(colours[[i]]))
    assign(paste0(colours[[i]],"x"), ProPane::propaneWarp(get(paste0(colours[[i]])),keyvalues_out=get(paste0(colours[[1]]))$keyvalues))
    if(savepassthru==TRUE){
      Rfits::Rfits_write(get(paste0(colours[[i]],"x")),paste0(image_directory,"/",colours[[i]],"x.fits"))
    }
    append(shapedimages, get(paste0(colours[[i]],"x")))
    remove(get(paste0(colours[[i]])))
  }
  # cat("Resizing images (1/2)\n")
  # im2x=ProPane::propaneWarp(im2,keyvalues_out=im1$keyvalues)
  # cat("Resizing images (2/2)\n")
  # im3x=ProPane::propaneWarp(im3,keyvalues_out=im1$keyvalues)
  cat("Resizing done.\n")

  # if(savepassthru == TRUE){

  #   Rfits::Rfits_write(im2x, paste0(image_directory,"/",colours[[2]],"x.fits"))
  #   Rfits::Rfits_write(im3x, paste0(image_directory,"/",colours[[3]],"x.fits"))
  # }
  # remove(im2)
  # remove(im3)
  return(shapedimages)
}