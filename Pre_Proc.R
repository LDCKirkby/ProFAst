Pre_Proc <- function(loc, computer){

  cat("Loading images from\n")
  if("sabine" == tolower(computer)){
    cat(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed"),"\n")
    g=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
    r=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
    i1=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
    #i2=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i2_DMAG.fits"), header=TRUE, ext=1)
  }else{
    cat(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed"),"\n")
    g=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
    r=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
    i1=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
    #i2=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i2_DMAG.fits"), header=TRUE, ext=1)
  }
  
  cat("Resizing images\n")
  rx=propaneWarp(r,keyvalues_out=g$keyvalues)
  remove(r)

  i1x=propaneWarp(i1,keyvalues_out=g$keyvalues)
  remove(i1)
  #i2x=propaneWarp(i2,keyvalues_out=g$keyvalues)
  #remove(i2)
  
  #Removing this line as it creates storage issues when running on large image numbers
  # dir.create(paste0(location,"Fits_files/",loc))
  # Rfits_write_image(g, filename = paste0(location,"Fits_files/",loc,"/g.fits"))
  # Rfits_write_image(rx, filename = paste0(location,"Fits_files/",loc,"/rx.fits"))
  # Rfits_write_image(i1x, filename = paste0(location,"Fits_files/",loc,"/i1x.fits"))
  # #Rfits_write_image(i2x, filename = paste0(location,"Fits_files/",loc,"/i2x.fits"))

  return(list(g, rx, i1x))
}