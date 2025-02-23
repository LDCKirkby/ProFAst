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
    g=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
    r=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
    i1=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
    #i2=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i2_DMAG.fits"), header=TRUE, ext=1)
  }
  
  cat("Resizing images\n")
  rx=propaneWarp(r,keyvalues_out=g$keyvalues)
  remove(r)

  i1x=propaneWarp(i1,keyvalues_out=g$keyvalues)
  remove(i1)
  
  #Uncomment to add in extra i image
  #NOT IMPLEMENTED IN OTHER FUNCTIONS
  #i2x=propaneWarp(i2,keyvalues_out=g$keyvalues)
  #remove(i2)
  
  return(list(g, rx, i1x))
}