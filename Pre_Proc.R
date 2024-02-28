library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
require(MASS)
library(dst)
library(Rwcs)
library(ProPane)
library(Rfits)
# cat("Enter RA_DEC to process:")
# loc = readLines(file("stdin"),1)
Pre_Proc <- function(loc){
  start_time <- Sys.time()
  # home = pwd()
  # location = paste0(home,"/")
  #
  cat("Loading images\n")
  g=Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
  r=Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
  i1=Rfits_read_image(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
  #i2=Rfits_read_image(paste0("/Volumes/WAVES/waves/wavesdata/kids/dr5/preprocessed/KIDS_",loc,"_i2_DMAG.fits"))
  #
  cat("Resizing images\n")
  rx=propaneWarp(r,keyvalues_out=g$keyvalues)
  remove(r)

  i1x=propaneWarp(i1,keyvalues_out=g$keyvalues)
  remove(i1)
  #i2x=propaneWarp(i2,keyvalues_out=g$keyvalues)
  # 
  # cat("Printing images\n")
  #Removing this line as it creates storage issues when running on large image numbers
  # dir.create(paste0(location,"Fits_files/",loc))
  # Rfits_write_image(g, filename = paste0(location,"Fits_files/",loc,"/g.fits"))
  # Rfits_write_image(rx, filename = paste0(location,"Fits_files/",loc,"/rx.fits"))
  # Rfits_write_image(i1x, filename = paste0(location,"Fits_files/",loc,"/i1x.fits"))
  # #Rfits_write_image(i2x, filename = paste0(location,"Fits_files/",loc,"/i2x.fits"))
  

  
  
  end_time <- Sys.time()
  data = list(list(g, rx, i1x), end_time-start_time)
  return(data)
}