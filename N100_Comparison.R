library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
require(MASS)

N100_Comparison <- function(loc){
  
  asteroids <- as.data.frame(read.csv(paste0("./",loc,"/",loc,"_Filtered_Asteroids.csv")))

  
  big_asteroids = subset(asteroids, N100 <= 0.35 | N100 <= 0.35 | N100 <= 0.35)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}