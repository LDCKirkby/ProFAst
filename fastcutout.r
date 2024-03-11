#
# Plot segim for individual object
#
library(celestial)
library(devtools)
library(Cairo)
library(Rfits)
library(Rwcs)
library(ProFound)
library('magicaxis')
library('data.table')
library('plotrix')
require(foreign)
require(MASS)
#
contplot=function(segsegIDs, segim, segcol, segwid, linewd){
  `%notin%`<-Negate(`%in%`)
  segim[segim%notin%segsegIDs]=0
  
  groupimage = which(segim == segsegIDs, arr.ind=TRUE)
  max_x = max(groupimage[1])
  min_x = min(groupimage[1])
  max_y = max(groupimage[2])
  min_y = min(groupimage[2])
  
  
  
  
  
  xrun=1:(dim(segim)[1]-1)
  yrun=1:(dim(segim)[2]-1)
  
  segim_lb=segim[xrun,yrun]
  segim_lt=segim[xrun+1,yrun]
  segim_rt=segim[xrun+1,yrun+1]
  segim_rb=segim[xrun,yrun+1]
  
  segim_temp = (segim_lb == segim_lt) & (segim_rt == segim_rb) & (segim_lb == segim_rb) & (segim_lt == segim_rt)
  
  segim_edge=matrix(0,dim(segim)[1],dim(segim)[2])
  
  segim_edge[xrun,yrun]=segim_edge[xrun,yrun]+segim_temp
  segim_edge[xrun+1,yrun]=segim_edge[xrun+1,yrun]+segim_temp
  segim_edge[xrun+1,yrun+1]=segim_edge[xrun+1,yrun+1]+segim_temp
  segim_edge[xrun,yrun+1]=segim_edge[xrun,yrun+1]+segim_temp
  
  segim[segim_edge==4]=0
  
  
  magimage(segim,col=c(NA,rep(segcol,max(segim))),magmap=FALSE,add=TRUE,sparse=1)
  magimage(x=max_x, y=max_y, add = TRUE, col = "white")
  magimage(x=min_x, y=min_y, add = TRUE, col = "hotpink")
  }
#