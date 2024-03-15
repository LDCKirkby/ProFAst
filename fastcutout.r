#
# Plot groupimage for individual object
#
# library(celestial)
# library(devtools)
# library(Cairo)
# library(Rfits)
# library(Rwcs)
# library(ProFound)
# library('magicaxis')
# library('data.table')
# library('plotrix')
# require(foreign)
# require(MASS)
#
contplot=function(ID, groupimage, groupcol, target = FALSE){
  print(ID)
  if(target == FALSE){
    groupimage[groupimage%in%ID]=0
  } else{groupimage[groupimage%notin%ID]=0}
  
  
  xrun=1:(dim(groupimage)[1]-1)
  yrun=1:(dim(groupimage)[2]-1)
  
  groupimage_lb=groupimage[xrun,yrun]
  groupimage_lt=groupimage[xrun+1,yrun]
  groupimage_rt=groupimage[xrun+1,yrun+1]
  groupimage_rb=groupimage[xrun,yrun+1]
  
  groupimage_temp = (groupimage_lb == groupimage_lt) & (groupimage_rt == groupimage_rb) & (groupimage_lb == groupimage_rb) & (groupimage_lt == groupimage_rt)
  
  groupimage_edge=matrix(0,dim(groupimage)[1],dim(groupimage)[2])
  
  groupimage_edge[xrun,yrun]=groupimage_edge[xrun,yrun]+groupimage_temp
  groupimage_edge[xrun+1,yrun]=groupimage_edge[xrun+1,yrun]+groupimage_temp
  groupimage_edge[xrun+1,yrun+1]=groupimage_edge[xrun+1,yrun+1]+groupimage_temp
  groupimage_edge[xrun,yrun+1]=groupimage_edge[xrun,yrun+1]+groupimage_temp
  
  groupimage[groupimage_edge==4]=0
  
  obj_points <- which(groupimage == ID, arr.ind = TRUE)
  
  # Find top-right, top-left, bottom-right, and bottom-left points
  top_right <- obj_points[which.max(obj_points[, 1] + obj_points[, 2]), ]
  top_left <- obj_points[which.min(obj_points[, 1] + obj_points[, 2]), ]
  bottom_right <- obj_points[which.max(obj_points[, 1] - obj_points[, 2]), ]
  bottom_left <- obj_points[which.min(obj_points[, 1] - obj_points[, 2]), ]
  
  cat(top_left)
  cat(top_right)
  cat(bottom_left)
  cat(bottom_right)
  
  big = matrix(ncol = ncol(groupimage), nrow = nrow(groupimage))
  big[top_right[[1]],top_right[[2]]] = 10
  big[top_left[[1]],top_left[[2]]] = 2
  big[bottom_right[[1]],bottom_right[[2]]] = 3
  big[bottom_left[[1]],bottom_left[[2]]] = 4
  
  x = c(top_right[[1]],bottom_right[[1]],top_left[[1]],bottom_left[[1]])
  y = c(top_right[[2]],bottom_right[[2]],top_left[[2]],bottom_left[[2]])
  locations = cbind(x,y)

  #plot(NA, xlim = c(1, ncol(groupimage)), ylim = c(1, nrow(groupimage)), xlab = "x", ylab = "y", xaxt = "n", yaxt = "n")
  magimage(groupimage,col=c(NA,rep(groupcol,max(groupimage))),magmap=FALSE,add=TRUE,sparse=1)
  points(locations, col=c("hotpink","pink","orange","orangered"), add=TRUE, pch = 4, lwd = 3)
  legend(x ="topright", legend = c("Top Right", "Bottom Right", "Top Left", "Bottom Left"), pch = c(3,3,3,3), col = c("hotpink","pink","orange","orangered"))
  
  #magimage(big, col=c("hotpink","violet","wheat","yellow"), magmap=FALSE,add=TRUE, sparse=1, pch = 4)

}
#