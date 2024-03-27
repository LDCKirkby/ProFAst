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
contplot=function(ast_data, i=NULL, groupimage, groupcol, header=NULL){
  
  if(is.null(i) == FALSE){
    ID = ast_data$groupID[i]
    groupimage[groupimage%notin%ID]=0
    
    cat(length(which(groupimage%in%ID)), "\n")
    cat("Are there points with ID in groupimage?: ", ID%in%groupimage, "\n\n")
    if(ID%in%groupimage == FALSE){
      ast_data = contplot(ast_data, i = NULL, groupimage, "skyblue", header=NULL) 
      return(ast_data)
    }
    
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
    top_left <- obj_points[which.min(obj_points[, 1] - obj_points[, 2]), ]
    bottom_right <- obj_points[which.max(obj_points[, 1] - obj_points[, 2]), ]
    bottom_left <- obj_points[which.min(obj_points[, 1] + obj_points[, 2]), ]
    ave_top <- c((top_right[[1]] + bottom_right[[1]])/2 , (top_right[[2]] + bottom_right[[2]])/2)
    ave_bottom <- c((top_left[[1]] + bottom_left[[1]])/2 , (top_left[[2]] + bottom_left[[2]])/2)
    
  
    ast_data$tl_RA[i] = xy2radec(top_left[[1]], top_left[[2]], header)[1]
    ast_data$tl_Dec[i] = xy2radec(top_left[[1]], top_left[[2]], header)[2]
    ast_data$tr_RA[i] = xy2radec(top_right[[1]], top_right[[2]], header)[1]
    ast_data$tr_Dec[i] = xy2radec(top_right[[1]], top_right[[2]], header)[2]
    ast_data$bl_RA[i] = xy2radec(bottom_left[[1]], bottom_left[[2]], header)[1]
    ast_data$bl_Dec[i] = xy2radec(bottom_left[[1]], bottom_left[[2]], header)[2]
    ast_data$br_RA[i] = xy2radec(bottom_right[[1]], bottom_right[[2]], header)[1]
    ast_data$br_Dec[i] = xy2radec(bottom_right[[1]], bottom_right[[2]], header)[2]
    ast_data$top_RA[i] = xy2radec(ave_top[[1]], ave_top[[2]], header)[1]
    ast_data$top_Dec[i] = xy2radec(ave_top[[1]], ave_top[[2]], header)[2]
    ast_data$bot_RA[i] = xy2radec(ave_bottom[[1]], ave_bottom[[2]], header)[1]
    ast_data$bot_Dec[i] = xy2radec(ave_bottom[[1]], ave_bottom[[2]], header)[2]
  
    x = c(top_right[[1]],bottom_right[[1]],top_left[[1]],bottom_left[[1]],ave_top[[1]],ave_bottom[[1]])
    y = c(top_right[[2]],bottom_right[[2]],top_left[[2]],bottom_left[[2]],ave_top[[2]],ave_bottom[[2]])
    locations = cbind(x,y)
  
    #plot(NA, xlim = c(1, ncol(groupimage)), ylim = c(1, nrow(groupimage)), xlab = "x", ylab = "y", xaxt = "n", yaxt = "n")
    magimage(groupimage,col=c(NA,rep(groupcol,max(groupimage))),magmap=FALSE,add=TRUE,sparse=1)
    points(locations, col=c("#b967ff","#fffb96","#00FFFF","#FF00FF", "#FFA500", "#05ffa1"), add=TRUE, pch = 4, lwd = 3)
    legend(x ="topright", legend = c("Top Right", "Bottom Right", "Top Left", "Bottom Left", "Right Midpoint", "Left Midpoint"), pch = c(3,3,3,3), col = c("#b967ff","#fffb96","#00FFFF","#FF00FF", "#FFA500", "#05ffa1"))
  
    return(ast_data)
  }else{
    for(groupID in ast_data){
    
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
    
      magimage(groupimage,col=c(NA,rep(groupcol,max(groupimage))),magmap=FALSE,add=TRUE,sparse=1, lwd = 0.5)
    
    }
  }
}