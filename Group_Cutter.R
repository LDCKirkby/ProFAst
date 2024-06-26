Data_Cutout <- function(ast, image_data, groupcol, segcol){
  wid <- 200.0
  box<-c(2*wid,2*wid)
  mulim<-22.0
  kids<-(0.339^2)*(10^(0.4*(0-mulim)))
  viking<-(0.339^2)*(10^(0.4*(30-mulim)))
  
  galradec = ast[c("RAcen", "Deccen")]
  colour = paste0(ast$Colour, "_image")
  keyvalues = image_data[[ colour ]]
  keyvalues = keyvalues$keyvalues
  galpos=as.integer(Rwcs_s2p(RA=galradec$RAcen, Dec=galradec$Deccen, keyvalues=keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
  
  cutim_g=image_data$g_image[galpos,box=box] # Cuts g_image to be postage-stamp centered on target asteroid
  cutim_r=image_data$r_image[galpos,box=box] # Cuts r_image to be postage-stamp centered on target asteroid
  cutim_i=image_data$i_image[galpos,box=box] # Cuts i_image to be postage-stamp centered on target asteroid
  
  cat("Making cut images\n")
  segimcut=magcutout(image = image_data$segim, loc=as.numeric(galpos),box=box,loc.type="image")
  groupcut=magcutout(image = image_data$groupim, loc=as.numeric(galpos),box=box,loc.type="image")
  
  data = list("cutim_g" = cutim_g, "cutim_r" = cutim_r, "cutim_i" = cutim_i, "segimcut" = segimcut, "groupcut" = groupcut)
  return(data)
}

Edger <- function(groupID, segID, ast, data){
  cat("Finding the edges of segment images\n")
  
  images = list("segimcut", "groupcut")
  ID = list(segID, groupID)
  for(i in 1:2){
    image = data[[ images[[i]] ]]$image

    xrun=1:(dim(image)[1]-1)
    yrun=1:(dim(image)[2]-1)
    
    image_lb=image[xrun,yrun]
    image_lt=image[xrun+1,yrun]
    image_rt=image[xrun+1,yrun+1]
    image_rb=image[xrun,yrun+1]
    
    image_temp = (image_lb == image_lt) & (image_rt == image_rb) & (image_lb == image_rb) & (image_lt == image_rt)
    
    image_edge=matrix(0,dim(image)[1],dim(image)[2])
    
    image_edge[xrun,yrun]=image_edge[xrun,yrun]+image_temp
    image_edge[xrun+1,yrun]=image_edge[xrun+1,yrun]+image_temp
    image_edge[xrun+1,yrun+1]=image_edge[xrun+1,yrun+1]+image_temp
    image_edge[xrun,yrun+1]=image_edge[xrun,yrun+1]+image_temp
    
    image[image_edge==4]=0
    image[is.na(image)] <- 0
    
    asteroid_image = image
    asteroid_image[asteroid_image%notin%ID[[i]]]=0 #Makes two copies, one for the asteroid of interest
    image[image%in%ID[[i]]]=0                      #And one for all other objects in the frame
    
    data[[images[[i]]]] = image
    
    data[[ paste0("ast_",images[[i]]) ]] = asteroid_image
  }
  
  return(data)
}

Plotter <- function(loc, ast_dat, im_dat, groupcol, segcol, keyvalue_data){
  wid <- 200.0
  box<-c(2*wid,2*wid)
  mulim<-22.0
  kids<-(0.339^2)*(10^(0.4*(0-mulim)))
  viking<-(0.339^2)*(10^(0.4*(30-mulim)))
  
  cat("Printing ",ast_dat$colour,ast_dat$segID," postage stamp\n")
  png(filename=paste0("./",loc,"/Group_Cutouts/",ast_dat$colour,ast_dat$segID,".png"), family = "")
  
  par(mfrow=c(1,1),mar=c(3,3,2,2))
  
  locut = c(median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE),median(cutim_i$imDat,na.rm=TRUE))
  if(locut[[1]] > kids){
    locut[[1]] = kids
  }
  if(locut[[2]] > kids){
    locut[[2]] = kids
  }
  if(locut[[3]] > kids){
    locut[[3]] = kids
  }
  
  Rwcs_imageRGB(R=imdat_$cutim_r, G=im_dat$cutim_g, B=im_dat$cutim_i, Rkeyvalues = keyvalue_data$r_image$keyvalues, Gkeyvalues = keyvalue_data$g_image$keyvalues, Bkeyvalues = keyvalue_data$i_image$keyvalues, xlab="Right Ascension (deg)",ylab="Declination (deg)",coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp=FALSE, hersh = FALSE)#, grid = TRUE)
  
  cat("Adding segment outlines\n")
  magimage(im_dat$segimcut,col=c(NA,rep("moccasin",max(im_dat$segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.25)
  magimage(im_dat$groupcut,col=c(NA,rep("peru",max(im_dat$groupcut))),magmap = FALSE,add=TRUE,sparse=1,lwd=1)
  
  magimage(im_dat$ast_segimcut,col=c(NA,rep(segcol, max(im_dat$ast_segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)
  magimage(im_dat$ast_groupcut,col=c(NA,rep(groupcol, max(im_dat$ast_groupcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=1)
  
  # Uncomment if you want min & max points added to image
  # cat("Adding max & min points\n")
  # x = c(ast_dat$segment_tl_RA, ast_dat$segment_tr_RA , ast_dat$segment_bl_RA , ast_dat$segment_br_RA , ast_dat$segment_top_RA , ast_dat$segment_bot_RA, ast_dat$RAcen, ast_dat$RAmax)
  # y = c(ast_dat$segment_tl_Dec, ast_dat$segment_tr_Dec , ast_dat$segment_bl_Dec , ast_dat$segment_br_Dec , ast_dat$segment_top_Dec , ast_dat$segment_bot_Dec, ast_dat$Deccen, ast_dat$Decmax)
  # min_max_seg = data.frame(x, y)
  # 
  # points(min_max_seg, col=c("orangered" , "orange", "sienna1", "darkviolet", "mediumorchid" , "darkmagenta", "hotpink", "gold"), pch = 4, lwd = 3)
  # 
  # legend(x ="topright", legend = c("Top Right", "Top Left", "Bottom Right", "Bottom Left", "Average Top", "Average Bottom", "Center of Flux", "Max Flux"), pch = c(2,2,2,2), col = c("orangered" , "orange", "sienna1", "darkviolet", "mediumorchid" , "darkmagenta", "hotpink", "gold"))
  # 
  text(1,2*wid-50, col=groupcol, label=paste0("segID=",ast_dat$colour,ast_dat$segID), cex=2.0, pos=4, family = "")
  
  dev.off()
}


Postage_Stamper <- function(ast, image_data){
  groupID = ast$groupID
  segID = ast$segID
  colour = ast$Colour
  
  cat("Imaging groupID:", groupID, ", segID:",segID, ", colour:", colour,"\n")
  
  if(grepl(colour,"g") == TRUE){
    cut_image_data <- Data_Cutout(ast, image_data)
    
    cut_image_data <- Edger(groupID, segID, ast, cut_image_data)
    
    #Creates png and overlays points and segment/group outlines
    Image_Maker(loc, ast, cut_image_data, "seagreen2", "green", image_data)
    
  }else if(grepl(colour,"r") == TRUE){
    cut_image_data <- Data_Cutout(ast, image_data)
    
    cut_image_data <- Edger(groupID, segID, ast, cut_image_data)
    
    #Creates png and overlays points and segment/group outlines
    Image_Maker(loc, ast, cut_image_data, "firebrick1", "red2", image_data)
    
  }else if(grepl(colour,"i") == TRUE){
    cut_image_data <- Data_Cutout(ast, image_data)
    
    cut_image_data <- Edger(groupID, segID, ast, cut_image_data)
    
    #Creates png and overlays points and segment/group outlines
    Image_Maker(loc, ast, cut_image_data, "skyblue", "blue", image_data)
    
  }
}

Group_Cutter <- function(loc, computer){
  
  #Load in asteroid data
  asteroids = read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
  
  #Add edge variables
  group_edge_points = c("group_tl_RA", "group_tl_Dec", "group_tr_RA", "group_tr_Dec", "group_bl_RA", "group_bl_Dec", "group_br_RA", "group_br_Dec", "group_top_RA", "group_top_Dec", "group_bot_RA", "group_bot_Dec")
  segment_edge_points = c("segment_tl_RA", "segment_tl_Dec", "segment_tr_RA", "segment_tr_Dec", "segment_bl_RA", "segment_bl_Dec", "segment_br_RA", "segment_br_Dec", "segment_top_RA", "segment_top_Dec", "segment_bot_RA", "segment_bot_Dec")
  edge_points = c(group_edge_points, segment_edge_points)
  
  #Append extra columns to asteroids table
  names = c(colnames(asteroids), edge_points)
  asteroids[,edge_points] <- NA
  
  #Read in data for the locations  
  Data_reader <- function(loc, computer){
    cat("Reading in segmentation map data\n")
    segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
    cat("Generating groupim\n")
    groupim = profoundSegimGroup(segim = segim)
    
    cat("Loading images\n")
    if("sabine" == tolower(computer)){
      #Image Information
      g_image=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
      r=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
      i1=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
      
      #Header information
      g_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
      r_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
      i_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
    }else{
      #Image Information
      g_image=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
      r=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
      i1=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
      
      #Header Information
      g_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
      r_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
      i_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
    }
    r_image=propaneWarp(r,keyvalues_out= g_image$keyvalues)
    i_image=propaneWarp(i1,keyvalues_out= g_image$keyvalues)
    
    return(list("g_image" = g_image, "r_image" = r_image, "i_image" = i_image, "groupim" = groupim$groupim, "segim" = segim, "g_hdr" = g_hdr, "r_hdr" = r_hdr, "i_hdr" = i_hdr))
  }
  
  #Make a directory to save the cutouts
  Data <- Data_reader(loc, computer)
  unlink(paste0("./",loc,"/Group_Cutouts"), recursive=TRUE)
  dir_create("./",loc,"/Group_Cutouts")
  
  for(i in 1:length(asteroids$segID)){
    #Pulls out data for target asteroid
    target = asteroids[i,]
    Postage_Stamper(target, Data)
  }
  
}