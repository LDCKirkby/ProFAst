Edger <- function(segimcut, ID, invert=FALSE){
  image = segimcut$image
  
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
  
  if(invert==TRUE){
    image[image%in%ID]=0
  }else{
    image[image%notin%ID]=0
  }
  return(image)
}

Group_Cutter <- function(loc, computer) {
  wid <- 100.0
  box<-c(2*wid,2*wid)
  mulim<-22.0
  kids<-(0.339^2)*(10^(0.4*(0-mulim)))
  viking<-(0.339^2)*(10^(0.4*(30-mulim)))

  #Load in asteroid data
  asteroids = read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
  #Add edge variables
  group_edge_points = c("group_tl_RA", "group_tl_Dec", "group_tr_RA", "group_tr_Dec", "group_bl_RA", "group_bl_Dec", "group_br_RA", "group_br_Dec", "group_top_RA", "group_top_Dec", "group_bot_RA", "group_bot_Dec")
  segment_edge_points = c("segment_tl_RA", "segment_tl_Dec", "segment_tr_RA", "segment_tr_Dec", "segment_bl_RA", "segment_bl_Dec", "segment_br_RA", "segment_br_Dec", "segment_top_RA", "segment_top_Dec", "segment_bot_RA", "segment_bot_Dec")
  edge_points = c(group_edge_points, segment_edge_points)
  
  #Append extra columns to asteroids table
  names = c(colnames(asteroids), edge_points)
  asteroids[,edge_points] <- NA

  cat("Reading in segmentation map data\n")
  segim <- as.matrix(read.csv(paste0("./",loc,"/segim.csv")))
  cat("Generating groupim\n")
  groupim = profoundSegimGroup(segim = segim)
  groupim = groupim$groupim

  cat("Loading images\n")
    if("sabine" == tolower(computer)) {
      #Image Information
      g_image=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
      r_image=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
      i_image=Rfits_point(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
      
      #Header information
      g_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
      r_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
      i_hdr = Rfits_read_header(paste0("/Volumes/ThunderBay/WAVES/wavesdata/Wide/kids/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
    }else {
      #Image Information
      g_image=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"), header=TRUE, ext=1)
      r_image=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"), header=TRUE, ext=1)
      i_image=Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"), header=TRUE, ext=1)
      
      #Header Information
      g_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"))
      r_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_r_DMAG.fits"))
      i_hdr = Rfits_read_header(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_i1_DMAG.fits"))
  }

  #Make a directory to save the cutouts
  unlink(paste0("./",loc,"/Group_Cutouts"), recursive=TRUE)
  dir_create("./",loc,"/Group_Cutouts")

  for(i in 1:length(asteroids$segID)) {
    #Pulls out data for target asteroid
    target = asteroids[i,]
    ID = target$segID
    segID = target$segID
    groupID = target$groupID
    Colour = target$Colour
    #cat("Imaging object w/ groupID:", groupID, ", segID:", segID, ", Colour:", Colour, "\n")
    cat("**************************\n")
    astradec = target[c("RAcen", "Deccen")]
    astpos=as.integer(Rwcs_s2p(RA=astradec$RAcen, Dec=astradec$Deccen, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
    
    cutim_g = g_image[astpos,box=box]
    cutim_r = propaneWarp(r_image,keyvalues_out=cutim_g$keyvalues)
    cutim_i = propaneWarp(i_image,keyvalues_out=cutim_g$keyvalues)

    segimcut=magcutout(image = segim, loc=as.numeric(astpos), box=box, loc.type="image")
    groupcut=magcutout(image = groupim, loc=as.numeric(astpos), box=box, loc.type="image")

    edged_segimcut <- Edger(segimcut, ID)
    edged_groupcut <- Edger(groupcut, groupID)

    anti_segimcut <- Edger(segimcut, ID, invert=TRUE)
    anti_groupcut <- Edger(groupcut, groupID, invert=TRUE)

    png(filename=paste0("./",loc,"/Group_Cutouts/",loc,"_",Colour,segID,".png"))
    
    par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
    locut = c(median(cutim_r$imDat,na.rm=TRUE),median(cutim_g$imDat,na.rm=TRUE),median(cutim_i$imDat,na.rm=TRUE))
    group_col = switch(Colour, "g" = "seagreen2", "r" = "firebrick1", "i" = "skyblue")
    seg_col = switch(Colour, "g" = "green", "r" = "red", "i" = "blue")

    Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = cutim_r$keyvalues, Gkeyvalues = cutim_g$keyvalues, Bkeyvalues = cutim_i$keyvalues,
                xlab="Right Ascension (deg)",ylab="Declination (deg)", main = paste0("Asteroid ", ID), coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp = FALSE, hersh = FALSE, family="Arial")
    
    cat("Adding segment outlines\n")
    magimage(anti_groupcut,col=c(NA,rep("peru",max(im_dat$groupcut))),magmap = FALSE,add=TRUE,sparse=1,lwd=0.5)
    magimage(anti_segimcut,col=c(NA,rep("moccasin",max(im_dat$segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.25)

    magimage(edged_groupcut,col=c(NA,rep(group_col, max(edged_groupcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=1)
    magimage(edged_segimcut,col=c(NA,rep(seg_col, max(edged_segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)

    text(1,2*wid-50, col=groupcol, label=paste0("segID=",segID,"\nColour=",Colour), cex=2.0, pos=4, family="Arial")
    dev.off()

  }
  
}