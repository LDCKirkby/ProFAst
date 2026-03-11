`%notin%` <- function(x,y) !(x %in% y)

edger <- function(segimcut, ID, inverted){
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

  if(inverted==TRUE){
    image[image%in%ID]=0
  }else if(inverted==FALSE){
    image[image %notin% ID]=0
  }
  return(image)
}
#' Group_Cutter
#' @description Postage stamp maker. Produces cutouts of fits images, with segmentation mask data overlaid.
#' @param RA_DEC String; Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param image_directory String; Location of input fits files. Default behaviour is to check working directory for suitable fits images.
#' @param asteroids Numeric Data Frame; Optional. Data frame containing astronomical bodies to produce cutouts of. If not supplied will look for appropriate file in working directory.
#' @param frames List; Optional. List containing three pixel-matched astronomical images. Expected order within list is (g,r,i), returned by \code{\link[ProFAst]{Pre_Proc}}. If not supplied will look for appropriate file in working directory and perform pixel matching. Can greatly increase execution time if images are sufficiently large.
#' @param segim Integer Matrix; Optional. Matrix containing object segmentation masks for an image. If not supplied will look for appropriate file in working directory. 
#' @param groupim Integer Matrix; Optional. Matrix containing group segmentation masks for an image. If not supplied will look for appropriate file in working directory. 
#' @param imagenumber Numeric scalar; Number of input images to analyse. ProFAst will default to 3 input fields.
#' @param colours Character vector; List containing detection bands for input fields. ProFAst will default to looking for g,r & i bands unless told otherwise.
#' 
#' @import ProFound
#' @import ProPane
#' @import Rfits
#' @import Rwcs
#' @import magicaxis
#' @import Cairo
#' @import MASS
#' @import data.table
#' @export
Group_Cutter <- function(RA_DEC, image_directory, asteroids=NULL, frames=NULL, segim=NULL, groupim=NULL, imagenumber=3, colours=c("g","r","i")) {
  if(is.null(asteroids)){
    asteroids = utils::read.csv(paste0("./",RA_DEC,"/",RA_DEC,"_Auto_Filtered_Asteroids.csv"))
  }
  if(is.null(frames)){
    cat("Pixel matched images not supplied. ")
    if(image_directory=="."){
      cat("Loading images from working directory.\n")
    }else{
      cat("Loading images from ",image_directory,"\n")
    }
    g_image=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*g*.fits"))), header=TRUE, ext=1)
    r_image=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*r*.fits"))), header=TRUE, ext=1)
    i_image=Rfits::Rfits_point(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*i1*.fits"))), header=TRUE, ext=1)
  }
  if(is.null(segim)){
    cat("segim not supplied. Reading in segmentation map data\n")
    segim <- as.matrix(utils::read.csv(paste0("./",RA_DEC,"/segim.csv")))
  }
  if(is.null(groupim)){
    cat("groupim not supplied. Generating groupim\n")
    groupim <- as.matrix(utils::read.csv(paste0("./",RA_DEC,"/groupim.csv")))
  }
  g_hdr = Rfits::Rfits_read_header(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*g*.fits"))))
  r_hdr = Rfits::Rfits_read_header(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*r*.fits"))))
  i_hdr = Rfits::Rfits_read_header(Sys.glob(file.path(image_directory,paste0("*",RA_DEC,"*i1*.fits"))))
  wid <- 200.0
  box<-c(2*wid,2*wid)
  mulim<-22.0
  kids<-(0.339^2)*(10^(0.4*(0-mulim)))
  viking<-(0.339^2)*(10^(0.4*(30-mulim)))

  #Make a directory to save the cutouts
  unlink(paste0("./",RA_DEC,"/Group_Cutouts"), recursive=TRUE)
  fs::dir_create("./",RA_DEC,"/Group_Cutouts")

  for(i in 1:length(asteroids$segID)) {
    #Pulls out data for target asteroid
    target = asteroids[i,]
    ID = target$segID
    segID = target$segID
    groupID = target$groupID
    Colour = target$Colour
    cat("Imaging object ",i,", groupID:", groupID, ", segID:", segID, ", Colour:", Colour, "\n")
    cat("**************************\n")
    astradec = target[c("RAcen", "Deccen")]
    astpos=as.integer(Rwcs::Rwcs_s2p(RA=astradec$RAcen, Dec=astradec$Deccen, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))

    cutim_g = g_image[astpos,box=box]
    cutim_r = ProPane::propaneWarp(r_image,keyvalues_out=cutim_g$keyvalues)
    cutim_i = ProPane::propaneWarp(i_image,keyvalues_out=cutim_g$keyvalues)

    segimcut=magicaxis::magcutout(image = segim, loc=as.numeric(astpos), box=box, loc.type="image")
    groupcut=magicaxis::magcutout(image = groupim, loc=as.numeric(astpos), box=box, loc.type="image")

    edged_segimcut <- edger(segimcut, ID, inverted=FALSE)
    edged_groupcut <- edger(groupcut, groupID, inverted=FALSE)

    anti_segimcut <- edger(segimcut, ID, inverted=TRUE)
    anti_groupcut <- edger(groupcut, groupID, inverted=TRUE)

    png::png(filename=paste0("./",RA_DEC,"/Group_Cutouts/",RA_DEC,"_",Colour,segID,".png"))

    graphics::par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
    locut = c(stats::median(cutim_r$imDat,na.rm=TRUE),stats::median(cutim_g$imDat,na.rm=TRUE),stats::median(cutim_i$imDat,na.rm=TRUE))
    if(max(locut) > kids){
      locut = c(kids,kids, kids)
    }
    group_col = switch(Colour, "g" = "seagreen2", "r" = "firebrick1", "i" = "skyblue")
    seg_col = switch(Colour, "g" = "green", "r" = "red", "i" = "blue")

    Rwcs::Rwcs_imageRGB(R=cutim_r, G=cutim_g, B=cutim_i, Rkeyvalues = cutim_r$keyvalues, Gkeyvalues = cutim_g$keyvalues, Bkeyvalues = cutim_i$keyvalues,
                xlab="Right Ascension (deg)",ylab="Declination (deg)", main = paste0("Asteroid ", ID), coord.type="deg",locut=locut, hicut=c(kids,kids,kids) ,type="num", dowarp = FALSE, hersh = FALSE, family="Arial")

    cat("Adding segment outlines\n")
    magixaxis::magimage(anti_groupcut,col=c(NA,rep("peru",max(anti_groupcut))),magmap = FALSE,add=TRUE,sparse=1,lwd=0.5)
    magixaxis::magimage(anti_segimcut,col=c(NA,rep("moccasin",max(anti_segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.25)

    magixaxis::magimage(edged_groupcut,col=c(NA,rep(group_col, max(edged_groupcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=1)
    magixaxis::magimage(edged_segimcut,col=c(NA,rep(seg_col, max(edged_segimcut))),magmap=FALSE,add=TRUE,sparse=1,lwd=0.5)

    graphics::text(1,2*wid-50, col=group_col, label=paste0("segID=",ID,"\nColour=",Colour), cex=2.0, pos=4, family="Arial")
    dev.off()
  }

}