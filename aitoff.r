library(scales)
library(celestial)
library(mapproj)
library(devtools)
library(ProFound)
library(Cairo)
library(sm)
library(magicaxis)
library(data.table)
library(plotrix)
library(foreign)
library(extrafont)
library(MASS)
library(grDevices)
library(showtext)
library(ggplot2)
library(ggnewscale)
font_add("Arial", "/Library/Fonts/Arial.ttf")
showtext_auto()

#
omegam=0.3158221
omegal=1-omegam
ho=67.2
name=c('DDF','DEEP','WIDE')
agelimit=13.0
zlimitw=21.5
zlimitd=21.5
zlimitd=50.0
redlimit=13.0
redlowlimit=10.0
#
AESOP_tight_hex = cbind(x = c(0.0, 1.097046, 1.097046, 1.097046, 0.0, -1.097046, -1.097046, -1.097046, 0.0),
                        y = c(1.26676, 0.6333799, 0.0, -0.6333799, -1.26676, -0.6333799, 0.0, 0.6333799, 1.26676)
)
#
WD_ddf = data.table(Name = c('WD01', 'WD02', 'WD03', 'WD10'),
                    RAcen = c(9.500, 35.875, 53.125, 150.125),
                    Deccen = c(-43.95,-5.025,-28.1,2.2),
                    rot=c(0.0,30.0,30.0,30.0))
WD_deep = data.table(RA=c(339.0,351.0),Dec=c(-30.0,-35.0))
WD_wide_north=data.table(RA=c(157.25,225.0),Dec=c(3.95,-3.95))
WD_wide_south=data.table(RA=c(-30.0,51.6),Dec=c(-27,-35.6))
#
LSST_Info = data.table(Name = c('WD01', 'WD02', 'WD03', 'WD10'),
                       RAcen = c(9.45, 35.70833, 53.125, 150.1),
                       Deccen = c(-44, -4.75, -28.1,2.181944)
)
#
LSST_rad <- 3.5/2.0
declimit=20.0
#
cat("****************Loading data****************\n")
all_points = read.csv("./all_asteroids.csv", header = TRUE)
# colnames(known_asteroids) = c("RA_Dec_ID")
# asteroid_and_fields = data.frame()
# for(asteroid in known_asteroids$RA_Dec_ID){
#   asteroid = strsplit(asteroid, split="_")[[1]]
#   RA_Dec = ""
#   ID = ""
#   if(length(asteroid) == 4){
#     RA_Dec = paste0(asteroid[2],"_",asteroid[3])
#     ID = gsub('[griGRI]', '', strsplit(asteroid[4], "[.]")[[1]][1])
#   }else if(length(asteroid) == 3){
#     RA_Dec = paste0(asteroid[1],"_",asteroid[2])
#     ID = gsub('[griGRI]', '', strsplit(asteroid[3], "[.]")[[1]][1])
#   }
#   asteroid_and_fields = rbind(asteroid_and_fields, c(RA_Dec, ID))
# }
# colnames(asteroid_and_fields) = c("RA_Dec", "ID")


# done = as.data.frame(list.files(path = paste0(pwd()), pattern = "_"))
# colnames(done) = "RA_Dec"

# all_points = data.frame()
# for(RA_Dec in done$RA_Dec){
#   #cat(RA_Dec,"\n")
#   if(paste0(RA_Dec,"_Verified.csv") %in% list.files(path = paste0("./",RA_Dec,"/")) == FALSE){
#     #cat("FALSE\n\n")
#     next
#   }
#   data = as.data.frame(read.csv(paste0("./",RA_Dec,"/",RA_Dec,"_Verified.csv")))
#   data = cbind(data, "Field" = RA_Dec)
#   all_points = try(rbind(all_points, data))
# }


#all_points=fread("/Users/sdriver/Drpbx/waves/mocks/waves-north_grps.csv")
#widesgrps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-south_grps.csv")

# widedgrps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-deep_grps.csv")
# wideddf1grps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-ddf1_grps.csv")
# wideddf2grps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-ddf2_grps.csv")
# wideddf3grps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-ddf3_grps.csv")
# wideddf4grps=fread("/Users/sdriver/Drpbx/waves/mocks/waves-ddf4_grps.csv")
# gaiadr3=fread("/Users/sdriver/Drpbx/active/flythru/cats/gaiadr3x.csv")
#
# all_points = cbind(all_points, "gFluxRatio" = all_points$flux_gt/(all_points$flux_rxt + all_points$flux_i1xt))
# all_points = cbind(all_points, "rFluxRatio" = all_points$flux_rxt/(all_points$flux_gt + all_points$flux_i1xt))
# all_points = cbind(all_points, "iFluxRatio" = all_points$flux_i1xt/(all_points$flux_gt + all_points$flux_rxt))
# all_points = cbind(all_points, "norm_g" = all_points$flux_gt/(all_points$flux_gt + all_points$flux_rxt + all_points$flux_i1xt))
# all_points = cbind(all_points, "norm_r" = all_points$flux_rxt/(all_points$flux_gt + all_points$flux_rxt + all_points$flux_i1xt))
# all_points = cbind(all_points, "norm_i" = all_points$flux_i1xt/(all_points$flux_gt + all_points$flux_rxt + all_points$flux_i1xt))
# all_points = cbind(all_points, "Entropy" = as.numeric(-( (all_points$norm_g * log(all_points$norm_g)) +
#                                                            (all_points$norm_i * log(all_points$norm_i))
#                                                          + (all_points$norm_r * log(all_points$norm_r)) )) )

# asteroids = subset(all_points, segID %in% asteroid_and_fields$ID & Field %in% asteroid_and_fields$RA_Dec)
# asteroids = all_points[all_points$segID %in% asteroid_and_fields$ID & all_points$Field %in% asteroid_and_fields$RA_Dec, ]

# write.csv(asteroids, "./Verified_Asteroid_Data.csv")
# write.csv(all_points, "./All_Flagged_Objects.csv")

# ##########################################################
# ####################### All Objects ######################
# ##########################################################
# cat("****************Making all object images****************\n")
# png(filename=paste0("./cutmap_g.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = all_points, mapping = aes(x = gFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), labels = c("0.00001","0.0001","0.001", "0.01", "0.1", "1", "10","100")) +
#                                     xlab("Flux Ratio (g band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick")) + #scale_color_viridis_c(option = "A") +
#                                     ggnewscale::new_scale_color() + 
#                                     geom_point(data = asteroids, mapping = aes(x = gFluxRatio, y = axrat, size = N100, color = Colour)) + scale_colour_manual(values = c("green", "blue", "red"))# + scale_colour_gradient(name = "Asteroids", low = "darkgreen", high = "green")
# dev.off()


# png(filename=paste0("./cutmap_r.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = all_points, mapping = aes(x = rFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), labels = c("0.00001","0.0001","0.001", "0.01", "0.1", "1", "10","100")) +
#                                     xlab("Flux Ratio (r band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick")) + #scale_color_viridis_c(option = "A") +
#                                     ggnewscale::new_scale_color() + 
#                                     geom_point(data = asteroids, mapping = aes(x = rFluxRatio, y = axrat, size = N100, color = Colour)) + scale_colour_manual(values = c("green", "blue", "red"))# + scale_colour_gradient(name = "Asteroids", low = "firebrick", high = "red")
# dev.off()


# png(filename=paste0("./cutmap_i.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = all_points, mapping = aes(x = iFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) + 
#                                     scale_x_log10(breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), labels = c("0.00001","0.0001","0.001", "0.01", "0.1", "1", "10","100")) +
#                                     xlab("Flux Ratio (i band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick")) + #scale_color_viridis_c(option = "A") +
#                                     ggnewscale::new_scale_color() + 
#                                     geom_point(data = asteroids, mapping = aes(x = iFluxRatio, y = axrat, size = N100, color = Colour)) + scale_colour_manual(values = c("green", "blue", "red"))# + scale_colour_gradient(name = "Asteroids", low = "darkblue", high = "blue")
# dev.off()

# png(filename=paste0("./entropy_map.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = all_points, mapping = aes(x = Entropy, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(labels = comma) +
#                                     xlab("Entropy") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick")) +# scale_color_viridis_c(option = "A") +
#                                     ggnewscale::new_scale_color() + 
#                                     geom_point(data = asteroids, mapping = aes(x = Entropy, y = axrat, size = N100, color = Colour)) + scale_colour_manual(values = c("green", "blue", "red"))# + scale_colour_gradient(name = "Asteroids", low = "deeppink4", high = "deeppink")

# dev.off()

# ##########################################################
# ############### Known Asteroids Only######################
# ##########################################################
# cat("****************Making asteroid only images****************\n")
# png(filename=paste0("./asteroid_cutmap_g.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = asteroids, mapping = aes(x = gFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10), n.breaks = 6, labels = c("0.0001","0.001", "0.01", "0.1", "1", "10")) +
#                                     xlab("Flux Ratio (g band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick"))
# dev.off()


# png(filename=paste0("./asteroid_cutmap_r.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = asteroids, mapping = aes(x = rFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10), n.breaks = 6, labels = c("0.0001","0.001", "0.01", "0.1", "1", "10")) +
#                                     xlab("Flux Ratio (r band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick"))
# dev.off()


# png(filename=paste0("./asteroid_cutmap_i.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = asteroids, mapping = aes(x = iFluxRatio, y = axrat, size = N100, color = Colour, alpha = 0.5)) +
#                                     scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10), n.breaks = 6, labels = c("0.0001","0.001", "0.01", "0.1", "1", "10")) +
#                                     xlab("Flux Ratio (i band)") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick"))
# dev.off()


# png(filename=paste0("./asteroid_entropy_map.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
# par(mfrow=c(1,1),mar=c(3,3,2,2), family="Arial")
# ggplot(data = asteroids, mapping = aes(x = Entropy, y = axrat, size = N100, color = Colour, alpha = 0.5)) + 
#                                     scale_x_log10(labels = comma) +
#                                     xlab("Entropy") + ylab("Axial Ratio") + 
#                                     scale_y_log10(labels = comma) +
#                                     geom_point() + scale_colour_manual(values = c("darkgreen", "darkblue", "firebrick"))
# dev.off()

##############################

png(filename=paste0("./projectionmap.png"),width=30.0,height=20.0,units="cm",res=240, family = "")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
magproj(-100,-100,type="p",pch=".",centre=c(60,0),latlim=c(-90,declimit),labloc=c(300,15),fliplong=FALSE, family = "")
magecliptic(width=10,col=rgb(0.5,0.5,0.5,0.1),border="NA")
magecliptic(width=1,col=rgb(0.5,0.5,0.5,0.25),border="NA")
for (i in 1:30){
  magMWplane(width=i,col=rgb(1,0.84,0,(51-i)/2000),border="NA")
  magMW(pch=16,cex=i/2,col=rgb(1,0.84,0,(51-i)/1000),border="NA")
}
magproj(-100,-100,type="p",pch=".",centre=c(60,0),latlim=c(-90,10),add=T)
magproj(WD_wide_north,add=T,col='NA')
magproj(WD_wide_south,add=T,col='NA')

#magproj(WD_deep,plot,add=T,col='NA')
#for (ii in 1:4){
#  AESOP_tight_hex_rot = cbind(x = AESOP_tight_hex[,1] * cos(WD_ddf[ii,rot]*pi/180) - AESOP_tight_hex[,2] * sin(WD_ddf[ii,rot]*pi/180),
#                              y = AESOP_tight_hex[,1] * sin(WD_ddf[ii,rot]*pi/180) + AESOP_tight_hex[,2] * cos(WD_ddf[ii,rot]*pi/180)
#  )
#  #
#  magproj((AESOP_tight_hex_rot[,1]/cos(WD_ddf[ii,Deccen] * pi/180)) + WD_ddf[ii,RAcen],
#          AESOP_tight_hex_rot[,2] + WD_ddf[ii,Deccen],
#          type='pl', col='NA', border='black', add=TRUE)
#}
#
cat("magproj time.\n")
cat(length(all_points$RAcen), " ", length(all_points$Deccen),"\n")
magproj(all_points$RAcen, all_points$Deccen,lwd=5, pch=19,col=rgb(1,0,0,0.025),cex=0.1,add=T,type="p")
#magproj(all_points[all_points$Colour == "r", "RAcen"], all_points[all_points$Colour == "r", "Deccen"], pch=1, col="red",cex=0.1,add=T,type="p")
#magproj(all_points[all_points$Colour == "i", "RAcen"], all_points[all_points$Colour == "i", "Deccen"], pch=1, col="blue",cex=0.1,add=T,type="p")

#magproj(widesgrps$ra[widesgrps$Nfof > 10],widesgrps$dec[widesgrps$Nfof > 10],pch=16,col=rgb(0,0,0.0,0.25),cex=0.25,add=T,type="p")
#magproj(widedgrps$ra[widedgrps$Nfof > 10],widedgrps$dec[widedgrps$Nfof > 10],pch=16,col=rgb(0.39,0.58,0.93,0.5),cex=0.25,add=T,type="p")
#magproj(wideddf1grps$ra[wideddf1grps$Nfof > 10],wideddf1grps$dec[wideddf1grps$Nfof > 10],pch=16,col=rgb(0.0,0.0,0.0,0.25),cex=0.25,add=T,type="p")
#magproj(wideddf2grps$ra[wideddf2grps$Nfof > 10],wideddf2grps$dec[wideddf2grps$Nfof > 10],pch=16,col=rgb(0.0,0.0,0.0,0.25),cex=0.25,add=T,type="p")
#magproj(wideddf3grps$ra[wideddf3grps$Nfof > 10],wideddf3grps$dec[wideddf3grps$Nfof > 10],pch=16,col=rgb(0.0,0.0,0.0,0.25),cex=0.25,add=T,type="p")
#magproj(wideddf4grps$ra[wideddf4grps$Nfof > 10],wideddf4grps$dec[wideddf4grps$Nfof > 10],pch=16,col=rgb(0.0,0.0,0.0,0.25),cex=0.25,add=T,type="p")
#
magproj(12.5,-23,type="t",plottext="WAVES-South",col="maroon",add=T, family = "")
magproj(190,7,type="t",plottext="WAVES-North",col="maroon",add=T, family = "")
#magproj(-20,-40,type="t",plottext="WAVES-Deep",col="maroon",add=T)
#magproj(-10.0,-49,type="t",plottext="WAVES-DDF1 (WD01)",col="maroon",add=T,pos=4)
#magproj(10.0,0,type="t",plottext="WAVES-DDF2 (WD02)",col="maroon",add=T,pos=4)
#magproj(48,-25,type="t",plottext="WAVES-DDF3 (WD03)",col="maroon",add=T,pos=4)
#magproj(100,6,type="t",plottext="WAVES-DDF4 (WD04)",col="maroon",add=T,pos=4)
#
r=LSST_rad
x=seq(-r,r,by=0.01)
y=(r^2-x^2)^0.5
x=c(x,rev(x))
y=c(-y,y)
#magproj(x/cosd(LSST_Info[1,Deccen])+LSST_Info[1,RAcen],y+LSST_Info[1,Deccen],type="pl",border="orange",col=rgb(1.0,0.65,0,0.25),add=T)
#magproj(x/cosd(LSST_Info[2,Deccen])+LSST_Info[2,RAcen],y+LSST_Info[2,Deccen],type="pl",border="orange",col=rgb(1.0,0.65,0,0.25),add=T)
#magproj(x/cosd(LSST_Info[3,Deccen])+LSST_Info[3,RAcen],y+LSST_Info[3,Deccen],type="pl",border="orange",col=rgb(1.0,0.65,0,0.25),add=T)
#magproj(x/cosd(LSST_Info[4,Deccen])+LSST_Info[4,RAcen],y+LSST_Info[4,Deccen],type="pl",border="orange",col=rgb(1.0,0.65,0,0.25),add=T)
# #
# ra1=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 12 & gaiadr3$dec<declimit,"ra"]))
# dec1=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 12 & gaiadr3$dec<declimit,"dec"]))
# b1=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 12 & gaiadr3$dec<declimit,"b"]))
# magproj(ra1,dec1,type="p",pch=".",cex=0.1,col=rgb(0.5,0.5,0.5,0.01*cosd(b1)),add=T)
# ra2=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 8 & gaiadr3$dec<declimit,"ra"]))
# dec2=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 8 & gaiadr3$dec<declimit,"dec"]))
# b2=as.numeric(unlist(gaiadr3[gaiadr3$phot_g_mean_mag < 8 & gaiadr3$dec<declimit,"b"]))
# magproj(ra2,dec2,type="p",pch=".",cex=0.1,col=rgb(0.0,0.5,0.5,0.1*cosd(b2)),add=T)
#
dev.off()

warnings()