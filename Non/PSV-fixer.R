library(Rwcs)
library(Rfits)
library(stringr)
library(data.table)

#Parse input arguments
args = commandArgs(trailingOnly = TRUE)
loc = as.character(args[[1]])
ID = as.character(args[[2]])
#Remove file extensions from ID
ID = str_split(ID, '\\.')[[1]][1]
#Extract declination to ensure sign is preserved
declination = as.numeric(str_split(loc, "_")[[1]][2])

#Load data in from PSV, MPC 80 Byte & detection csv
PSV = read.table(file = paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".psv"), header=TRUE, sep="|")
MPC_80 = read.table(file = paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".mpc"), header=FALSE, sep="", fill=TRUE)
asteroids = read.csv(paste0("./",loc,"/",loc,"_no_dupes.csv"))
#Load g_image for accurate cutim_g adjustments
g_image = Rfits_point(paste0("/Volumes/WAVESSPD/waves/wavesdata/VST/dr5/preprocessed/KIDS_",loc,"_g_DMAG.fits"),header=TRUE,ext=1)

#Create cutim for accurate RWCS
target = asteroids[asteroids$segID == ID,]
astradec = target[c("RAcen", "Deccen")]
astpos=as.integer(Rwcs_s2p(RA=astradec$RAcen, Dec=astradec$Deccen, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS"))
wid <- 100.0
box<-c(2*wid,2*wid)
mulim<-22.0
kids<-(0.339^2)*(10^(0.4*(0-mulim)))
viking<-(0.339^2)*(10^(0.4*(30-mulim)))
cutim_g=g_image[astpos,box=box]

#Adjust declination in PSV based on 80 byte value to increase accuracy
if(str_detect(MPC_80[1,5], "\\+")==TRUE){
MPC_hrs = c(str_split(MPC_80[1,5], "\\+")[[1]][2], str_split(MPC_80[2,6], "\\+")[[1]][2], str_split(MPC_80[3,6], "\\+")[[1]][2])
neg =1
}else if(declination <= 0){
MPC_hrs = c(str_split(MPC_80[1,5], "-")[[1]][2],str_split(MPC_80[2,6], "-")[[1]][2],str_split(MPC_80[3,6], "-")[[1]][2])
neg = -1
}else{
MPC_hrs = c(str_split(MPC_80[1,5], "-")[[1]][2],str_split(MPC_80[2,6], "-")[[1]][2],str_split(MPC_80[3,6], "-")[[1]][2])
neg = -1
}

MPC_min = c(MPC_80[1,6],MPC_80[2,7],MPC_80[3,7])
MPC_sec = c(as.numeric(MPC_80[1,7]),as.numeric(MPC_80[2,8]),as.numeric(MPC_80[3,8]))
MPC_HMS = data.frame()
MPC_HMS = rbind(MPC_HMS, MPC_hrs, MPC_min, MPC_sec)

wrong_dec_start = neg*(as.numeric(MPC_HMS[1,1]) + as.numeric(MPC_HMS[2,1])/60 + as.numeric(MPC_HMS[3,1])/3600)
wrong_dec_mid = neg*(as.numeric(MPC_HMS[1,2]) + as.numeric(MPC_HMS[2,2])/60 + as.numeric(MPC_HMS[3,2])/3600)
wrong_dec_end = neg*(as.numeric(MPC_HMS[1,3]) + as.numeric(MPC_HMS[2,3])/60 + as.numeric(MPC_HMS[3,3])/3600)

#Convert wrong RA_Dec to pixels
wrong_start = Rwcs_s2p(PSV$ra[1], wrong_dec_start, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")
wrong_mid = Rwcs_s2p(PSV$ra[2], wrong_dec_mid, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")
wrong_end = Rwcs_s2p(PSV$ra[3], wrong_dec_end, keyvalues=g_image$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")

#Convert pixels to right RA_Dec
right_start = Rwcs_p2s(wrong_start, keyvalues=cutim_g$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")
right_mid = Rwcs_p2s(wrong_mid, keyvalues=cutim_g$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")
right_end = Rwcs_p2s(wrong_end, keyvalues=cutim_g$keyvalues, EQUINOX = 2000L, RADESYS = "ICRS")

#Format RA & Dec to correct PSV format
RA_start = formatC(right_start[1], width=11, digits=7, format="f")
RA_mid = formatC(right_mid[1], width=11, digits=7, format="f")
RA_end = formatC(right_end[1], width=11, digits=7, format="f")
Dec_start = formatC(right_start[2], width=11, digits=7, format="f", flag=" ")
Dec_mid = formatC(right_mid[2], width=11, digits=7, format="f", flag=" ")
Dec_end = formatC(right_end[2], width=11, digits=7, format="f", flag=" ")

col_vals = c("trkSub ","mode","stn ","obsTime                ","ra         ","dec        ","mag  ","band","remarks")
iso_start = PSV$obsTime[1]
iso_mid = PSV$obsTime[2]
iso_end = PSV$obsTime[3]

temp_start = PSV$trkSub[1]
temp_mid = PSV$trkSub[2]
temp_end = PSV$trkSub[3]

mag = formatC(as.numeric(PSV$mag[1]), digits=2, format="f")          
col = formatC(PSV$band[1], format="s", width=4, flag="- ")

start = c(temp_start," CCD","X11 ",iso_start,RA_start,Dec_start,mag,col,PSV$remarks[1])
mid = c(temp_mid," CCD","X11 ",iso_mid,RA_mid,Dec_mid,mag,col,PSV$remarks[2])
end = c(temp_end," CCD","X11 ",iso_end,RA_end,Dec_end,mag,col,PSV$remarks[3])
psv_output= as.data.table(rbind(start,mid,end))
colnames(psv_output) <- col_vals
write.table(psv_output, paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".psv"), sep = "|", row.names=FALSE, quote=FALSE)
