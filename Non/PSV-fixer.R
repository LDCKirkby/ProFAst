library(Rwcs)
library(stringr)
library(data.table)

args = commandArgs(trailingOnly = TRUE)
loc = as.character(args[[1]])
ID = as.character(args[[2]])
ID = str_split(ID, '\\.')[[1]][1]

PSV = read.table(file = paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".psv"), header=TRUE, sep="|")
MPC_80 = read.table(file = paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".mpc"), header=FALSE, sep="", fill=TRUE)

if(str_detect(MPC_80[1,5], "\\+")==TRUE){
MPC_hrs = c(str_split(MPC_80[1,5], "\\+")[[1]][2], str_split(MPC_80[2,6], "\\+")[[1]][2], str_split(MPC_80[3,6], "\\+")[[1]][2])
}else{
MPC_hrs = c(str_split(MPC_80[1,5], "-")[[1]][2], str_split(MPC_80[2,6], "-")[[1]][2], str_split(MPC_80[3,6], "-")[[1]][2])
}
MPC_min = c(MPC_80[1,6],MPC_80[2,7],MPC_80[3,7])
MPC_sec = c(as.numeric(MPC_80[1,7]),as.numeric(MPC_80[2,8]),as.numeric(MPC_80[3,8]))
MPC_HMS = data.frame()
MPC_HMS = rbind(MPC_HMS, MPC_hrs, MPC_min, MPC_sec)
colnames(MPC_HMS) <- c("obs1", "obs2", "obs3")

for(i in 1:3){
    DEC = as.numeric(MPC_HMS[1,i]) + as.numeric(MPC_HMS[2,i])/60 + as.numeric(MPC_HMS[3,i])/3600
    DEC = formatC(DEC, width=11, digits=6, format="f", flag="- ")
    PSV[i,"dec"] = DEC
}

col_vals = c("trkSub ","mode","stn ","obsTime                ","ra         ","dec        ","mag  ","band","remarks")
iso_start = PSV$obsTime[1]
iso_mid = PSV$obsTime[2]
iso_end = PSV$obsTime[3]

temp_start = PSV$trkSub[1]
temp_mid = PSV$trkSub[2]
temp_end = PSV$trkSub[3]

RA_start = formatC(PSV$ra[1], width=11, format="f", flag="-")
RA_mid = formatC(PSV$ra[2], width=11, format="f", flag="-")
RA_end = formatC(PSV$ra[3], width=11, format="f", flag="-")

mag = formatC(as.numeric(PSV$mag[1]), digits=2, format="f")          
col = formatC(PSV$band[1], format="s", width=4, flag="- ")

start = c(temp_start," CCD","X11 ",iso_start,RA_start,PSV$dec[1],mag,col,PSV$remarks[1])
mid = c(temp_mid," CCD","X11 ",iso_mid,RA_mid,PSV$dec[2],mag,col,PSV$remarks[2])
end = c(temp_end," CCD","X11 ",iso_end,RA_end,PSV$dec[3],mag,col,PSV$remarks[3])
psv_output= as.data.table(rbind(start,mid,end))
colnames(psv_output) <- col_vals
write.table(psv_output, paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",ID,".psv"), sep = "|", row.names=FALSE, quote=FALSE)
