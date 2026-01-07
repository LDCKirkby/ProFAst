PSV_maker <- function(loc, ID, colour, magnitude, RA_vals, Dec_vals){
    orig_ID = ID
    obs_times = read.delim("./obs_times_full.txt", header =FALSE, col.names = c("frame","obs1","obs2","obs3","obs4","obs5","obs6","obs7","obs8","obs9"),  sep = ",")

    exposure = switch(tolower(colour),
                    "g" = 900,
                    "r" = 1800,
                    "i" = 1200) #seconds

    #Date of Observation (J2000.0)
    obs = subset(obs_times, subset = grepl(paste0("_",loc,"_",colour), obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
    obs_start = as.POSIXct(obs$obs1, tz = "UTC")
    obs_mid = as.POSIXct(obs$obs3, tz = "UTC")
    obs_end = as.POSIXct(obs$obs5, tz = "UTC") + (exposure/5)

    iso_start = paste0(format_ISO8601(obs_start, usetz="Z"),"   ")
    iso_mid = paste0(format_ISO8601(obs_mid, usetz="Z"),"   ")
    iso_end = paste0(format_ISO8601(obs_end, usetz="Z"),"   ")

    #T: Temporary Designation Number (6 - 12)
    hex_string <- as.hexmode(as.numeric(ID))
    hex_string <- sprintf("%04s", hex_string)      
    hex_string <- substr(hex_string, 1, 4)
    temp_start = paste0("K", substr(format(obs_start, "%Y"), 3, 4),hex_string)  
    temp_mid = paste0("K", substr(format(obs_mid, "%Y"), 3, 4),hex_string)
    temp_end = paste0("K", substr(format(obs_end, "%Y"), 3, 4),hex_string)

    RA_start = formatC(RA_vals[[1]], width=11, digits=7, format="f")
    Dec_start = formatC(Dec_vals[[1]], width=11, digits=7, format="f", flag=" ")
    RA_mid = formatC(RA_vals[[as.integer(length(RA_vals)/2)]], width=11, digits=7, format="f")
    Dec_mid = formatC(Dec_vals[[as.integer(length(Dec_vals)/2)]], width=11, digits=7, format="f", flag=" ")
    RA_end = formatC(RA_vals[[length(RA_vals)]], width=11, digits=7, format="f")
    Dec_end = formatC(Dec_vals[[length(Dec_vals)]], width=11, digits=7, format="f", flag=" ")

    mag = formatC(as.numeric(magnitude), digits=2, format="f")          
    col = formatC(colour, format="s", width=4, flag="- ")

    col_vals = c("trkSub  ","mode","stn ","obsTime                ","ra         ","dec        ","astCat","mag  ","band","remarks")
    start = c(temp_start," CCD","X11 ",iso_start,RA_start,Dec_start,"   UNK",mag,col,paste0("surveyID:",ID))
    mid = c(temp_mid," CCD","X11 ",iso_mid,RA_mid,Dec_mid,"   UNK",mag,col,paste0("surveyID:",ID))
    end = c(temp_end," CCD","X11 ",iso_end,RA_end,Dec_end,"   UNK",mag,col,paste0("surveyID:",ID))
    psv_output= as.data.table(rbind(start,mid,end))
    colnames(psv_output) <- col_vals
    write.table(psv_output, paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",orig_ID,".psv"), sep = "|", row.names=FALSE, quote=FALSE)
}