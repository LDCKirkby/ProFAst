library(magicaxis)
library(celestial)
library(lubridate)
library(common)


#formatter <- function(loc){
args = commandArgs(trailingOnly=TRUE)
loc = args[[1]]

obs_times = read.delim("./obs_times_full.txt", header =FALSE, col.names = c("frame","obs1","obs2","obs3","obs4","obs5","obs6","obs7","obs8","obs9"),  sep = ",")
asteroids = read.csv(paste0("./",loc,"/",loc,"_Asteroids.csv"))

exposure = 0 
# half_year = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y")

find_orb = c()
astcheck = c()

for( i in 1:length(asteroids$groupID)){
  colour = asteroids$Colour[i]
  ID = asteroids$groupID[i]
  
  if("g" %in% colour == TRUE){
    exposure = 900 #seconds
  }
  if("i" %in% colour == TRUE){
    exposure = 1080 #seconds
  }
  if("r" %in% colour == TRUE){
    exposure = 1800 #seconds
  }
  
  obs = subset(obs_times, subset = grepl(paste0(loc,"_",asteroids$Colour[i]), obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
  obs_start = as.POSIXct(obs$obs1, tz = "UTC")
  obs_end = as.POSIXct(obs$obs5, tz = "UTC") + (exposure/5)

  RA_top = paste0(deg2hms(asteroids[i, "tr_RA"])[[1]], " ",deg2hms(asteroids[i, "tr_RA"])[[2]], " ",deg2hms(asteroids[i, "tr_RA"], digits = 3)[[3]])
  Dec_top = paste0(deg2dms(asteroids[i, "tr_Dec"])[[1]], " ",deg2dms(asteroids[i, "tr_Dec"])[[2]], " ",deg2dms(asteroids[i, "tr_Dec"], digits = 2)[[3]])
  RA_bottom = paste0(deg2hms(asteroids[i, "bl_RA"])[[1]], " ",deg2hms(asteroids[i, "bl_RA"])[[2]], " ",deg2hms(asteroids[i, "bl_RA"], digits = 3)[[3]])
  Dec_bottom = paste0(deg2dms(asteroids[i, "bl_Dec"])[[1]], " ",deg2dms(asteroids[i, "bl_Dec"])[[2]], " ",deg2dms(asteroids[i, "bl_Dec"], digits = 2)[[3]])
  
  
  spacer1 = "00001"
  spacer2 = "00002"
  
  if(nchar(ID) > 7){
    long = substr(ID, 1, (nchar(ID) - 7))
    capital = FALSE
    if(strtoi(long) > 26){
      capital = TRUE
      long = strtoi(long) - 26
      if(long > 26){
        next
      }
      long = paste0(chartr("0-9JA-I","JA-I0-9",long))
    }else{
      long = chartr("0-9ja-i","ja-i0-9",long)
    }
    ID = paste0(long, substr(ID, nchar(ID) - 7, nchar(ID)))
    print(long_alpha)
  }else if(nchar(ID) < 6){
    add = ""
    for(j in 1: (6 - nchar(ID))){
      add = paste0(add,"0")
    }
    ID = paste0(add,ID)
  }
  
    
  ymd_start = paste0(year(obs_start)," ")
  ymd_end = paste0(year(obs_end), " ")
  day_start = ""
  day_end = ""
  
  if(month(obs_start) < 10){
    ymd_start = paste0(ymd_start, paste0("0",month(obs_start)), " ")
    ymd_end = paste0(ymd_end, paste0("0",month(obs_end)), " ")
  }else{
    ymd_start = paste0(ymd_start, month(obs_start), " ")
    ymd_end = paste0(ymd_end, month(obs_end), " ")
  }
  
  if(day(obs_start) < 10){
    day_start = paste0("0",day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^6)/10^6)
    day_end = paste0("0",day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^6)/10^6)
  }else{
    day_start = paste0(day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^6)/10^6)
    day_end = paste0(day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^6)/10^6)
  }
  
  if(nchar(day_start) < 9){
    day_start = paste0(day_start, "0")
  }
  
  if(nchar(day_end) < 9){
    day_end = paste0(day_end, "0")
  }
  ymd_start = paste0(ymd_start, day_start)
  ymd_end = paste0(ymd_end, day_end)

  mag = trunc(asteroids$mag[i]*10^2)/10^2
  if(unlist(gregexpr('[.]', mag)) == -1){
    mag = paste0(mag,".00")
  }else if((nchar(toString(mag)) - unlist(gregexpr('[.]', mag))) < 3){
    for(l in 1:(nchar(toString(mag)) - unlist(gregexpr('[.]', mag)))){
      mag = paste0(mag,"0")
    }
  }
  colour = asteroids$Colour[i]
  
  line  = paste0(spacer1," ",ID," ","P",ymd_start, "  ", RA_top, " ", Dec_top,spaces(9),spaces(8-nchar(mag)),mag,colour," X11")
  line2 = paste0(spacer2," ",ID," ","P", ymd_end , "  ", RA_top, " ", Dec_top,spaces(9),spaces(8-nchar(mag)),mag,colour," X11")
  cat(line, "\n")
  cat(line2, "\n")
  find_orb <- append(find_orb, line)
  find_orb <- append(find_orb, line2)
}

# 
# 
# 
# for( i in 1:length(asteroids$groupID)){
#   colour = asteroids$Colour[i]
#   
#   if("g" %in% colour == TRUE){
#     exposure = 900 #seconds
#   }
#   if("i" %in% colour == TRUE){
#     exposure = 1080 #seconds
#   }
#   if("r" %in% colour == TRUE){
#     exposure = 1800 #seconds
#   }
#   
#   
#   obs = subset(obs_times, subset = grepl(paste0(loc,"_",asteroids$Colour[i]), obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
#   obs_start = as.POSIXct(obs$obs1)
#   obs_end = as.POSIXct(obs$obs5) + (exposure/5)
#   obs_start = paste0(year(obs_start), " ", month(obs_start), " ", day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
#   obs_end = paste0(year(obs_end), " ", month(obs_end), " ", day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
#   
#   if()
#   
#   
#   RA_cen = paste0(deg2hms(asteroids[i, "RAcen"])[[1]], " ",deg2hms(asteroids[i, "RAcen"])[[2]], " ", deg2hms(asteroids[i, "RAcen"])[[3]])
#   Dec_cen = paste0(deg2dms(asteroids[i, "Deccen"])[[1]], " ",deg2dms(asteroids[i, "Deccen"])[[2]], " ",substr(deg2dms(asteroids[i, "Deccen"])[[3]][], 1,4))
#   
#   
#   
#   line = paste0("     ","K24",, "  ", "C",obs_start, " ", RA_cen, " ", Dec_cen,"                      X11")
#   astcheck <- append(astcheck, line)
# 
# }  
# 

write.table(find_orb, paste0("./",loc,"/",loc,"_findorb.txt"), sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(astcheck, paste0("./",loc,"/",loc,"_astcheck.txt"), sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)






