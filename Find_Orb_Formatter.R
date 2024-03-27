library(magicaxis)
library(celestial)
library(lubridate)


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
  #ymd_start = paste0(year(obs_start), " ", month(obs_start), " ", day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
  #ymd_end = paste0(year(obs_end), " ", month(obs_start), " ", day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
  
  #Uncomment once the max & min RA_Dec finder works properly 
  RA_top = paste0(deg2hms(asteroids[i, "tr_RA"])[[1]], " ",deg2hms(asteroids[i, "tr_RA"])[[2]], " ", deg2hms(asteroids[i, "tr_RA"])[[3]])
  Dec_top = paste0(deg2dms(asteroids[i, "tr_Dec"])[[1]], " ",deg2dms(asteroids[i, "tr_Dec"])[[2]], " ",substr(deg2dms(asteroids[i, "tr_Dec"])[[3]][], 1,4))
  RA_bottom = paste0(deg2hms(asteroids[i, "bl_RA"])[[1]], " ",deg2hms(asteroids[i, "bl_RA"])[[2]], " ", deg2hms(asteroids[i, "bl_RA"])[[3]])
  Dec_bottom = paste0(deg2dms(asteroids[i, "bl_Dec"])[[1]], " ",deg2dms(asteroids[i, "bl_Dec"])[[2]], " ",substr(deg2dms(asteroids[i, "bl_Dec"])[[3]][], 1,4))
  
  
  # RA_top = paste0(deg2hms(asteroids$RAmax[[i]])[[1]], " ",deg2hms(asteroids$RAmax[[i]])[[2]], " ", deg2hms(asteroids$RAmax[[i]])[[3]])
  # Dec_top = paste0(deg2dms(asteroids$Decmax[[i]])[[1]], " ",deg2dms(asteroids$Decmax[[i]])[[2]], " ",substr(deg2dms(asteroids$Decmax[[i]])[[3]][], 1,4))
  # RA_bottom = paste0(deg2hms(asteroids$RAcen[[i]])[[1]], " ",deg2hms(asteroids$RAcen[[i]])[[2]], " ", deg2hms(asteroids$RAcen[[i]])[[3]])
  # Dec_bottom = paste0(deg2dms(asteroids$Deccen[[i]])[[1]], " ",deg2dms(asteroids$Deccen[[i]])[[2]], " ",substr(deg2dms(asteroids$Deccen[[i]])[[3]][], 1,4))
  # 
  if(nchar(asteroids$groupID[[i]]) <= 8){
    diff = 8-nchar(asteroids$groupID[[i]])
    spacer1 = "     "
    for(j in 1:diff){
      spacer1 = paste0(spacer1,"0")
    }
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
    day_start = paste0("0",day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
    day_end = paste0("0",day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
  }else{
    day_start = paste0(day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
    day_end = paste0(day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
  }
  
  if(nchar(day_start) < 7){
    day_start = paste0(day_start, "0")
  }
  
  if(nchar(day_end) < 7){
    day_end = paste0(day_end, "0")
  }
  ymd_start = paste0(ymd_start, day_start)
  ymd_end = paste0(ymd_end, day_end)

#   ymd_start = paste0(year(obs_start), " ", month(obs_start), " ", day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
#   ymd_end = paste0(year(obs_end), " ", month(obs_end), " ", day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)

 
  
  line  = paste0(spacer1,asteroids$groupID[[i]], "  C",ymd_start, " ", RA_top, " ", Dec_top,"                      X11")
  line2 = paste0(spacer1,asteroids$groupID[[i]], "  C", ymd_end , " ", RA_top, " ", Dec_top,"                      X11")
  cat(line)
  cat(line2)
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






