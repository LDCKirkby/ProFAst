library(magicaxis)
library(celestial)
library(lubridate)



formatter <- function(loc){

obs_times = read.delim("./obs_times_full.txt", header =FALSE, col.names = c("frame","obs1","obs2","obs3","obs4","obs5","obs6","obs7","obs8","obs9"),  sep = ",")
asteroids = read.csv(paste0("./",loc,"/N100_Filtered_Asteroids.csv"))

exposure = 0 

output = c()

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
  
  
  obs = subset(obs_times, subset = grepl(loc, obs_times$frame) == TRUE & grepl(asteroids$Colour[i], obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
  obs_start = as.POSIXct(obs$obs1)
  obs_end = as.POSIXct(obs$obs5) + (4*60)
  time_start = ymd_hms(obs_start)
  ymd_start = format(obs_start, format = "%Y %M %d")
  ymd_end = format(obs_end, format = "%Y %M %d")
  time_start = as.numeric(format(obs_start, format = "%H"))/24 +as.numeric(format(obs_start, format = "%M"))/(24*60) + as.numeric(format(obs_start, format = "%S"))/(24*60*60)
  time_end = as.numeric(format(obs_end, format = "%H"))/24 +as.numeric(format(obs_end, format = "%M"))/(24*60) + as.numeric(format(obs_end, format = "%S"))/(24*60*60)
  time_start = as.character(trunc(time_start*10^4)/10^4)
  time_end = as.character(trunc(time_end*10^4)/10^4)
  
  #Uncomment once the max & min RA_Dec finder works properly 
  # RA_top = paste0(deg2hms(asteroids$RA_top[[i]])[[1]], " ",deg2hms(asteroids$RA_top[[i]])[[2]], " ", deg2hms(asteroids$RA_top[[i]])[[3]])
  # Dec_top = paste0(deg2dms(asteroids$Dec_top[[i]])[[1]], " ",deg2dms(asteroids$Dec_top[[i]])[[2]], " ",substr(deg2dms(asteroids$Dec_top[[i]])[[3]][], 1,4))
  # RA_bottom = paste0(deg2hms(asteroids$RA_bottom[[i]])[[1]], " ",deg2hms(asteroids$RA_bottom[[i]])[[2]], " ", deg2hms(asteroids$RA_bottom[[i]])[[3]])
  # Dec_bottom = paste0(deg2dms(asteroids$Dec_bottom[[i]])[[1]], " ",deg2dms(asteroids$Dec_bottom[[i]])[[2]], " ",substr(deg2dms(asteroids$Dec_bottom[[i]])[[3]][], 1,4))
  # 
  
  RA_top = paste0(deg2hms(asteroids$RAmax[[i]])[[1]], " ",deg2hms(asteroids$RAmax[[i]])[[2]], " ", deg2hms(asteroids$RAmax[[i]])[[3]])
  Dec_top = paste0(deg2dms(asteroids$Decmax[[i]])[[1]], " ",deg2dms(asteroids$Decmax[[i]])[[2]], " ",substr(deg2dms(asteroids$Decmax[[i]])[[3]][], 1,4))
  RA_bottom = paste0(deg2hms(asteroids$RAcen[[i]])[[1]], " ",deg2hms(asteroids$RAcen[[i]])[[2]], " ", deg2hms(asteroids$RAcen[[i]])[[3]])
  Dec_bottom = paste0(deg2dms(asteroids$Deccen[[i]])[[1]], " ",deg2dms(asteroids$Deccen[[i]])[[2]], " ",substr(deg2dms(asteroids$Deccen[[i]])[[3]][], 1,4))
  
  if(nchar(asteroids$groupID[[i]]) <= 8){
    diff = 8-nchar(asteroids$groupID[[i]])
    spacer1 = ""
    for(j in 1:diff){
      spacer1 = paste0(spacer1," ")
    }
  }
  line = paste0("     ",asteroids$groupID[[i]],spacer1, "  ", "C",ymd_start, substr(time_start, 2, 6), " ", RA_top, " ", Dec_top,"                      X11")
  line2 = paste0("     ",asteroids$groupID[[i]],spacer1, "  ", "C",ymd_end, substr(time_end, 2, 6), " ", RA_bottom, " ", Dec_bottom,"                      X11")
  output <- append(output, line)
  output <- append(output, line2)
  print(nchar(line))
}

write.table(output, paste0("./",loc,"/",loc,"_MPC_Format.txt", sep = "X11", row.names = FALSE, col.names = FALSE))
}