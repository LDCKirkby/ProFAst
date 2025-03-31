library(magicaxis)
library(celestial)
library(lubridate)
library(common)

#Creates outputs according to MPC 80 byte format
#NNNNNTTTTTTT*KCYYYY M1 D1.d1d1d1HH M2 SS.d2dsD2 M3 SS.d3         OOOOOO      CCC
#     K24Q00T  C2024 08 27.42670522 42 15.588-01 16 01.10               VEQ054F52

formatter <- function(loc, ID, colour, magnitude, RA_vals, Dec_vals){
  cat("Reading observation times\n")
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

  hex_string <- as.hexmode(as.numeric(ID))
  hex_string <- sprintf("%04s", hex_string)
  hex_string <- substr(hex_string, 1, 4)
  temp_start = paste0(" K", substr(format(obs_start, "%Y"), 3, 4),hex_string)
  temp_mid = paste0(" K", substr(format(obs_mid, "%Y"), 3, 4),hex_string)
  temp_end = paste0(" K", substr(format(obs_end, "%Y"), 3, 4),hex_string)

  #ID = formatC(ID, width = 7, flag = "0", format = "d")

  ym_start = format(obs_start, "%Y %m")
  ym_mid = format(obs_mid,  "%Y %m")
  ym_end = format(obs_end, "%Y %m")

  day_start = as.integer(day(obs_start)) + as.integer(hour(obs_start))/24 + as.integer(minute(obs_start))/(24*60) + as.integer(second(obs_start))/(24*60*60)
  day_mid = as.integer(day(obs_mid)) + as.integer(hour(obs_mid))/24 + as.integer(minute(obs_mid))/(24*60) + as.integer(second(obs_mid))/(24*60*60)
  day_end = as.integer(day(obs_end)) + as.integer(hour(obs_end))/24 + as.integer(minute(obs_end))/(24*60) + as.integer(second(obs_end))/(24*60*60)

  day_start = formatC(day_start, format = "f", width = 9, digits = 6, flag = "0")
  day_mid = formatC(day_mid, format = "f", width = 9, digits = 6, flag = "0")
  day_end = formatC(day_end, format = "f", width = 9, digits = 6, flag = "0")

  ymd_start = paste0(ym_start," ", day_start)
  ymd_mid = paste0(ym_mid," ", day_mid)
  ymd_end = paste0(ym_end," ", day_end)

  #Observed Magnitude and Band (66 - 71)
  mag = formatC(magnitude, digits = 2, width = 5, format = "f", flag = "0")  
  
  RA_first = paste0(deg2hms(RA_vals[[1]])[[1]], " ",deg2hms(RA_vals[[1]])[[2]], " ",deg2hms(RA_vals[[1]], digits = 3)[[3]])
  Dec_first = paste0(deg2dms(Dec_vals[[1]])[[1]], " ",deg2dms(Dec_vals[[1]])[[2]], " ",deg2dms(Dec_vals[[1]], digits = 2)[[3]])
  
  RA_end = paste0(deg2hms(RA_vals[[length(RA_vals)]])[[1]], " ",deg2hms(RA_vals[[length(RA_vals)]])[[2]], " ",deg2hms(RA_vals[[length(RA_vals)]], digits = 3)[[3]])
  Dec_end = paste0(deg2dms(Dec_vals[[length(Dec_vals)]])[[1]], " ",deg2dms(Dec_vals[[length(Dec_vals)]])[[2]], " ",deg2dms(Dec_vals[[length(Dec_vals)]], digits = 2)[[3]])
  
  RA_mid = paste0(deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]])[[1]], " ",deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]])[[2]], " ",deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]], digits = 3)[[3]])
  Dec_mid = paste0(deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]])[[1]], " ",deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]])[[2]], " ",deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]], digits = 2)[[3]])

  line  = paste0("     ",temp_start,"*KC",ymd_start, RA_first, Dec_first,"         ",mag,colour,"      X11")
  line2 = paste0("     ",temp_mid," KC",ymd_mid  , RA_mid  , Dec_mid  ,"         ",mag,colour,"      X11")
  line3 = paste0("     ",temp_end," KC",ymd_end  , RA_end  , Dec_end  ,"         ",mag,colour,"      X11")
  output <- c(line, line2, line3)  
  cat("Writing formatted data to ",loc,"/Linear_Fits/MPC_Format/",loc,"_",orig_ID,".mpc")
  write.table(output, paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",orig_ID,".mpc"), sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

  temp_IDs = list(temp_start, temp_mid, temp_end)
  return(temp_IDs)
}
# To call find_orb and produce x,y,z location parameters
# fo *findorb.txt -C 500 -e ./orbital_params/%p/ephem_%p.json


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

# write.table(astcheck, paste0("./",loc,"/",loc,"_astcheck.txt"), sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)






