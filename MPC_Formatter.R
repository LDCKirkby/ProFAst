library(magicaxis)
library(celestial)
library(lubridate)
library(common)


formatter <- function(loc, ID, colour, magnitude, RA_vals, Dec_vals){
    cat("Reading observation times\n")
    obs_times = read.delim("./obs_times_full.txt", header =FALSE, col.names = c("frame","obs1","obs2","obs3","obs4","obs5","obs6","obs7","obs8","obs9"),  sep = ",")
    # cat("Reading asteroid data\n")
    # asteroids = read.csv(paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
    
    # half_year = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y")
    
    find_orb = c()
    
    # for( i in 1:length(asteroids$groupID)){
    #   colour = asteroids$Colour[i]
    #   ID = asteroids$segID[i]
    #   
    #   cat("Formatting asteriod ", colour, ID,"\n")
    
    
    #N: Number (1 - 5)
    
    #T: Temporary Designation Number (6 - 12)
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
    }else if(nchar(ID) < 7){
      add = spaces(7-nchar(ID))
      ID = paste0(add,ID)
      }
  
    exposure = switch(tolower(colour),
                      "g" = 900,
                      "r" = 1080,
                      "i" = 1800) #seconds
    
    #Date of Observation (J2000.0)
    #Lots of formatting to get it into YYYY MM DD.dddddd format
    obs = subset(obs_times, subset = grepl(paste0(loc,"_",colour), obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
    obs_start = as.POSIXct(obs$obs1, tz = "UTC")
    obs_end = as.POSIXct(obs$obs5, tz = "UTC") + (exposure/5)
    obs_mid = as.POSIXct(obs$obs3, tz = "UTC")
    
    ymd_start = paste0(year(obs_start)," ")
    ymd_mid = paste0(year(obs_mid), " ")
    ymd_end = paste0(year(obs_end), " ")
    day_start = ""
    day_mid = ""
    day_end = ""
    
    if(month(obs_start) < 10){
      ymd_start = paste0(ymd_start, paste0("0",month(obs_start)), " ")
      ymd_mid = paste0(ymd_mid, paste0("0",month(obs_mid)), " ")
      ymd_end = paste0(ymd_end, paste0("0",month(obs_end)), " ")
    }else{
      ymd_start = paste0(ymd_start, month(obs_start), " ")
      ymd_mid = paste0(ymd_mid, month(obs_mid), " ")
      ymd_end = paste0(ymd_end, month(obs_end), " ")
    }
    
    if(day(obs_start) < 10){
      day_start = paste0("0",day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
      day_mid = paste0("0",day(obs_mid) + trunc((hour(obs_mid)/24 + minute(obs_mid)/(24*60) + second(obs_mid)/(24*60*60))*10^4)/10^4)
      day_end = paste0("0",day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
    }else{
      day_start = paste0(day(obs_start) + trunc((hour(obs_start)/24 + minute(obs_start)/(24*60) + second(obs_start)/(24*60*60))*10^4)/10^4)
      day_mid = paste0(day(obs_mid) + trunc((hour(obs_mid)/24 + minute(obs_mid)/(24*60) + second(obs_mid)/(24*60*60))*10^4)/10^4)
      day_end = paste0(day(obs_end) + trunc((hour(obs_end)/24 + minute(obs_end)/(24*60) + second(obs_end)/(24*60*60))*10^4)/10^4)
    }
    
    if(nchar(day_start) < 7){
      day_start = paste0(day_start, "0")
    }
    if(nchar(day_mid) < 7){
      day_mid = paste0(day_mid, "0")
    }
    if(nchar(day_end) < 7){
      day_end = paste0(day_end, "0")
    }
    ymd_start = paste0(ymd_start, day_start)
    ymd_mid = paste0(ymd_mid, day_mid)
    ymd_end = paste0(ymd_end, day_end)
  
    #Observed Magnitude and Band (66 - 71)
    #Need to check if decimals have been removed & add them back in if they have been
    mag = trunc(asteroids$mag[i]*10^2)/10^2
    if(unlist(gregexpr('[.]', mag)) == -1){
      mag = paste0(mag,".00")
    }else if((nchar(toString(mag)) - unlist(gregexpr('[.]', mag))) < 2){
      for(l in 1:(nchar(toString(mag)) - unlist(gregexpr('[.]', mag)))){
        mag = paste0(mag,"0")
      }
    }
    
    RA_first = paste0(deg2hms(RA_vals[[1]])[[1]], " ",deg2hms(RA_vals[[1]])[[2]], " ",deg2hms(RA_vals[[1]], digits = 2)[[3]])
    Dec_first = paste0(deg2dms(Dec_vals[[1]])[[1]], " ",deg2dms(Dec_vals[[1]])[[2]], " ",deg2dms(Dec_vals[[1]], digits = 2)[[3]])
    RA_end = paste0(deg2hms(RA_vals[[length(RA_vals)]])[[1]], " ",deg2hms(RA_vals[[length(RA_vals)]])[[2]], " ",deg2hms(RA_vals[[length(RA_vals)]], digits = 2)[[3]])
    Dec_end = paste0(deg2dms(Dec_vals[[length(Dec_vals)]])[[1]], " ",deg2dms(Dec_vals[[length(Dec_vals)]])[[2]], " ",deg2dms(Dec_vals[[length(Dec_vals)]], digits = 2)[[3]])
    RA_mid = paste0(deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]])[[1]], " ",deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]])[[2]], " ",deg2hms(RA_vals[[as.integer(length(RA_vals)/2)]], digits = 2)[[3]])
    Dec_mid = paste0(deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]])[[1]], " ",deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]])[[2]], " ",deg2dms(Dec_vals[[as.integer(length(Dec_vals)/2)]], digits = 2)[[3]])
    
    
    line  = paste0("     ",ID,"*KP",ymd_start, " ", RA_first, " ", Dec_first,"         ",mag,colour,"      X11")
    line2 = paste0("     ",ID," KP",ymd_mid  , " ", RA_mid  , " ", Dec_mid  ,"         ",mag,colour,"      X11")
    line3 = paste0("     ",ID," KP",ymd_end  , " ", RA_end , " ", Dec_end ,"         ",mag,colour,"      X11")
    cat(line, "\n")
    cat(line2, "\n")
    find_orb <- append(find_orb, line)
    find_orb <- append(find_orb, line2)
    find_orb <- append(find_orb, line3)
  
    
    cat("Writing formatted data to ", loc,"_findorb.txt\n")
    write.table(find_orb, paste0("./",loc,"/Linear_Fits/",loc,"_",ID,".mpc"), sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

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






