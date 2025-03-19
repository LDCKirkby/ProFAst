library(magicaxis)
library(celestial)
library(lubridate)  
library(common)

get_half_month_letter <- function(posix_time) {
  # Convert POSIX time to date
  date <- as.Date(posix_time)

  # Get the month and day
  month <- format(date, "%m")
  day <- as.numeric(format(date, "%d"))

  # Define the mapping for the half-month letters
  half_month_map <- data.frame(
    letter = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y"),
    month_start = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12),
    day_threshold = c(15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16)
  )

  # Determine the letter based on the month and day
  for (i in 1:nrow(half_month_map)) {
    if (as.numeric(month) == half_month_map$month_start[i]) {
      if (day <= half_month_map$day_threshold[i]) {
        return(half_month_map$letter[i * 2 - 1])  # First half of the month
      } else {
        return(half_month_map$letter[i * 2])  # Second half of the month
      }
    }
  }

  # Return NA if the date doesn't match any criteria
  return("0")
}

PSV_maker <- function(loc, ID, colour, magnitude, RA_vals, Dec_vals){
    orig_ID = ID
    obs_times = read.delim("./obs_times_full.txt", header =FALSE, col.names = c("frame","obs1","obs2","obs3","obs4","obs5","obs6","obs7","obs8","obs9"),  sep = ",")

    exposure = switch(tolower(colour),
                    "g" = 900,
                    "r" = 1800,
                    "i" = 1200) #seconds

    #Date of Observation (J2000.0)
    obs = subset(obs_times, subset = grepl(paste0(loc,"_",colour), obs_times$frame) == TRUE & grepl("i2", obs_times$frame) ==FALSE)
    obs_start = as.POSIXct(obs$obs1, tz = "UTC")
    obs_mid = as.POSIXct(obs$obs3, tz = "UTC")
    obs_end = as.POSIXct(obs$obs5, tz = "UTC") + (exposure/5)

    iso_start = paste0(format_ISO8601(obs_start, usetz="Z"),"   ")
    iso_mid = paste0(format_ISO8601(obs_mid, usetz="Z"),"   ")
    iso_end = paste0(format_ISO8601(obs_end, usetz="Z"),"   ")

    start_half_month <- get_half_month_letter(obs_start)
    mid_half_month <- get_half_month_letter(obs_mid)
    end_half_month <- get_half_month_letter(obs_end)

    #T: Temporary Designation Number (6 - 12)
    base36 <- as.character(as.hexmode(ID))
    # Convert the base36 to a string of alphanumeric characters
    base36_alph <- toupper(paste0(base36, collapse=""))
    # Ensure that the result is exactly two characters
    ID_3_dig <- substr(base36_alph, nchar(base36_alph)-2, nchar(base36_alph))
    temp_start = paste0("K", substr(format(obs_start, "%Y"), 3, 4), start_half_month,ID_3_dig)
    temp_mid = paste0("K", substr(format(obs_mid, "%Y"), 3, 4), mid_half_month,ID_3_dig)
    temp_end = paste0("K", substr(format(obs_end, "%Y"), 3, 4), end_half_month,ID_3_dig)

    RA_start = formatC(RA_vals[[1]], width=11, format="f", flag="-")
    Dec_start = formatC(Dec_vals[[1]], width=11, digits=2, format="f", flag="- ")
    RA_mid = formatC(RA_vals[[as.integer(length(RA_vals)/2)]], width=11, format="f", flag="-")
    Dec_mid = formatC(Dec_vals[[as.integer(length(Dec_vals)/2)]], width=11, digits=2, format="f", flag="- ")
    RA_end = formatC(RA_vals[[length(RA_vals)]], width=11, format="f", flag="-")
    Dec_end = formatC(Dec_vals[[length(Dec_vals)]], width=11, digits=2, format="f", flag="- ")

    mag = formatC(as.numeric(magnitude), digits=2, format="f")          
    colour = formatC(colour, format="s", width=4, flag="- ")

    col_vals = c("trkSub  ","mode","stn ","obsTime                ","ra         ","dec        ","mag  ","band","remarks")
    start = c(temp_start," CCD","X11 ",iso_start,RA_start,Dec_start,mag,colour,paste0("surveyID:",ID))
    mid = c(temp_mid," CCD","X11 ",iso_mid,RA_mid,Dec_mid,mag,colour,paste0("surveyID:",ID))
    end = c(temp_end," CCD","X11 ",iso_end,RA_end,Dec_end,mag,colour,paste0("surveyID:",ID))
    psv_output= as.data.table(rbind(start,mid,end))
    colnames(psv_output) <- col_vals
    write.table(psv_output, paste0("./",loc,"/Linear_Fits/MPC_Format/",loc,"_",orig_ID,".psv"), sep = "|", row.names=FALSE, quote=FALSE)
}