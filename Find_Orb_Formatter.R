library(magicaxis)
library(celestial)

asteroids = read.csv("D:/Swap/Thesis/206.6_2.5/Filtered_Asteroids.csv")
output = c()

for( i in 1:length(asteroids$X)){
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
  line = paste0("     ",asteroids$groupID[[i]],spacer1, "  ", "C2017 05 02.0407", " ", RA_top, " ", Dec_top,"                      X11")
  line2 = paste0("     ",asteroids$groupID[[i]],spacer1, "  ", "C2017 05 02.0410", " ", RA_bottom, " ", Dec_bottom,"                      X11")
  output <- append(output, line)
  output <- append(output, line2)
  print(nchar(line))
}

write.table(output, "D:/Swap/Thesis/206.6_2.5/MPC_Format.txt", sep = "X11", row.names = FALSE, col.names = FALSE)
