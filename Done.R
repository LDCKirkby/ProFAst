library(pracma)
library(data.table)

#Uncomment to create ordered heading csv
done = as.data.frame(list.files(path = paste0(pwd()), pattern = "_"))
bearings = as.data.frame(list.files(path = "/Volumes/WAVES/waves/wavesdata/kids/dr5/preprocessed/", pattern = ".5_g_"))
colnames(bearings) <- c("RA_Dec")
colnames(done) <- c("RA_Dec")
RA_done = c()
Dec_done = c()
RA = c()
Dec = c()
bruh = c()
for (i in 1:length(bearings$RA_Dec)){
  cat(bearings$RA_Dec[[i]],"\n")
  ra = strsplit(bearings$RA_Dec[i], split = "_")[[1]][2]
  dec = strsplit(bearings$RA_Dec[i], split = "_")[[1]][3]
  if(paste0(ra,"_",dec) %in% done$RA_Dec){
    next
  }
  RA = append(RA,ra)
  Dec = append(Dec, dec)

}

RA_Dec = cbind(RA, Dec)
colnames(RA_Dec) = c("RA", "Dec")


write.csv(RA_Dec, paste0(pwd(),"/todo.csv"))
