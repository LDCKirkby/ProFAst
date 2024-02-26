library(pracma)
library(data.table)

#Uncomment to create ordered heading csv
bearings = as.data.frame(list.files(path = paste0(pwd()), pattern = "_"))
colnames(bearings) <- c("RA_Dec")
RA = c()
Dec = c()
for (i in 1:length(bearings$RA_Dec)){
  cat(RA_Dec)
  RA = append(RA,strsplit(bearings$RA_Dec[i], split = "_")[[1]][2])
  Dec = append(Dec, strsplit(bearings$RA_Dec[i], split = "_")[[1]][3])
  #bearings[i] =

}
# colnames(bearings) <- c("RA", "Dec")

RA_Dec = cbind(RA, Dec)

RA_Dec = as.data.frame(RA_Dec)
colnames(RA_Dec) = c("RA", "Dec")

RA_Dec = RA_Dec[order(RA_Dec$Dec, decreasing = TRUE),]

write.csv(RA_Dec, paste0(pwd(),"/done.csv"))
