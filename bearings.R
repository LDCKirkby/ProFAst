source("/Volumes/WAVES/lkirkby/R_Files/Pre_Proc.R")
source("/Volumes/WAVES/lkirkby/R_Files/New_Detect.R")
source("/Volumes/WAVES/lkirkby/R_Files/Flux_Comparison.R")
source("/Volumes/WAVES/lkirkby/R_Files/Axrat_Comparison.R")
source("/Volumes/WAVES/lkirkby/R_Files/Group_Cutter.R")


#Uncomment to create ordered heading csv
# bearings = as.data.frame(list.files(path = "/Volumes/WAVES/waves/wavesdata/kids/dr5/preprocessed/", pattern = "_u_"))
# colnames(bearings) <- c("RA_Dec")
# RA = c()
# Dec = c()
# for (i in 1:length(bearings$RA_Dec)){
#   RA = append(RA,strsplit(bearings$RA_Dec[i], split = "_")[[1]][2])
#   Dec = append(Dec, strsplit(bearings$RA_Dec[i], split = "_")[[1]][3])
#   #bearings[i] = 
#   
# }
# # colnames(bearings) <- c("RA", "Dec")
# 
# RA_Dec = cbind(RA, Dec)
# 
# RA_Dec = as.data.frame(RA_Dec)
# colnames(RA_Dec) = c("RA", "Dec")
# 
# RA_Dec = RA_Dec[order(RA_Dec$Dec, decreasing = TRUE),]
# 
# write.csv(RA_Dec, "/Users/lkirkby/bearings.csv")


dir = getwd()
kids = as.data.frame(read.csv("/Volumes/WAVES/lkirkby/bearings.csv"))
for(i in 1:length(kids$RA)){
  if(grepl(".", kids$RA[i], fixed = TRUE)){
  RA_DEC = paste0(kids$RA[i],"_",kids$Dec[i])
  }
  else{
    RA_DEC = paste0(kids$RA[i],".0_",kids$Dec[i])
  }
  cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
  data <- Pre_Proc(RA_DEC)
  frames = data[[1]]
  run_time = data[[2]]
  all_data = New_Detect(RA_DEC, frames, run_time)
  
  
  Flux_Comparison(RA_DEC)
  Axrat_Comparison(RA_DEC)
  Group_Cutter(RA_DEC)
  
  
  #Uncomment to remove any unwanted files
  file.remove(paste0("/Volumes/WAVES/lkirkby/",RA_DEC,"/stacked.rds"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/allcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/groupcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/objectcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/Possible_Asteroids.csv"))
  
  
  
  
}