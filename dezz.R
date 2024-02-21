source("./R_Files/Pre_Proc.R")
source("./R_Files/New_Detect.R")
source("./R_Files/Flux_Comparison.R")
source("./R_Files/Axrat_Comparison.R")
source("./R_Files/Group_Cutter.R")
source("./R_Files/memobj.R")
library(peakRAM)


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


  bruh = commandArgs()
  num_args = length(bruh)
  print(bruh[[num_args]])
  i = as.numeric(bruh[[num_args]])
  dir = getwd()
  kids = as.data.frame(read.csv("./bearings.csv"))
  
  
    
  if(grepl(".", kids$RA[i], fixed = TRUE)){
    RA_DEC = paste0(kids$RA[i],"_",kids$Dec[i])
  }else{
    RA_DEC = paste0(kids$RA[i],".0_",kids$Dec[i])
  }
  cat("*************\n","Beginning Detection on:",RA_DEC,"\n","*************\n")
  Pre_Proc_RAM = peakRAM(data <- Pre_Proc(RA_DEC))
  post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", Pre_Proc_RAM$Elapsed_Time_sec, 
                           "\nTotal RAM used during Pre_Proc for ", RA_DEC," :", Pre_Proc_RAM$Total_RAM_Used_MiB,
                           "\nMax RAM used during Pre_Proc for ", RA_DEC," :", Pre_Proc_RAM$Peak_RAM_Used_MiB))
  frames = data[[1]]
  run_time = data[[2]]
  New_Detect_RAM = peakRAM(all_data = New_Detect(RA_DEC, frames, run_time))
  post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", New_Detect_RAM$Elapsed_Time_sec, 
                           "\nTotal RAM used during New_Detect for ", RA_DEC," :", New_Detect_RAM$Total_RAM_Used_MiB,
                           "\nMax RAM used during New_Detect for ", RA_DEC," :", New_Detect_RAM$Peak_RAM_Used_MiB))
  
  Flux_Comparison_RAM = peakRAM(Flux_Comparison(RA_DEC))
  post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", Flux_Comparison_RAM$Elapsed_Time_sec, 
                           "\nTotal RAM used during Flux_Comparison for ", RA_DEC," :", Flux_Comparison_RAM$Total_RAM_Used_MiB,
                           "\nMax RAM used during Flux_Comparison for ", RA_DEC," :", Flux_Comparison_RAM$Peak_RAM_Used_MiB))
  
  Axrat_Comparison_RAM = peakRAM(Axrat_Comparison(RA_DEC))
  post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", Axrat_Comparison_RAM$Elapsed_Time_sec, 
                           "\nTotal RAM used during Axrat_Comparison for ", RA_DEC," :", Axrat_Comparison_RAM$Total_RAM_Used_MiB,
                           "\nMax RAM used during Axrat_Comparison for ", RA_DEC," :", Axrat_Comparison_RAM$Peak_RAM_Used_MiB))
  
  Group_Cutter_RAM = peakRAM(Group_Cutter(RA_DEC, frames))
  post_text_to_ntfy(paste0("Time elapsed for ",RA_DEC," :", Group_Cutter_RAM$Elapsed_Time_sec, 
                           "\nTotal RAM used during Group_Cutter for ", RA_DEC," :", Group_Cutter_RAM$Total_RAM_Used_MiB,
                           "\nMax RAM used during Group_Cutter for ", RA_DEC," :", Group_Cutter_RAM$Peak_RAM_Used_MiB))
  
  
  #Uncomment to remove any unwanted files
  #file.remove(paste0("/Volumes/WAVES/lkirkby/",RA_DEC,"/stacked.rds"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/allcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/groupcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/objectcati.csv"))
  #file.remove(paste0("/Users/lkirkby/",RA_DEC,"/Possible_Asteroids.csv"))
  
  
  
  
