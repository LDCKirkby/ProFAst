source("/Users/lkirkby/R_Files/Flux_Comparison_OLD.R")
source("/Users/lkirkby/R_Files/Axrat_Comparison_OLD.R")
source("/Users/lkirkby/R_Files/Group_Cutter_OLD.R")

all_files = as.data.frame(list.files(path = "/Users/lkirkby/", pattern = "_"))
colnames(all_files) <- c("File_Names")

done = c("156.4_3.5","157.4_3.5","158.4_3.5","159.4_3.5","1.2_-35.1","180.0_-0.5","191.0_0.5","219.0_-05.","222.0_1.5","223.0_0.5","353.0_-31.2","36.0_-34.1","6.0_-34.1","9.7_-35.1")


for(i in 1:length(all_files$File_Names)){
  if("Non" %in% all_files$File_Names[[i]] | "Files" %in% all_files$File_Names[[i]] | all_files$File_Names[[i]] %in% done){
    next
  }
  else{
    print(all_files$File_Names[[i]])
    print("Flux_Comparison time")
    Flux_Comparison(all_files$File_Names[[i]])
    print("Axrat_Comparison time")
    Axrat_Comparison(all_files$File_Names[[i]])
    print("Group_Cutter time")
    Group_Cutter(all_files$File_Names[[i]])
  }
  
}
