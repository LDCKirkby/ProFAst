library(data.table)

#Uncomment to create ordered heading csv
 complete = (list.files(path = "/Volumes/WAVES/lkirkby/", pattern = ".5", include.dirs = TRUE))
 average = 0
 for (i in 1:length(complete)){
   cat("Getting runtime of",complete[i],"\n")
   
   Input_Args = read.csv(paste0("/Volumes/WAVES/lkirkby/",complete[i],"/Input_Args.csv"))
   cat("Input_Args read","\n")
   runtime = Input_Args$x[9]
   cat("Runtime of ", runtime,"\n")
   
   runtime = as.double(strsplit(runtime, "Runtime: ")[[1]][2])
   if(is.na(runtime)){
     next
   }
   average = average + runtime
 }
 average = average/i
 write.csv(average, file = "/Volumes/WAVES/lkirkby/average_runtime.csv")