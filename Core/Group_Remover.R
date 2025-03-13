Group_Remover.R <- function(RA_Dec){
    asteroid_data = read.csv(paste0("./",RA_Dec,"/",RA_Dec,"_Verified.csv"), header=TRUE)
    no_dupes = subset(asteroid_data, !duplicated(groupID))
    dupes = subset(asteroid_data, duplicated(groupID))
    
    write.csv(no_dupes, paste0("./",RA_Dec,"/",RA_Dec,"_no_dupes.csv"), col.names=TRUE, row.names=FALSE)
    write.table(dupes$segID, paste0("./",RA_Dec,"/",RA_Dec,"_dupes.txt"), col.names=FALSE, row.names=FALSE)
}

args = commandArgs(trailingOnly = TRUE)
RA_DEC = gsub("[\r\n]", "", as.character(args[[1]]))