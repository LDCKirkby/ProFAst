Flux_Filter <- function(RA_DEC, flux_value=1, edge_buffer=0.001){

#Checks to see if objectcati.csv exists, if not, checks to see if stacked.rds exists.
#If neither exist it cancels
if("objectcati.csv" %in% list.files(path = paste0("./",RA_DEC,"/")) == FALSE){
  cat("*********\n")
  cat("Objectcati.csv not found\n")
  if("stacked.rds" %in% list.files(path = paste0("./",RA_DEC,"/")) == FALSE){
    cat("stacked.rds not found either. Please ensure necessary files exist or run detection again\n")
    break
  }
  multi_data = readRDS(paste0("./",RA_DEC,"/stacked.rds"))
  cat_objects <- as.data.table(cbind(multi_data$pro_detect$segstats,multi_data$cat_tot))
  
  cat_groupinfo=cbind(segID=unlist(multi_data$pro_detect$group$groupsegID$segID),groupID=rep(multi_data$pro_detect$group$groupsegID$groupID,times=multi_data$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(multi_data$pro_detect$group$groupsegID$Ngroup, times=multi_data$pro_detect$group$groupsegID$Ngroup))
  
  cat_objects=cbind(cat_objects,cat_groupinfo[match(cat_objects$segID, cat_groupinfo[,"segID"]),2:3])
  
  cat_groups <- as.data.table(cbind(multi_data$pro_detect$group$groupsegID$Ngroup,multi_data$pro_detect$groupstats$groupID,multi_data$cat_grp))
  
  names(cat_groups)[1] <- "Ngroup"
  names(cat_groups)[2] <- "groupID"
  
  group_matches=match(cat_objects$segID,cat_groups$groupID,nomatch=NA)
  
  datafile0=as.data.table(cbind(cat_objects,cat_groups[group_matches,]))
  
  cat("Creating objectcati.csv\n")
  write.csv(cat_objects,file=paste0("./",RA_DEC,"/objectcati.csv"), row.names=FALSE)
  
  cat("Creating groupcati.csv\n")
  write.csv(cat_groups,file=paste0("./",RA_DEC,"/groupcati.csv"), row.names=FALSE)
  
  cat("Creating allcati.csv\n")
  cat("*********\n\n")
  write.csv(datafile0,file=paste0("./",RA_DEC,"/allcati.csv"), row.names=FALSE)
  
  rm(multi_data, cat_objects, cat_groupinfo, cat_groups, group_matches, datafile0)
  gc()
}
  
cat_groups = read.csv(paste0("./",RA_DEC,"/allcati.csv"))
cat("*********\n")
cat(length(cat_groups$X), " Objects Detected\n")
cat("*********\n\n")

#Remove whole rows of NA's
cat_groups = cat_groups[rowSums(is.na(cat_groups)) != ncol(cat_groups),]

#Remove potential error fluxes that can be small negative numbers
cat_groups$flux_gt[cat_groups$flux_gt < 0] <- 0
cat_groups$flux_rxt[cat_groups$flux_rxt < 0] <- 0
cat_groups$flux_i1xt[cat_groups$flux_i1xt < 0] <- 0
#cat_groups$flux_gt = abs(cat_groups$flux_gt)
#cat_groups$flux_rxt = abs(cat_groups$flux_rxt)
#cat_groups$flux_i1xt = abs(cat_groups$flux_i1xt)

#Extracting potential asteroids, based on their flux ratio
cat("*********\n")
cat("Beginning asteroid search\n")
green_objects = cbind("Colour" = "g", subset(cat_groups, subset = cat_groups$flux_gt/(cat_groups$flux_rxt + cat_groups$flux_i1xt) >= flux_value))
red_objects = cbind("Colour" = "r", subset(cat_groups, subset = cat_groups$flux_rxt/(cat_groups$flux_gt + cat_groups$flux_i1xt) >= flux_value))
blue_objects = cbind("Colour" = "i", subset(cat_groups, subset = cat_groups$flux_i1xt/(cat_groups$flux_gt + cat_groups$flux_rxt) >= flux_value))

#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(RA_DEC, "_")[[1]][[1]])
Dec = as.numeric(strsplit(RA_DEC, "_")[[1]][[2]])

#Useful to apply edge buffer since some frames are being artificially grown
cat("Applying edge buffer\n")
red_objects = rbind(red_objects[red_objects$RAcen >= (RA - 0.5 + edge_buffer) & red_objects$RAcen <= (RA + 0.5 - edge_buffer) & red_objects$Deccen >= (Dec-0.5 + edge_buffer) & red_objects$Deccen <= (Dec + 0.5 - edge_buffer),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA - 0.5 + edge_buffer) & blue_objects$RAcen <= (RA + 0.5 - edge_buffer) & blue_objects$Deccen >= (Dec-0.5 + edge_buffer) & blue_objects$Deccen <= (Dec + 0.5 - edge_buffer),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA - 0.5 + edge_buffer) & green_objects$RAcen <= (RA + 0.5 - edge_buffer) & green_objects$Deccen >= (Dec-0.5 + edge_buffer) & green_objects$Deccen <= (Dec + 0.5 - edge_buffer),])

#Bind final lists of objects together
possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
cat(length(possible_asteroids$groupID), " potential asteroids in data\n")
cat("Writing to ", paste0("./", RA_DEC,"/Possible_Asteroids.csv"),"\n")
cat("*********\n\n")

#Write data to file
write.csv(possible_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_Possible_Asteroids.csv"), row.names=FALSE)

rm(blue_objects, green_objects, red_objects, possible_asteroids, cat_groups)
gc()
}