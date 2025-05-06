Flux_Filter <- function(loc){

#Checks to see if objectcati.csv exists, if not, checks to see if stacked.rds exists.
#If neither exist it cancells
if("objectcati.csv" %in% list.files(path = paste0("./",loc,"/")) == FALSE){
  cat("*********\n")
  cat("Objectcati.csv not found\n")
  if("stacked.rds" %in% list.files(path = paste0("./",loc,"/")) == FALSE){
    cat("stacked.rds not found either. Please ensure neccessary files exist or run detection again\n")
    break
  }
  multi_data = readRDS(paste0("./",loc,"/stacked.rds"))
  cat_objects <- as.data.table(cbind(multi_data$pro_detect$segstats,multi_data$cat_tot))
  
  cat_groupinfo=cbind(segID=unlist(multi_data$pro_detect$group$groupsegID$segID),groupID=rep(multi_data$pro_detect$group$groupsegID$groupID,times=multi_data$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(multi_data$pro_detect$group$groupsegID$Ngroup, times=multi_data$pro_detect$group$groupsegID$Ngroup))
  
  cat_objects=cbind(cat_objects,cat_groupinfo[match(cat_objects$segID, cat_groupinfo[,"segID"]),2:3])
  
  cat_groups <- as.data.table(cbind(multi_data$pro_detect$group$groupsegID$Ngroup,multi_data$pro_detect$groupstats$groupID,multi_data$cat_grp))
  
  names(cat_groups)[1] <- "Ngroup"
  names(cat_groups)[2] <- "groupID"
  
  group_matches=match(cat_objects$segID,cat_groups$groupID,nomatch=NA)
  
  datafile0=as.data.table(cbind(cat_objects,cat_groups[group_matches,]))
  
  cat("Creating objectcati.csv\n")
  write.csv(cat_objects,file=paste0("./",loc,"/objectcati.csv"), row.names=FALSE)
  
  cat("Creating groupcati.csv\n")
  write.csv(cat_groups,file=paste0("./",loc,"/groupcati.csv"), row.names=FALSE)
  
  cat("Creating allcati.csv\n")
  cat("*********\n\n")
  write.csv(datafile0,file=paste0("./",loc,"/allcati.csv"), row.names=FALSE)
  
  rm(multi_data, cat_objects, cat_groupinfo, cat_groups, group_matches, datafile0)
  gc()
}
  
cat_groups = read.csv(paste0("./",loc,"/objectcati.csv"))
cat("*********\n")
cat(length(cat_groups$X), " Objects Detected\n")
cat("*********\n\n")

#Remove whole rows of NA's
cat_groups = cat_groups[rowSums(is.na(cat_groups)) != ncol(cat_groups),]

cat_groups$flux_gt = abs(cat_groups$flux_gt)
cat_groups$flux_rxt = abs(cat_groups$flux_rxt)
cat_groups$flux_i1xt = abs(cat_groups$flux_i1xt)

#Extracting potential asteroids, based on their flux ratio
cat("*********\n")
cat("Beginning asteroid search\n")
green_objects = cbind("Colour" = "g", subset(cat_groups, subset = cat_groups$flux_gt/(cat_groups$flux_rxt + cat_groups$flux_i1xt) >= 1))
red_objects = cbind("Colour" = "r", subset(cat_groups, subset = cat_groups$flux_rxt/(cat_groups$flux_gt + cat_groups$flux_i1xt) >= 1))
blue_objects = cbind("Colour" = "i", subset(cat_groups, subset = cat_groups$flux_i1xt/(cat_groups$flux_gt + cat_groups$flux_rxt) >= 1))

#Old Method, less concise than g/r+i, r/g+i, i/g+r but unsure how other will turn out
# green_objects = cbind(subset(cat_groups, subset = cat_groups$flux_gt/cat_groups$flux_rxt>=8 | cat_groups$flux_gt/cat_groups$flux_i1xt>=8), "Colour" = "g")
# red_objects = cbind(subset(cat_groups, subset = cat_groups$flux_rxt/cat_groups$flux_gt>=8 | cat_groups$flux_rxt/cat_groups$flux_i1xt>=8), "Colour" = "r")
# blue_objects = cbind(subset(cat_groups, subset = cat_groups$flux_i1xt/cat_groups$flux_gt>=8 | cat_groups$flux_i1xt/cat_groups$flux_gt>=8), "Colour" = "i")

#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(loc, "_")[[1]][[1]])
Dec = as.numeric(strsplit(loc, "_")[[1]][[2]])

#Useful to apply edge buffer since some frames are being artificially grown
cat("Applying edge buffer\n")
red_objects = rbind(red_objects[red_objects$RAcen >= (RA - 0.5 + 0.01) & red_objects$RAcen <= (RA + 0.5 - 0.01) & red_objects$Deccen >= (Dec-0.5 + 0.01) & red_objects$Deccen <= (Dec + 0.5 - 0.01),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA - 0.5 + 0.01) & blue_objects$RAcen <= (RA + 0.5 - 0.01) & blue_objects$Deccen >= (Dec-0.5 + 0.01) & blue_objects$Deccen <= (Dec + 0.5 - 0.01),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA - 0.5 + 0.01) & green_objects$RAcen <= (RA + 0.5 - 0.01) & green_objects$Deccen >= (Dec-0.5 + 0.01) & green_objects$Deccen <= (Dec + 0.5 - 0.01),])

#Bind final lists of objects together
possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
print(length(possible_asteroids$groupID))

cat("Writing to ", paste0("./", loc,"/Possible_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(possible_asteroids, file = paste0("./",loc,"/",loc,"_Possible_Asteroids.csv"), row.names=FALSE)

rm(blue_objects, green_objects, red_objects, possible_asteroids, cat_groups)
gc()

#Uncomment to make plots of asteroid colour distribution before and after filtering
# # fg = ggplot(data = cat_groups, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("g Band Flux")
# # fi = ggplot(data = cat_groups, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("i Band Flux")
# # fr = ggplot(data = cat_groups, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("r Band Flux")
# # 
# # ggsave(filename = paste0("./",loc,"/flux_gt.png"), fg)
# # ggsave(filename = paste0("./",loc,"/flux_i1xt.png"), fi)
# # ggsave(filename = paste0("./",loc,"/flux_rxt.png"), fr)
# 
# 
# # g_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("Possible Asteroids g Band Flux")
# # i_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("Possible Asteroids i Band Flux")
# # r_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("Possible Asteroids r Band Flux")
# 
# # ggsave(filename = paste0("./",loc,"/ast_flux_gt.png"), g_as)
# # ggsave(filename = paste0("./",loc,"/ast_flux_i1xt.png"), i_as)
# # ggsave(filename = paste0("./",loc,"/ast_flux_rxt.png"), r_as)
}