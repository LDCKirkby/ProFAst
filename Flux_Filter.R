# library(celestial)
# library(devtools)
# library(Cairo)
# library(ProFound)
# library(magicaxis)
# library(data.table)
# require(foreign)
# library(ggplot2)
# require(MASS)
# library(dplyr)
# #

Flux_Filter <- function(loc){

  
  
if("objectcati.csv" %in% list.files(path = paste0("./",loc,"/")) == FALSE){
  cat("*********\n")
  cat("Objectcati.csv not found\n")
  trim = readRDS(paste0("./",loc,"/stacked.rds"))
  cat_objects <- as.data.table(cbind(trim$pro_detect$segstats,trim$cat_tot))
  
  cat_groupinfo=cbind(segID=unlist(trim$pro_detect$group$groupsegID$segID),groupID=rep(trim$pro_detect$group$groupsegID$groupID,times=trim$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(trim$pro_detect$group$groupsegID$Ngroup, times=trim$pro_detect$group$groupsegID$Ngroup))
  
  cat_objects=cbind(cat_objects,cat_groupinfo[match(cat_objects$segID, cat_groupinfo[,"segID"]),2:3])
  
  cat_groups <- as.data.table(cbind(trim$pro_detect$group$groupsegID$Ngroup,trim$pro_detect$groupstats$groupID,trim$cat_grp))
  
  names(cat_groups)[1] <- "Ngroup"
  names(cat_groups)[2] <- "groupID"
  
  group_matches=match(cat_objects$segID,cat_groups$groupID,nomatch=NA)
  
  datafile0=as.data.table(cbind(cat_objects,cat_groups[group_matches,]))
  
  cat("Creating objectcati.csv\n")
  write.csv(cat_objects,file=paste0("./",loc,"/objectcati.csv"))
  
  cat("Creating groupcati.csv\n")
  write.csv(cat_groups,file=paste0("./",loc,"/groupcati.csv"))
  
  cat("Creating allcati.csv\n")
  cat("*********\n\n")
  write.csv(datafile0,file=paste0("./",loc,"/allcati.csv"))
  
  rm(trim, cat_objects, cat_groupinfo, cat_groups, group_matches, datafile0)
  gc()
}
  
  
cat_groups = read.csv(paste0("./",loc,"/objectcati.csv"))
cat("*********\n")
cat(length(cat_groups$X), " Objects Detected\n")
cat("*********\n\n")

#Remove whole rows of NA's
cat_groups = cat_groups[rowSums(is.na(cat_groups)) != ncol(cat_groups),]

#Extracting potential asteroids, based on their flux ratios
cat("*********\n")
cat("Beginning asteroid search\n")
green_objects = cbind(subset(cat_groups, subset = cat_groups$flux_gt/cat_groups$flux_rxt>=8 | cat_groups$flux_gt/cat_groups$flux_i1xt>=8), "Colour" = "g")
red_objects = cbind(subset(cat_groups, subset = cat_groups$flux_rxt/cat_groups$flux_gt>=8 | cat_groups$flux_rxt/cat_groups$flux_i1xt>=8), "Colour" = "r")
blue_objects = cbind(subset(cat_groups, subset = cat_groups$flux_i1xt/cat_groups$flux_gt>=8 | cat_groups$flux_i1xt/cat_groups$flux_gt>=8), "Colour" = "i")


#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(loc, "_")[[1]][[1]])
Dec = as.numeric(strsplit(loc, "_")[[1]][[2]])

cat("Applying edge buffer\n")

red_objects = rbind(red_objects[red_objects$RAcen >= (RA-0.5 + 0.05) & red_objects$RAcen <= (RA+0.5 + 0.05) & red_objects$Deccen >= (Dec-0.5 + 0.05) & red_objects$Deccen <= (Dec+0.5 - 0.05),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA-0.5 + 0.05) & blue_objects$RAcen <= (RA+0.5 - 0.05) & blue_objects$Deccen >= (Dec-0.5 + 0.05) & blue_objects$Deccen <= (Dec+0.5 - 0.05),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA-0.5 + 0.05) & green_objects$RAcen <= (RA+0.5 - 0.05) & green_objects$Deccen >= (Dec-0.5 + 0.05) & green_objects$Deccen <= (Dec+0.5 - 0.05),])


possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
print(length(possible_asteroids$groupID))

cat("Writing to ", paste0("./", loc,"/Possible_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(possible_asteroids, file = paste0("./",loc,"/Possible_Asteroids.csv"))

rm(blue_objects, green_objects, red_objects, possible_asteroids, cat_groups)
gc()

#Uncomment to make plots of asteroid colour distribution before and after filtering

# fg = ggplot(data = cat_groups, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("g Band Flux")
# fi = ggplot(data = cat_groups, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("i Band Flux")
# fr = ggplot(data = cat_groups, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("r Band Flux")
# 
# ggsave(filename = paste0("./",loc,"/flux_gt.png"), fg)
# ggsave(filename = paste0("./",loc,"/flux_i1xt.png"), fi)
# ggsave(filename = paste0("./",loc,"/flux_rxt.png"), fr)


# g_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("Possible Asteroids g Band Flux")
# i_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("Possible Asteroids i Band Flux")
# r_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("Possible Asteroids r Band Flux")

# ggsave(filename = paste0("./",loc,"/ast_flux_gt.png"), g_as)
# ggsave(filename = paste0("./",loc,"/ast_flux_i1xt.png"), i_as)
# ggsave(filename = paste0("./",loc,"/ast_flux_rxt.png"), r_as)

}