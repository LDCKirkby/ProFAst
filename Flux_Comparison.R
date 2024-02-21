library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
library(ggplot2)
require(MASS)
#

Flux_Comparison <- function(loc){
# cat("Enter location to save to: ")
# loc = readLines(file("stdin"),1)
wd = pwd()
rdsfile = readRDS(paste0(wd,"/",loc,"/stacked.rds"))
cat("Opened stacked.rds\n")
cat(length(rdsfile$pro_detect$group$groupsegID$Ngroup), "objects detected\n")
cat_groups = read.csv(paste0(wd,"/",loc,"/objectcati.csv"))

# cat_objects <- as.data.table(cbind(rdsfile$pro_detect$segstats,rdsfile$cat_tot))
# cat_groupinfo=cbind(segID=unlist(rdsfile$pro_detect$group$groupsegID$segID),groupID=rep(rdsfile$pro_detect$group$groupsegID$groupID,times=rdsfile$pro_detect$group$groupsegID$Ngroup), Ngroup=rep(rdsfile$pro_detect$group$groupsegID$Ngroup, times=rdsfile$pro_detect$group$groupsegID$Ngroup))
# cat_objects=cbind(cat_objects,cat_groupinfo[match(cat_objects$segID, cat_groupinfo[,"segID"]),2:3])
# 
# cat_groups <- as.data.table(cbind(rdsfile$pro_detect$group$groupsegID$Ngroup, rdsfile$pro_detect$groupstats$groupID, rdsfile$cat_grp))
# names(cat_groups)[1] <- "Ngroup"
# names(cat_groups)[2] <- "groupID"
# 
# group_matches=match(cat_objects$segID,cat_groups$groupID,nomatch=NA)
# datafile0=as.data.table(cbind(cat_objects,cat_groups[group_matches,]))
# 
# #cat_groups <- as.data.table(cbind(rdsfile$pro_detect$group$groupsegID$Ngroup,rdsfile$pro_detect$groupstats$groupID,rdsfile$cat_grp))
# names(cat_groups)[1] <- "Ngroup"
# names(cat_groups)[2] <- "groupID"
flux_gt <- cat_groups$flux_gt
flux_rxt <- cat_groups$flux_rxt
flux_i1xt <- cat_groups$flux_i1xt

possible_asteroids = data.table()

cat_groups <- subset(cat_groups, select = -3)

# fg = ggplot(data = cat_groups, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("g Band Flux")
# fi = ggplot(data = cat_groups, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("i Band Flux")
# fr = ggplot(data = cat_groups, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("r Band Flux")
# 
# ggsave(filename = paste0(wd,"/",loc,"/flux_gt.png"), fg)
# ggsave(filename = paste0(wd,"/",loc,"/flux_i1xt.png"), fi)
# ggsave(filename = paste0(wd,"/",loc,"/flux_rxt.png"), fr)

green_objects = data.frame()
red_objects = data.frame()
blue_objects = data.frame()

cat("Beginning asteroid search\n")
green_objects=rbind(cat_groups[flux_gt/flux_rxt>=15 | flux_gt/flux_i1xt>=15,],"g")
red_objects=rbind(cat_groups[flux_rxt/flux_gt>=15 | flux_rxt/flux_i1xt>=15,],"r")
blue_objects=rbind(cat_groups[flux_i1xt/flux_gt>=15 | flux_i1xt/flux_gt>=15,],"i")

RA = as.numeric(strsplit(loc, "_")[[1]][[1]])
Dec = as.numeric(strsplit(loc, "_")[[1]][[2]])

print("*********")
red_objects = rbind(red_objects[red_objects$RAcen >= (RA-0.5 + 0.1) & red_objects$RAcen <= (RA + 0.5 - 0.1) & red_objects$Deccen >= (Dec-0.5 + 0.1) & red_objects$Deccen <= (Dec+0.5 - 0.1),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA-0.5 + 0.1) & blue_objects$RAcen <= (RA+0.5 - 0.1) & blue_objects$Deccen >= (Dec-0.5 + 0.1) & blue_objects$Deccen <= (Dec+0.5 - 0.1),])
green_objects = rbind(blue_objects[green_objects$RAcen >= (RA-0.5 + 0.1) & green_objects$RAcen <= (RA+0.5 - 0.1) & green_objects$Deccen >= (Dec-0.5 + 0.1) & green_objects$Deccen <= (Dec+0.5 - 0.1),])


possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
print(length(possible_asteroids$groupID))

flux_gt_as <- possible_asteroids$flux_gt
flux_rxt_as <- possible_asteroids$flux_rxt
flux_i1xt_as <- possible_asteroids$flux_i1xt

possible_asteroids <- subset(possible_asteroids, select = -3)

# g_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_gt)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("g") + ggtitle("Possible Asteroids g Band Flux")
# i_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_i1xt)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("i") + ggtitle("Possible Asteroids i Band Flux")
# r_as = ggplot(data = possible_asteroids, mapping = aes(x=flux_rxt)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-20, 20) + xlab("r") + ggtitle("Possible Asteroids r Band Flux")

# ggsave(filename = paste0(wd,"/",loc,"/ast_flux_gt.png"), g_as)
# ggsave(filename = paste0(wd,"/",loc,"/ast_flux_i1xt.png"), i_as)
# ggsave(filename = paste0(wd,"/",loc,"/ast_flux_rxt.png"), r_as)

write.csv(possible_asteroids, file = paste0(wd,"/",loc,"/Possible_Asteroids.csv"))
}