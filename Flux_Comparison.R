library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
library(ggplot2)
require(MASS)
library(dplyr)
#

Flux_Comparison <- function(loc){

cat_groups = read.csv(paste0("./",loc,"/objectcati.csv"))
cat("*********\n")
cat(length(cat_groups$X), " Objects Detected\n")
cat("*********\n")

#Remove whole rows of NA's
cat_groups = cat_groups[rowSums(is.na(cat_groups)) != ncol(cat_groups),]

#Extracting potential asteroids, based on their flux ratios
cat("*********\n")
cat("Beginning asteroid search\n")
cat("*********\n")
green_objects = cbind(subset(cat_groups, subset = cat_groups$flux_gt/cat_groups$flux_rxt>=15 | cat_groups$flux_gt/cat_groups$flux_i1xt>=15), "Colour" = "g")
red_objects = cbind(subset(cat_groups, subset = cat_groups$flux_rxt/cat_groups$flux_gt>=15 | cat_groups$flux_rxt/cat_groups$flux_i1xt>=15), "Colour" = "r")
blue_objects = cbind(subset(cat_groups, subset = cat_groups$flux_i1xt/cat_groups$flux_gt>=15 | cat_groups$flux_i1xt/cat_groups$flux_gt>=15), "Colour" = "i")


#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(loc, "_")[[1]][[1]])
Dec = as.numeric(strsplit(loc, "_")[[1]][[2]])

cat("*********\n")
cat("Applying edge buffer")
cat("*********\n")

red_objects = rbind(red_objects[red_objects$RAcen >= (RA-0.5 + 0.1) & red_objects$RAcen <= (RA + 0.5 - 0.1) & red_objects$Deccen >= (Dec-0.5 + 0.1) & red_objects$Deccen <= (Dec+0.5 - 0.1),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA-0.5 + 0.1) & blue_objects$RAcen <= (RA+0.5 - 0.1) & blue_objects$Deccen >= (Dec-0.5 + 0.1) & blue_objects$Deccen <= (Dec+0.5 - 0.1),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA-0.5 + 0.1) & green_objects$RAcen <= (RA+0.5 - 0.1) & green_objects$Deccen >= (Dec-0.5 + 0.1) & green_objects$Deccen <= (Dec+0.5 - 0.1),])


possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
print(length(possible_asteroids$groupID))

cat("*********\n")
cat("Writing to ", paste0("./", loc,"/Possible_Asteroids.csv"))
cat("*********\n")

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