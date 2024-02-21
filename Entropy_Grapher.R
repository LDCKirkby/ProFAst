library(ggplot2)
library(dplyr, quietly = TRUE)

entropy_grapher = function(loc){
found = as.data.frame(read.csv(paste0("/Users/lkirkby/",loc,"/Filtered_Asteroids.csv")))

names <- colnames(found)
#found = data.frame(row.names = names)

# #Extract asteroid data from filtered asteroid data
# for(i in 1:length(filtered$groupID)){
#   if(filtered$groupID[i] %in% asteroid_numbers == TRUE){
#     found <- rbind(found, filtered[i,])
#   }
# }
cat("Binding flux ratios\n")
#Add flux ratios to end of data
found=cbind(found[,],as.numeric(found$flux_gg/found$flux_i1xg))
found=cbind(found[,],as.numeric(found$flux_gg/found$flux_rxg))
found=cbind(found[,],as.numeric(found$flux_rxg/found$flux_i1xg))
found=cbind(found[,],as.numeric(found$flux_rxg/found$flux_gg))
found=cbind(found[,],as.numeric(found$flux_i1xg/found$flux_rxg))
found=cbind(found[,],as.numeric(found$flux_i1xg/found$flux_gg))

#Add normalized flux to end of data
found=cbind(found[,],as.numeric(found$flux_gg/(found$flux_gg + found$flux_rxg + found$flux_i1xg)))
found=cbind(found[,],as.numeric(found$flux_rxg/(found$flux_gg + found$flux_rxg + found$flux_i1xg)))
found=cbind(found[,],as.numeric(found$flux_rxg/(found$flux_gg + found$flux_rxg + found$flux_i1xg)))

#Add new column names
colnames(found) <- c(names, "girat", "grrat", "rirat", "rgrat", "irrat", "igrat", "norm_g", "norm_r", "norm_i")

cat("Binding Entropy\n")
#Add entropy to end of data
found=cbind(found[,],as.numeric(-( (found$norm_g * log(found$norm_g)) + (found$norm_i * log(found$norm_i)) + (found$norm_r * log(found$norm_r)))))
colnames(found) <- c(names, "girat", "grrat", "rirat", "rgrat", "irrat", "igrat", "norm_g", "norm_r", "norm_i", "Entropy")


#Sort asteroids based on colour
green_objects = found["g" == found$V2,]
red_objects = found["r" == found$V2,]
blue_objects = found["i" == found$V2,]

#Plot flux ratios
# gi = ggplot(data = green_objects, mapping = aes(x=girat)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("g/i") + ggtitle("g/i Band Flux Ratio ")
# gr = ggplot(data = green_objects, mapping = aes(x=grrat)) + geom_bar(stat = "bin", fill = "lightgreen")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("g/r") + ggtitle("g/r Band Flux Ratio ")
# ri = ggplot(data = red_objects, mapping = aes(x=rirat)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 17, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("r/i") + ggtitle("r/i Band Flux Ratio ")
# rg = ggplot(data = red_objects, mapping = aes(x=rgrat)) + geom_bar(stat = "bin", fill = "firebrick")+ geom_vline(xintercept = 17, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("r/g") + ggtitle("r/g Band Flux Ratio ")
# ig = ggplot(data = blue_objects, mapping = aes(x=igrat)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("i/g") + ggtitle("i/g Band Flux Ratio ")
# ir = ggplot(data = blue_objects, mapping = aes(x=irrat)) + geom_bar(stat = "bin", fill = "steelblue")+ geom_vline(xintercept = 15, colour = "red", linewidth = 1) + xlim(-50, 150) + xlab("i/r") + ggtitle("i/r Band Flux Ratio ")
# 
# #Plot axial ratios
# axg = ggplot(data = found) + geom_bar(mapping = aes(x=axrat_gg), stat = "bin", fill = "lightgreen") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("g Band Axial Ratio")
# axi = ggplot(data = found) + geom_bar(mapping = aes(x=axrat_ig), stat = "bin", fill = "steelblue") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("i Band Axial Ratio")
# axr = ggplot(data = found) + geom_bar(mapping = aes(x=axrat_rg), stat = "bin", fill = "firebrick") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("r Band Axial Ratio")

# #Save Plots
# dir.create("D:/Swap/Thesis/Final/Graphs/")
# ggsave("D:/Swap/Thesis/Final/Graphs/gi_flux.png", gi)
# ggsave("D:/Swap/Thesis/Final/Graphs/gr_flux.png", gr)
# ggsave("D:/Swap/Thesis/Final/Graphs/ri_flux.png", ri)
# ggsave("D:/Swap/Thesis/Final/Graphs/rg_flux.png", rg)
# ggsave("D:/Swap/Thesis/Final/Graphs/ig_flux.png", ig)
# ggsave("D:/Swap/Thesis/Final/Graphs/ir_flux.png", ir)
# ggsave("D:/Swap/Thesis/Final/Graphs/axg.png", axg)
# ggsave("D:/Swap/Thesis/Final/Graphs/axi.png", axi)
# ggsave("D:/Swap/Thesis/Final/Graphs/axr.png", axr)



#Read full profoundMultiband data file
cat("Reading stacked.rds\n")
all_obj = readRDS(paste0("/Users/lkirkby/",loc,"/stacked.rds"))

names <- colnames(all_obj$pro_detect$groupstats)

cat("Binding normalized flux for all objects\n")
#Add normalized flux for each asteroid in all_obj$cat_grp
all_obj$pro_detect$groupstats=cbind(all_obj$pro_detect$groupstats[,],as.numeric(all_obj$cat_grp$flux_gg/(all_obj$cat_grp$flux_gg + all_obj$cat_grp$flux_i1xg + all_obj$cat_grp$flux_rxg)))
all_obj$pro_detect$groupstats=cbind(all_obj$pro_detect$groupstats[,],as.numeric(all_obj$cat_grp$flux_rxg/(all_obj$cat_grp$flux_gg + all_obj$cat_grp$flux_i1xg + all_obj$cat_grp$flux_rxg)))
all_obj$pro_detect$groupstats=cbind(all_obj$pro_detect$groupstats[,],as.numeric(all_obj$cat_grp$flux_i1xg/(all_obj$cat_grp$flux_gg + all_obj$cat_grp$flux_i1xg + all_obj$cat_grp$flux_rxg)))
colnames(all_obj$pro_detect$groupstats) <- c(names, "norm_g","norm_r","norm_i")

#Add entropy for each asteroid in all_obj$cat_grp
all_obj$pro_detect$groupstats=cbind(all_obj$pro_detect$groupstats[,],as.numeric(-( (all_obj$pro_detect$groupstats$norm_g * log(all_obj$pro_detect$groupstats$norm_g)) + (all_obj$pro_detect$groupstats$norm_i * log(all_obj$pro_detect$groupstats$norm_i)) + (all_obj$pro_detect$groupstats$norm_r * log(all_obj$pro_detect$groupstats$norm_r)))))
colnames(all_obj$pro_detect$groupstats) <- c(names, "norm_g","norm_r","norm_i", "entropy")


#Make a new data frame of same dimensions as all_obj$pro_detect$groupstats
names <-  colnames(all_obj$pro_detect$groupstats)
group_stats = data.frame(row.names = names)
no_ast = data.frame(row.names = names)

#Extract known asteroid group stats
for(j in 1:length(all_obj$pro_detect$groupstats$groupID)){
  if(all_obj$pro_detect$groupstats$groupID[j] %in% found$groupID == TRUE){
    group_stats <- rbind(group_stats, all_obj$pro_detect$groupstats[j,])
  }
}

no_ast = anti_join(all_obj$pro_detect$groupstats, group_stats, by = "groupID")

# cat("Plotting entropy vs axrat\n")
# #Plot entropy vs axrat for each colour band
# entropy = ggplot() + xlab("Entropy") + ggtitle("Image Entropy of Objects")
# entropy = entropy  + geom_point(data = no_ast, mapping = aes(entropy, axrat))
# entropy = entropy  + geom_point(data = green_objects,  mapping = aes(Entropy, axrat_gg), colour = "lightgreen")
# entropy = entropy  + geom_point(data = red_objects,  mapping = aes(Entropy, axrat_rg), colour = "firebrick") 
# entropy = entropy  + geom_point(data = blue_objects,  mapping = aes(Entropy, axrat_ig), colour = "steelblue")

# ggsave("D:/Swap/Thesis/Final/Graphs/entropy.png", entropy)
bruh <- list("found" = found, "group_stats" = group_stats, "no_ast" = no_ast)


return(bruh)
}