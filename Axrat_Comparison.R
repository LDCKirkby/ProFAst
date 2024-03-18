# library(celestial)
# library(devtools)
# library(Cairo)
# library(ProFound)
# library(magicaxis)
# library(data.table)
# require(foreign)
# require(MASS)
#

Axrat_Comparison <- function(loc){

possible_asteroids = read.csv(paste0("./",loc,"/Possible_Asteroids.csv"), header = TRUE, fill = TRUE)
possible_asteroids = as.data.table(possible_asteroids)

cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= 0.35 | axrat_rxt <= 0.35 | axrat_i1xt <= 0.35)


top_tail = subset(filtered_asteroids, select = c(Colour, X.1))

filtered_asteroids = distinct(subset(filtered_asteroids, select = -c(X.1, Colour)))

filtered_asteroids = cbind(subset(filtered_asteroids, select = c(groupID)), "Colour" = top_tail[top_tail$X.1 %in% filtered_asteroids$X ==TRUE,]$Colour, subset(filtered_asteroids, select = c(Ngroup)), subset(filtered_asteroids, select = -c(groupID, Ngroup)))
filtered_asteroids = setorder(filtered_asteroids, "groupID")

cat("*********\n")
cat("Filtered to ", length(filtered_asteroids$axrat_gt), "potential asteroids\n")
cat("*********\n\n")

cat("*********\n")
cat("Writing to ", paste0("./", loc,"/Filtered_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(filtered_asteroids, file = paste0("./",loc,"/Filtered_Asteroids.csv"))

#N100 filter
cat("*********\n")
cat("Performing N100 Filter\n")
N100_filtered_asteroids = subset(filtered_asteroids, N100 >= 100 | N100 >= 100 | N100 >= 100)

cat("Removed ", length(filtered_asteroids$groupID) - length(N100_filtered_asteroids$groupID), " small errors\n")

write.csv(N100_filtered_asteroids, file = paste0("./",loc,"/",loc,"_N100_Filtered_Asteroids.csv"))
cat("Writing to " ,loc, "_N100_Filtered_Asteroids.csv\n")
cat("*********\n\n")


rm(possible_asteroids, top_tail, filtered_asteroids, N100_filtered_asteroids) 
gc()


#Uncomment to produce axial ratio graphs 
#
# g_axrat <- filtered_asteroids$axrat_gt
# r_axrat <- filtered_asteroids$axrat_rxt
# i_axrat <- filtered_asteroids$axrat_i1xt
#
# axg = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_gt), stat = "bin", fill = "lightgreen") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("g Band Axial Ratio")
# axi = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_i1xt), stat = "bin", fill = "steelblue") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("i Band Axial Ratio")
# axr = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_rxt), stat = "bin", fill = "firebrick") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("r Band Axial Ratio")
# 
# 
# ggsave(paste0("./",loc,"/ast_g_axrat.png"), axg)
# ggsave(paste0("./",loc,"/ast_r_axrat.png"), axr)
# ggsave(paste0("./",loc,"/ast_i_axrat.png"), axi)


}
