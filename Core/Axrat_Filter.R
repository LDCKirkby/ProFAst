Axrat_Filter <- function(loc){

possible_asteroids = read.csv(paste0("./",loc,"/",loc,"_Possible_Asteroids.csv"), fill = TRUE)
cat("*********\n")
cat("Beginning axial filtering\n")
cat("*********\n\n")

#Axrat filter
filtered_asteroids = subset(possible_asteroids, axrat_gt <= 0.35 | axrat_rxt <= 0.35 | axrat_i1xt <= 0.35)
filtered_asteroids = filtered_asteroids[,c(2,251,1,3:250,252)]
filtered_asteroids = setorder(filtered_asteroids, "groupID")

cat("*********\n")
cat("Filtered to ", length(filtered_asteroids$axrat_gt), "potential asteroids\n")
cat("*********\n\n")

cat("*********\n")
cat("Writing to ", paste0("./", loc,"/",loc,"Filtered_Asteroids.csv"),"\n")
cat("*********\n\n")

write.csv(filtered_asteroids, file = paste0("./",loc,"/",loc,"_Filtered_Asteroids.csv"), row.names=FALSE)

rm(possible_asteroids, filtered_asteroids) 
gc()


#Uncomment to produce axial ratio graphs 
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
