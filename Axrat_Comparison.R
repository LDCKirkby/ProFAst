library(celestial)
library(devtools)
library(Cairo)
library(ProFound)
library(magicaxis)
library(data.table)
require(foreign)
require(MASS)
library(ggplot2)
#

Axrat_Comparison <- function(loc){
# cat("Enter location to save to: ")
# loc = readLines(file("stdin"),1)
wd = pwd()

possible_asteroids = read.csv(paste0(wd,"/",loc,"/Possible_Asteroids.csv"), header = TRUE, fill = TRUE)
possible_asteroids = as.data.table(possible_asteroids)
cat("Opened ", wd,"/",loc,"Possible_Asteroids.csv\n")
cat(length(possible_asteroids$axrat_gt), "potential asteroids\n")


cat("Beginning axial filtering")
filtered_asteroids <- cbind(possible_asteroids[possible_asteroids$axrat_gt <= 0.35 | possible_asteroids$axrat_rxt <= 0.35 | possible_asteroids$axrat_i1xt <= 0.35])


g_axrat <- filtered_asteroids$axrat_gt
r_axrat <- filtered_asteroids$axrat_rxt
i_axrat <- filtered_asteroids$axrat_i1xt


# axg = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_gt), stat = "bin", fill = "lightgreen") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("g Band Axial Ratio")
# axi = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_i1xt), stat = "bin", fill = "steelblue") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("i Band Axial Ratio")
# axr = ggplot(data = filtered_asteroids) + geom_bar(mapping = aes(x=axrat_rxt), stat = "bin", fill = "firebrick") + geom_vline(xintercept = 0.35, colour = "red", linewidth = 1) + ggtitle("r Band Axial Ratio")
# 
# 
# ggsave(paste0(wd,"/",loc,"/ast_g_axrat.png"), axg)
# ggsave(paste0(wd,"/",loc,"/ast_r_axrat.png"), axr)
# ggsave(paste0(wd,"/",loc,"/ast_i_axrat.png"), axi)

cat("Filtered to ", length(filtered_asteroids$axrat_gt), "potential asteroids\n")

write.csv(filtered_asteroids, file = paste0(wd,"/",loc,"/Filtered_Asteroids.csv"))

}