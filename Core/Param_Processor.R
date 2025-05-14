library(rjson)
library(data.table)
library(ggplot2)
library(stringr)
library(fmsb)
library(tidyverse)
library(viridis)
library(ggridges)
library(paletteer)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...){
    radarchart(
        data, axistype = 2,
        # Customize the polygon
        pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
        # Customize the grid
        cglcol = "grey", cglty = 1, cglwd = 0.8,
        # Customize the axis
        axislabcol = "grey", 
        # Variable labels
        vlcex = vlcex, vlabels = vlabels,
        caxislabels = caxislabels, title = title, ...
    )
}

find_orb_params <- function(survey_ID, dt){
    params <- rjson::fromJSON(file = paste0("./Orbital_Params/",survey_ID,"/elem_",survey_ID,".json"))
    elements =  params$objects[[1]]$elements
    
    new_row = as.data.frame(t(rep(NA,8)))
    # names = c("survey_ID","JPL_ID","a","P","M","n","e","peri","Q","i",
    # "a_sigma","P_sigma","M_sigma","n_sigma","e_sigma","peri_sigma","Q_sigma","i_sigma","r")
    names = c("survey_ID","a","e","i","w", "N","M","P")
    colnames(new_row) = names
    new_row$survey_ID = survey_ID

    # elem_names = c("survey_ID","JPL_ID","a","P","M","n","e","q","Q","i",
    # "a sigma","P sigma","M sigma","n sigma","e sigma","q sigma","Q sigma","i sigma")
    elem_names = c("a","e","i","arg_per", "asc_node","M","P")

    for(i in 1:7){
        if(is.null(elements[[elem_names[i]]])){
            next
        }else{
            new_row[names[i+1]] = elements[[elem_names[i]]]
        }
    }
    #new_row$r = sqrt(new_row$peri * new_row$Q)
    if(new_row$i > 90){
        new_row$i <- 180-new_row$i
    }
    dt <- rbind(dt, new_row)
    return(dt)
}

JPL_params <- function(JPL_ID, dt){
    param <- rjson::fromJSON(file = paste0("./AST_JSONS/",JPL_ID))
    IDS = str_split(JPL_ID, "_")
    survey = IDS[[1]][1]
    JPL = str_split(IDS[[1]][2], "[.]")[[1]][1]

    elem = param$orbit$elements
    new_row = as.data.frame(t(rep(NA,8)))
    names = c("survey_ID","a","e","i","w","N","M","P")
    colnames(new_row) = names
    rownames(new_row) = JPL
    new_row$survey_ID = survey
    #"Semi-major Axis", "Eccentricity", "Inclination","Argument of Perihelion","Longitude of the Ascending Node","Mean Anomaly","Period"
    for(item in elem){
        switch(item$name,
               "a" = {new_row$a <- as.double(item$value)},
               "e" = {new_row$e <- as.double(item$value)},
               "i" = {new_row$i <- as.double(item$value)},
               "w" = {new_row$w <- as.double(item$value)},
               "om"= {new_row$N <- as.double(item$value)},
               "ma"= {new_row$M <- as.double(item$value)},
               "per"={new_row$P <- as.double(item$value)}
        )
    }
    dt <- rbind(dt, new_row)
    return(dt)
}

calculate_rmse_object <- function(estimations, truth) {
  sqrt(rowMeans((estimations - truth)^2))
}

calculate_rmse_parameter <- function(estimations, truth){
  sqrt(colMeans((estimations - truth)^2))
}

asteroid_IDS <- list.dirs(path="./Orbital_Params/",recursive=FALSE,full.names=FALSE)
asteroid_params <- data.frame()
for(asteroid in asteroid_IDS){
    asteroid_params <- find_orb_params(asteroid, asteroid_params)
}
asteroid_params <- na.omit(asteroid_params)

JPL_IDS = list.files(path="./AST_JSONS/", recursive=FALSE, full.names=FALSE)
JPL_param <- data.frame()
for(JPL in JPL_IDS){
    JPL_param <- JPL_params(JPL, JPL_param)
}
JPL_param <- na.omit(JPL_param)

max_min <- data.frame(survey_ID=c(NA,NA), a=c(10,0), e=c(1,0), i=c(90,0), w=c(400,0), N=c(360,0),M=c(360,0), P=c(12500,0))
rownames(max_min)= c("max","min")
title_names = c("Semi-major Axis (AU)", "Eccentricity", "Inclination","Argument of Perihelion","Longitude of the Ascending Node","Mean Anomaly","Period")

for(ID in JPL_param$survey_ID){
    target_ast = asteroid_params[asteroid_params$survey_ID==ID ,]
    target_JPL = JPL_param[JPL_param$survey_ID == ID,]

    # error <- calculate_rmse(as.numeric(target_ast[,2:8]), as.numeric(target_JPL[,2:8]))
    # ast_error <- data.frame(ID=ID, error=error)
    # RMSE <- as.data.frame(rbind(RMSE,ast_error))

    # data = rbind(max_min, target_ast, target_JPL)
    png(filename=paste0("./",ID,"_radar_graph.png"), height=1200, width = 1200, pointsize = 17)
    par(mfrow=c(1,1),mar=c(2,2,2.5,2), family="Arial")
    create_beautiful_radarchart(data[,2:8], color = c("#00AFBB", "#E7B800"), title=paste0("JPL ID:",rownames(target_JPL), " Survey ID: ", ID), vlabels=title_names)
    legend(
    x = "bottom", legend = c("Calculated","JPL"), horiz = TRUE, 
        bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
        text.col = "black", cex = 1, pt.cex = 1.5,
    )
    dev.off()
}
matched = subset(asteroid_params, subset=survey_ID %in% JPL_param$survey_ID)
RMSE_object <- as.data.frame(calculate_rmse_object(matched[,2:8],JPL_param[,2:8]))
RMSE_object <- as.data.frame(cbind(RMSE_object,matched$survey_ID))
colnames(RMSE_object) = c("error","ID")
RMSE_parameter <- as.data.frame(calculate_rmse_parameter(matched[,2:8],JPL_param[,2:8]))
RMSE_parameter <- as.data.frame(cbind(RMSE_parameter, rownames(RMSE_parameter)))
colnames(RMSE_parameter) <- c("error","parameter")
range_ground_truth <- c(15,1,90,max(JPL_param$w)-min(JPL_param$w), 360, max(JPL_param$M)-min(JPL_param$M), max(JPL_param$P)-min(JPL_param$P))#apply(JPL_param[,2:8], 2, function(x) max(x) - min(x))
normalised_RMSE_parameter <- RMSE_parameter
normalised_RMSE_parameter$error <- RMSE_parameter$error/range_ground_truth
normalised_RMSE_parameter <- as.data.table(cbind(normalised_RMSE_parameter, title_names))

# names = c("survey_ID","JPL_ID","a","e","i","w", "N","M","P",
#               "a","e","i","w", "N","M","P")
graph_values = c("a","e","i","w","N","M","P")
JPL_values = c("a","e","i","w","N","M","P")
lower_bounds = c(0,0,0,0,0,0,0)
upper_bounds = c(10,1,90,400,360,360,12500)
colours = viridis_pal(option = "C")(7)
i=1
for(value in graph_values){
    cat(value,"\n")
    name = title_names[i]
    lower = lower_bounds[i]
    upper = upper_bounds[i]
    
    #Calculated
    plot <- ggplot(data=asteroid_params, aes(x=.data[[paste0(value)]])) + 
            geom_histogram(aes(y=after_stat(density)), fill = colours[i], alpha=0.3, bins=50)+
            geom_density(fill = colours[i], alpha = 0.4) +
            xlab(name) + ggtitle(paste0("Calculated Asteroid Distribution: ",name)) + scale_x_continuous(limits = c(lower,upper))
    ggsave(filename = paste0("./Elements/",value,"_plot.png"), plot)
    #Calculated & JPL
    plot <- ggplot() + 
            geom_density(data=asteroid_params, aes(x=.data[[paste0(value)]], fill="Calculated"), alpha = 0.3) + 
            geom_histogram(data=asteroid_params, aes(x=.data[[paste0(value)]],y=after_stat(density), fill="Calculated"), alpha=0.3, bins=50) +
            geom_density(data=JPL_param, aes(x=.data[[paste0(value)]], fill="JPL"), alpha = 0.3) + xlab(name) + 
            geom_histogram(data=JPL_param, aes(x=.data[[paste0(value)]],y=after_stat(density), fill="JPL"), alpha=0.3, bins=50) +
            ggtitle(paste0("Calculated Asteroid Distribution: ",name)) + scale_x_continuous(limits = c(lower,upper)) + #scale_y_continuous(expand=c(0,1)) +
            scale_fill_manual(name="Source", breaks=c("Calculated","JPL"), values=c("Calculated"="#00AFBB","JPL"="#E7B800")) +
            theme(legend.title=element_text(size=10),legend.text=element_text(size=8),legend.position="bottom")
    ggsave(filename = paste0("./Elements/",value,"_JPL_v_Calculated_plot.png"), plot)
    i = i+1
}       
kirkwood = data.frame(xintercepts = c(2.502, 2.825, 2.958, 3.279), names=c("3:1", "5:2", "7:3", "2:1"))
plot <- ggplot(data=asteroid_params, aes(x=a)) + geom_density(fill = "seagreen3", alpha = 0.3) +
        geom_histogram(aes(y=after_stat(density)), fill = "seagreen3", alpha=0.4, bins=50)+
        xlab("Semi-Major Axis (AU)") + ggtitle(paste0("Calculated Asteroid Distribution: Semi-Major Axis")) + 
        scale_x_continuous(limits = c(0,10))
plot <- plot + geom_vline(data=kirkwood, aes(xintercept=xintercepts, color=names), alpha =0.85) + 
            scale_color_viridis_d(name = "Kirkwood Gaps (Resonance w/ Jupiter)", option="plasma")+ 
            theme(legend.title=element_text(size=10),legend.text=element_text(size=8),legend.position="bottom")
ggsave(filename = paste0("./Elements/a_plot.png"), plot)
plot <- ggplot() + 
            geom_density(data=asteroid_params, aes(x=a, fill="Calculated"), alpha = 0.3) + 
            geom_histogram(data=asteroid_params, aes(x=a,y=after_stat(density), fill="Calculated"), alpha=0.3, bins=50) +
            geom_density(data=JPL_param, aes(x=a, fill="JPL"), alpha = 0.3) + 
            geom_histogram(data=JPL_param, aes(x=a,y=after_stat(density), fill="JPL"), alpha=0.3, bins=50) +
            xlab("Semi-Major Axis (AU)") + 
            ggtitle(paste0("Calculated Asteroid Distribution: Semi-Major Axis")) + scale_x_continuous(limits = c(0,10)) + #scale_y_continuous(expand=c(0,1)) +
            scale_fill_manual(name="Source", breaks=c("Calculated","JPL"), values=c("Calculated"="#00AFBB","JPL"="#E7B800"))
plot <- plot + geom_vline(data=kirkwood, aes(xintercept=xintercepts, color=names), alpha =0.85) + 
            scale_color_viridis_d(name = "Kirkwood Gaps (Resonance w/ Jupiter)", option="plasma")+ 
            theme(legend.title=element_text(size=10),legend.text=element_text(size=8),legend.position="bottom")
ggsave(filename = paste0("./Elements/a_JPL_v_Calculated_plot.png"), plot)

1.780, 2.065, 3.972, 4.296
"1.780 AU", "2.065 AU", "3.972 AU", "4.296 AU"
# For reading in lots of csv's at once
# all_sources=data.frame()
# for(file in csv_files){
#   print(file)
#   temp = read.csv(file)
#   grouped = as.data.frame(cbind(temp$N100, temp$groupID, temp$segID, temp$mag, temp$mag_gt, temp$mag_rxt, temp$mag_i1xt, temp$RAcen, temp$Deccen, temp$Colour))
#   all_sources = as.data.frame(rbind(all_sources, grouped))
# }
# colnames(all_sources) = c("N100","groupID","segID","mag","mag_gt","mag_rxt","mag_i1xt","RAcen","Deccen","Colour")

# cat_groups = read.csv(file)
# for(i in 1:length(cat_groups$segID)){
#   print(i)
#   g_ratio = cat_groups$flux_gt[i]/(cat_groups$flux_rxt[i] + cat_groups$flux_i1xt[i])
#   r_ratio = cat_groups$flux_rxt[i]/(cat_groups$flux_gt[i] + cat_groups$flux_i1xt[i])
#   i_ratio = cat_groups$flux_i1xt[i]/(cat_groups$flux_gt[i] + cat_groups$flux_rxt[i])
#   biggest = max(c(g_ratio, r_ratio, i_ratio))
#   if(biggest == g_ratio){
#     cat_groups$Colour[i] = 'g'
#   }else if(biggest == r_ratio){
#     cat_groups$Colour[i] = 'r'
#   }else if(biggest == i_ratio){
#     cat_groups$Colour[i] ='i'
#   }
# }
# write.csv(cat_groups, file, row.names=FALSE)