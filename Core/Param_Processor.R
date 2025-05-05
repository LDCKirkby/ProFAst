library(rjson)
library(data.table)
library(ggplot2)
library(stringr)
library(fmsb)
library(tidyverse)
library(viridis)
library(ggridges)

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
    target_ast = asteroid_params[asteroid_params$survey_ID==ID  ,]
    target_JPL = JPL_param[JPL_param$survey_ID == ID,]
    data = rbind(max_min, target_ast, target_JPL)
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

# names = c("survey_ID","JPL_ID","a","e","i","w", "N","M","P",
#               "a","e","i","w", "N","M","P")
graph_values = c("a","e","i","w","N","M","P")
JPL_values = c("a","e","i","w","N","M","P")       
colours = rainbow(18)
i=1
for(value in graph_values){
    cat(value,"\n")
    name = title_names[i]
    plot <- ggplot(data=asteroid_params, aes(x=.data[[paste0(value)]])) + geom_histogram(bins = 50,fill = colours[i], alpha = 0.9) + xlab(name) + 
            ggtitle(paste0("Calculated Asteroid Distribution: ",name)) + scale_x_continuous(limits = quantile(asteroid_params[[paste0(value)]], c(0.0005,0.9995), na.rm=TRUE)) + scale_y_continuous(expand=c(0,1))
    ggsave(filename = paste0("./Elements/",value,"_plot.png"), plot)
    plot <- ggplot() + 
            geom_histogram(data=asteroid_params, aes(x=.data[[paste0(value)]], fill="Calculated"),bins = 50, alpha = 0.7) + 
            geom_histogram(data=JPL_param, aes(x=.data[[paste0(value)]], fill="JPL"),bins = 50, alpha = 0.7) + xlab(name) + 
            ggtitle(paste0("Calculated Asteroid Distribution: ",name)) + scale_x_continuous(limits = quantile(asteroid_params[[paste0(value)]], c(0.0005,0.9995), na.rm=TRUE)) + scale_y_continuous(expand=c(0,1)) +
            scale_fill_manual(name="Source", breaks=c("Calculated","JPL"), values=c("Calculated"="seagreen3","JPL"="salmon"))
    ggsave(filename = paste0("./Elements/",value,"_JPL_v_Calculated_plot.png"), plot)
    i = i+1
}       
kirkwood = data.frame(xintercepts = c(1.780, 2.065, 2.502, 2.825, 2.958, 3.279, 3.972, 4.296), names=c("1.780 AU", "2.065 AU", "2.502 AU", "2.825 AU", "2.958 AU", "3.279 AU", "3.972 AU", "4.296 AU"))
plot <- ggplot(data=asteroid_params, aes(x=a)) + geom_histogram(bins = 50,fill = "seagreen3", alpha = 0.9) + xlab("Semi-Major Axis (AU)") +
            ggtitle(paste0("Calculated Asteroid Distribution: Semi-Major Axis")) + scale_x_continuous(limits = quantile(asteroid_params[["a"]], c(0.0005,0.9995), na.rm=TRUE)) + scale_y_continuous(expand=c(0,1))
plot <- plot + geom_vline(data=kirkwood, aes(xintercept=xintercepts, color=names), alpha =0.85) + 
            scale_color_viridis_d(name = "Kirkwood Gaps", option="plasma")+ 
            theme(legend.title=element_text(size=10),legend.text=element_text(size=8))
ggsave(filename = paste0("./Elements/a_plot.png"), plot)
plot <- ggplot() + 
            geom_histogram(data=asteroid_params, aes(x=a, fill="Calculated"),bins = 50, alpha = 0.7) + 
            geom_histogram(data=JPL_param, aes(x=a, fill="JPL"),bins = 50, alpha = 0.7) + xlab("Semi-Major Axis (AU)") + 
            ggtitle(paste0("Calculated Asteroid Distribution: Semi-Major Axis")) + scale_x_continuous(limits = quantile(asteroid_params$a, c(0.0005,0.9995), na.rm=TRUE)) + scale_y_continuous(expand=c(0,1)) +
            scale_fill_manual(name="Source", breaks=c("Calculated","JPL"), values=c("Calculated"="seagreen3","JPL"="salmon"))
plot <- plot + geom_vline(data=kirkwood, aes(xintercept=xintercepts, color=names), alpha =0.85) + 
            scale_color_viridis_d(name = "Kirkwood Gaps", option="plasma")+ 
            theme(legend.title=element_text(size=10),legend.text=element_text(size=8))
ggsave(filename = paste0("./Elements/a_JPL_v_Calculated_plot.png"), plot)



