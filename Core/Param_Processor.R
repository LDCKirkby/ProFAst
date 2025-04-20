library(rjson)
library(data.table)
library(ggplot2)

find_orb_params <- function(survey_ID, dt){
    params <- rjson::fromJSON(file = paste0("./Orbital_Params/",survey_ID,"/elem_",survey_ID,".json"))
    elements =  params$objects[[1]]$elements
    
    new_row = as.data.frame(t(rep(NA,19)))
    names = c("survey_ID","JPL_D","a","P","M","n","e","peri","Q","i",
    "a_sigma","P_sigma","M_sigma","n_sigma","e_sigma","peri_sigma","Q_sigma","i_sigma","r_calc")
    colnames(new_row) = names
    new_row$survey_ID = survey_ID
    new_row$JPL_D = NA

    elem_names = c("survey_ID","JPL_D","a","P","M","n","e","q","Q","i",
    "a sigma","P sigma","M sigma","n sigma","e sigma","q sigma","Q sigma","i sigma")

    for(i in 3:18){
        if(is.null(elements[[elem_names[i]]])){
            next
        }else{
            new_row[names[i]] = elements[[elem_names[i]]]
        }
    }
    new_row$r_calc = sqrt(new_row$peri * new_row$Q)
    dt <- rbind(dt, new_row)
    return(dt)
}

JPL_params <- function(JPL_ID, dt){
    param <- rjson::fromJSON(file = paste0("./AST_JSONS/",JPL_ID,".json"))
    elem = param$orbit$elements
    a = NULL
    P = NULL
    M = NULL
    n = NULL
    e = NULL
    q = NULL
    Q = NULL
    i = NULL
    a_sigma = NULL
    P_sigma = NULL
    M_sigma = NULL
    n_sigma = NULL
    e_sigma = NULL
    q_sigma = NULL
    Q_sigma = NULL
    i_sigma = NULL
    for(item in elements){
        switch(item$name,
               "a" = {a <- item$value; a_sigma <- item$sigma},
               "e" = {e <- item$value; e_sigma <- item$sigma},
               "i" = {i <- item$value; i_sigma <- item$sigma},
               "q" = {q <- item$value; q_sigma <- item$sigma},
               "om"= {n <- item$value; n_sigma <- item$sigma},
               "M" = {M <- item$value; M_sigma <- item$sigma},
               "ad"= {Q <- item$value; Q_sigma <- item$sigma},
               "per"={P <- item$value, P_sigma <- item$sigma},
        )
    }

    JPL_row = data.frame(
        survey_ID = NULL,
        JPL_ID = JPL_ID,
        a = a,
        P = P,
        M = M,
        n = n,
        e = e,
        q = q, 
        Q = Q, 
        i = i, 
        a_sigma = a_sigma,
        P_sigma = P_sigma,
        M_sigma = M_sigma,
        n_sigma = n_sigma,
        e_sigma = e_sigma,
        q_sigma = q_sigma,
        Q_sigma = Q_sigma,
        i_sigma = i_sigma
    )
    dt <- rbind(dt, JPL_row)
    return(dt)
}

args = commandArgs(trailingOnly = TRUE)
loc = as.character(args[[1]])
survey = as.character(args[[2]])
JPL = as.character(args[[3]])

asteroid_IDS <- list.dirs(path="./Orbital_Params/",recursive=FALSE,full.names=FALSE)
asteroid_params <- data.frame()
for(asteroid in asteroid_IDS){
    asteroid_params <- find_orb_params(asteroid, asteroid_params)
}

graph_values = c("a","P","M","n","e","peri","Q","i",
    "a_sigma","P_sigma","M_sigma","n_sigma","e_sigma","peri_sigma","Q_sigma","i_sigma","r_calc")
colours = rainbow(18)
i=1
for(value in graph_values){
    cat(value,"\n")
    plot <- ggplot(data=asteroid_params, aes(x=.data[[paste0(value)]])) + geom_histogram(bins = 200,fill = colours[i]) + xlab(paste0(value)) +
    ggtitle(paste0("Calculated ",value," Distributions")) + scale_x_continuous(limits = quantile(asteroid_params[[paste0(value)]], c(0.0005,0.9995), na.rm=TRUE))# + scale_y_continuous(trans='log10')
    ggsave(filename = paste0("./",value,"_plot.png"), plot)
    i = i+1
}               
#df <- JPL_params(JPL, df) 
subset()