library(rjson)
library(data.table)

find_orb_params <- function(survey_ID, dt){
    params <- rjson::fromJSON(file = paste0("./Orbital_Params/",survey_ID,"/elem_",survey_ID,".json"))
    elements =  params$objects[[1]]$elements
    
    new_row = data.frame(
        survey_ID = survey_ID,
        JPL_D = NULL,
        a = elements$a,
        P = elements$P,
        M = elements$M,
        n = elements$n,
        e = elements$e,
        q = elements$q,
        Q = elements$Q,
        i = elements$i,
        a_sigma = elements$`a sigma`,
        P_sigma = elements$`P sigma`,
        M_sigma = elements$`M sigma`,
        n_sigma = elements$`n sigma`,
        e_sigma = elements$`e sigma`,
        q_sigma = elements$`q sigma`,
        Q_sigma = elements$`Q sigma`,
        i_sigma = elements$`i sigma`
    )

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

df <- data.frame()
df <- find_orb_params(survey, df)
df <- JPL_params(JPL, df) 
