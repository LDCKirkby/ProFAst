post_text_to_ntfy <- function(txt){
  NTFY_TOKEN = "tk_wghoq838fu17o8r53kcn48wuaidme"
  NTFY_ENDPOINT = "https://ntfy.sigmari.ch/lk-RAM"
  httr::POST(url = NTFY_ENDPOINT,
             body = txt,
             httr::add_headers('Authorization' = paste0('Bearer ',NTFY_TOKEN)))
}

get_object_sizes <- function(){
  objs = sapply(ls(envir=.GlobalEnv),
                function(x){
                  format(object.size(get(x)),units="auto")
                }
  )
  objs_formatted = paste(paste(names(objs),objs,sep=": "),collapse = '\n')
  return(objs_formatted)
}

post_sze_to_ntfy <- function(){
  obsize = get_object_sizes()
  post_text_to_ntfy(obsize)
}