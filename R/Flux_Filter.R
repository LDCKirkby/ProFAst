#' Flux_filter
#' @description Filter sources in data frame based on axial ratio value.
#' @param RA_DEC Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param flux_value Numeric scalar; Flux filter cutoff value. Determined by dividing query filter flux by sum of fluxes in other bands , i.e. (g/r+i). Sources with an flux ratio lower than the value are assumed to not be asteroids and are removed.
#' @param edge_buffer Numeric scalar; Edge boundary value within which positive hits are ignored. Useful if images are artificially extended as is done in \code{\link[ProFAst]{Pre_Proc}}.
#' @param savepassthru Logical; should intermediate files be saved to directory? Can greatly increase size on disk but useful to see which objects are being filtered out.
#' @param ast_data List; R List containing data output from ProFAst::MultiDetect(). If not supplied will look for appropriate file in working directory.
#' @param colours Character vector; List containing detection bands for input fields. ProFAst will default to looking for g,r & i bands unless told otherwise.
#' 
#' @return Data frame containing all flux filtered sources.
#' @export
#'
Flux_Filter <- function(RA_DEC, flux_value=1, edge_buffer=0.001, savepassthru=FALSE, ast_data=NULL, colours=c("g","r","i")){
if(is.null(ast_data)){
  ast_data = utils::read.csv(paste0("./",RA_DEC,"/allcat.csv"))
}
cat("*********\n")
cat(length(ast_data$X), " Objects Detected\n")
cat("*********\n\n")

#Remove whole rows of NA's
ast_data = ast_data[rowSums(is.na(ast_data)) != ncol(ast_data),]

#Remove potential error fluxes that can be small negative numbers
for(band in colours){
  ast_data[paste0("flux_",band,"t")][ast_data[paste0("flux_",band,"t")] < 0] <- 0
}

#Extracting potential asteroids, based on their flux ratio
cat("*********\n")
cat("Beginning asteroid search\n")
for(x in 1:length(colours)){
  main_colour = colours[x]
  other_colours = colours[-x]
  numer=ast_data[paste0("flux_",main_colour,"t")]
  denom=numeric(length(numer))
  for(y in 1:length(other_colours)){
    numer_part=other_colours[y]
    denom = denom + ast_data[paste0("flux_",numer_part,"t")]
  }
  assign(paste0(main_colour,"_objects"), cbind("Colour" = main_colour, subset(ast_data, subset = numer/denom >= flux_value)))
}

#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(RA_DEC, "_")[[1]][[1]])
Dec = as.numeric(strsplit(RA_DEC, "_")[[1]][[2]])

#Useful to apply edge buffer since some frames are being artificially grown
cat("Applying edge buffer\n")
for(band in colours){
  band_list = get(paste0(band,"_objects"))
  band_list = rbind(band_list[band_list$RAcen >= (RA - 0.5 + edge_buffer) & band_list$RAcen <= (RA + 0.5 - edge_buffer) & band_list$Deccen >= (Dec-0.5+edge_buffer) & band_list$Deccen <= (Dec+0.5-edge_buffer),])
  assign(paste0(band,"_objects"), band_list)
  assign(paste0(band,"_objects"), rbind(get(paste0(band,"_objects"))))
}

#Bind final lists of objects together
possible_asteroids = c()
for(band in colours){
  possible_asteroids <- rbind(possible_asteroids, get(paste0(band,"_objects")))
}
cat(length(possible_asteroids$groupID), " potential asteroids in data\n")
cat("Writing to ", paste0("./", RA_DEC,"/_Flux_Filtered_Objects.csv"),"\n")
cat("*********\n\n")

if(savepassthru==TRUE){
#Write data to file
utils::write.csv(possible_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_Flux_Filtered_Objects.csv"), row.names=FALSE)
}

gc()
return(possible_asteroids)
}