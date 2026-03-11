#' Flux_filter
#' @description Filter sources in data frame based on axial ratio value.
#' @param RA_DEC Celestial Right Ascension and Declination of Input Frame separated by underscore (RA_Dec).
#' @param flux_value Numeric scalar; Flux filter cutoff value. Determined by dividing query filter flux by sum of fluxes in other bands , i.e. (g/r+i). Sources with an flux ratio lower than the value are assumed to not be asteroids and are removed.
#' @param edge_buffer Numeric scalar; Edge boundary value within which positive hits are ignored. Useful if images are artificially extended as is done in \code{\link{PreProc}}.
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
ast_data$flux_gt[ast_data$flux_gt < 0] <- 0
ast_data$flux_rxt[ast_data$flux_rxt < 0] <- 0
ast_data$flux_i1xt[ast_data$flux_i1xt < 0] <- 0

#Extracting potential asteroids, based on their flux ratio
cat("*********\n")
cat("Beginning asteroid search\n")
green_objects = cbind("Colour" = "g", subset(ast_data, subset = ast_data$flux_gt/(ast_data$flux_rxt + ast_data$flux_i1xt) >= flux_value))
red_objects = cbind("Colour" = "r", subset(ast_data, subset = ast_data$flux_rxt/(ast_data$flux_gt + ast_data$flux_i1xt) >= flux_value))
blue_objects = cbind("Colour" = "i", subset(ast_data, subset = ast_data$flux_i1xt/(ast_data$flux_gt + ast_data$flux_rxt) >= flux_value))

#Applies edge buffer to red and blue, since they've been extended artificially
RA = as.numeric(strsplit(RA_DEC, "_")[[1]][[1]])
Dec = as.numeric(strsplit(RA_DEC, "_")[[1]][[2]])

#Useful to apply edge buffer since some frames are being artificially grown
cat("Applying edge buffer\n")
red_objects = rbind(red_objects[red_objects$RAcen >= (RA - 0.5 + edge_buffer) & red_objects$RAcen <= (RA + 0.5 - edge_buffer) & red_objects$Deccen >= (Dec-0.5 + edge_buffer) & red_objects$Deccen <= (Dec + 0.5 - edge_buffer),])
blue_objects = rbind(blue_objects[blue_objects$RAcen >= (RA - 0.5 + edge_buffer) & blue_objects$RAcen <= (RA + 0.5 - edge_buffer) & blue_objects$Deccen >= (Dec-0.5 + edge_buffer) & blue_objects$Deccen <= (Dec + 0.5 - edge_buffer),])
green_objects = rbind(green_objects[green_objects$RAcen >= (RA - 0.5 + edge_buffer) & green_objects$RAcen <= (RA + 0.5 - edge_buffer) & green_objects$Deccen >= (Dec-0.5 + edge_buffer) & green_objects$Deccen <= (Dec + 0.5 - edge_buffer),])

#Bind final lists of objects together
possible_asteroids <- rbind(blue_objects,green_objects,red_objects)
cat(length(possible_asteroids$groupID), " potential asteroids in data\n")
cat("Writing to ", paste0("./", RA_DEC,"/_Flux_Filtered_Objects.csv"),"\n")
cat("*********\n\n")

if(savepassthru==TRUE){
#Write data to file
utils::write.csv(possible_asteroids, file = paste0("./",RA_DEC,"/",RA_DEC,"_Flux_Filtered_Objects.csv"), row.names=FALSE)
}

return(possible_asteroids)
}