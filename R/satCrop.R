if ( !isGeneric("cropSat") ) {
  setGeneric("cropSat", function(x, ...)
    standardGeneric("cropSat"))
}

#' Crop satellite object.
#'
#' @description
#' The function is a wrapper around the raster::crop function to easily crop
#' a satellite object by an extent object.
#'
#' @param x object of type Satellite
#' @param y Extent object
#' @param subset logical, defaults to TRUE. Drops all layers but the cropped ones.
#' If set to false appends cropped layers to Satellite object.
#'
#' @return Satellite object cropped layers
#' 
#' @export cropSat
#' 
#' @name cropSat
#'
#' @details Crop layers of a Satellite object to the size of a given Extent object.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for \code{\link[raster]{crop}}.
#'
#' @examples
# #' EXAMPLE STILL NEEDS DATA (i.e. the crop template)
#'
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' crop_template <- raster(paste0(path,crop_template))
#' 
#' sat <- crop(sat, crop_template)
#' 
setMethod("cropSat", 
          signature(x = "Satellite"), 
          function(x, y, subset = TRUE){
            rad_bands <- getSatBCDE(x)
            for(bcde_rad in rad_bands){
              ref <-crop(getSatDataLayer(x, bcde_rad), y)
              #keep all metadata except for file path since cropped layers are in memory and set
              #calib column flag.
              meta_param <- getSatMeta(x,bcde_rad)
              meta_param$CALIB <- "cropped"
              meta_param$FILE <- NULL

              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = bcde_rad, data = ref,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde_rad)
            }
            if(subset == TRUE){
              x <- subsetSat(x,"cropped")
              #reset LNBR (dirty hack)
              x@meta$LNBR <- rep(1:nrow(x@meta))
            }
            return(x)
          })
