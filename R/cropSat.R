if ( !isGeneric("cropSat") ) {
  setGeneric("cropSat", function(x, ...)
    standardGeneric("cropSat"))
}

#' Crop satellite object
#'
#' @description
#' The function is a wrapper around the \code{\link{raster::crop}} function to 
#' easily crop a Satellite object by an \code{raster::extent} object.
#'
#' @param x Satellite object.
#' @param y \code{raster::Extent} object. 
#' @param subset Logical; if \code{TRUE} (default), all layers but the cropped 
#' ones are being dropped; if \code{FALSE}, cropped layers are appended to the 
#' Satellite object.
#'
#' @return A Satellite object consisting of cropped layers only. If 
#' \code{subset = FALSE}, a Satellite object with the cropped layers appended.
#' 
#' @export cropSat
#' 
#' @name cropSat
#'
#' @details Crop layers of a Satellite object to the size of a given 
#' \code{raster::extent} object.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for \code{\link{raster::crop}}.
#'
#' @examples
#' ## sample data
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#'
#' ## geographic extent of georg-gassmann-stadium (utm 32-n)
#' ext_ggs <- raster::extent(484015, 484143, 5627835, 5628020)
#' 
#' ## crop satellite object by specified extent
#' sat <- cropSat(sat, ext_ggs)
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
              x <- satSubset(x,"cropped")
              #reset LNBR (dirty hack)
              x@meta$LNBR <- rep(1:nrow(x@meta))
              x@meta$CALIB <- "SC"
            }
            return(x)
          })
