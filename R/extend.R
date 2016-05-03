if ( !isGeneric("extend") ) {
  setGeneric("extend", function(x, ...)
    standardGeneric("extend"))
}

#' extend satellite object
#'
#' @description
#' The function is a wrapper around the \code{\link{extend}} function to 
#' easily extend a Satellite object by an \code{\link{extent}} object.
#'
#' @param x Satellite object.
#' @param y \code{\link{extent}} object. 
#' @param subset Logical; if \code{TRUE} (default), all layers but the extendped 
#' ones are being dropped; if \code{FALSE}, extendped layers are appended to the 
#' Satellite object.
#'
#' @return A Satellite object consisting of extended layers only. If 
#' \code{subset = FALSE}, a Satellite object with the extendped layers appended.
#' 
#' @export extend
#' 
#' @name extend
#' @aliases extend,Satellite-method
#'
#' @details extends layers of a Satellite object to the size of a given 
#' \code{raster::extent} object.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for \code{raster::extend}.
#'
#' @examples
#' \dontrun{
#' ## sample data
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#'
#' ## geographic extent of georg-gassmann-stadium (utm 32-n)
#' ext_ggs <- raster::extent(484015, 484143, 5627835, 5628020)
#' 
#' ## extend satellite object by specified extent
#' sat_ggs <- extend(sat, ext_ggs)
#' 
#' plot(sat)
#' plot(sat_ggs)
#' }
setMethod("extend", 
          signature(x = "Satellite"), 
          function(x, y, subset = TRUE) {
            rad_bands <- getSatBCDE(x)
            for (bcde_rad in rad_bands) {
              ref <- extend(getSatDataLayer(x, bcde_rad), y)
              # keep all metadata except for file path since extendped 
              # layers are in memory and set calib column flag.
              meta_param <- getSatMeta(x, bcde_rad)
              meta_param$CALIB <- "extendped"
              meta_param$FILE <- NULL
              
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = bcde_rad, data = ref,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde_rad)
            }
            
            if(subset == TRUE){
              x <- subset(x, cid = "extendped")
              #reset LNBR (dirty hack)
              x@meta$LNBR <- rep(1:nrow(x@meta))
              x@meta$CALIB <- "SC"
            }
            
            return(x)
          }
)
