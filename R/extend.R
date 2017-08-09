if ( !isGeneric("extend") ) {
  setGeneric("extend", function(x, ...)
    standardGeneric("extend"))
}

#' Extend a Satellite object
#'
#' @description
#' The function is a wrapper around \code{\link[raster]{extend}} to easily 
#' extend a Satellite object to a larger spatial extent.
#'
#' @param x Satellite object.
#' @param y Target \code{Extent}, see \code{\link[raster]{extent}}. 
#' @param subset Logical. If \code{TRUE} (default), all layers but the extended 
#' ones are being dropped, else the extended layers are appended to the initial
#' Satellite object.
#' @param value Fill value assigned to new cells passed to 
#' \code{\link[raster]{extend}}, defaults to \code{NA}.
#'
#' @return A Satellite object consisting of extended layers only or, if 
#' \code{subset = FALSE}, a Satellite object with the extended layers appended.
#' 
#' @export extend
#' 
#' @name extend
#' @aliases extend,Satellite-method
#'
#' @seealso This function is a wrapper around \code{\link[raster]{extend}}.
#'
#' @examples
#' \dontrun{
#' ## sample data
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#'
#' ## geographic extent of georg-gassmann-stadium (utm 32-n)
#' ext_ggs <- raster::extent(482606.4, 482781.4, 5627239, 5627489)
#' 
#' ## extend satellite object by specified extent
#' sat_ggs <- extend(sat, ext_ggs)
#' 
#' plot(sat)
#' plot(sat_ggs)
#' }
setMethod("extend", 
          signature(x = "Satellite"), 
          function(x, y, subset = TRUE, value = NA) {
            rad_bands <- getSatBCDE(x)
            for (bcde_rad in rad_bands) {

              ref <- raster::extend(getSatDataLayer(x, bcde_rad), y, value)
              # keep all metadata except for file path since extended 
              # layers are in memory and set calib column flag.
              meta_param <- getSatMeta(x, bcde_rad)
              meta_param$CALIB <- "extended"
              meta_param$FILE <- NULL
              
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = bcde_rad, data = ref,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde_rad)
            }
            
            if(subset == TRUE){
              x <- subset(x, cid = "extended")
              #reset LNBR (dirty hack)
              x@meta$LNBR <- rep(1:nrow(x@meta))
              x@meta$CALIB <- "SC"
            }
            
            return(x)
          }
)
