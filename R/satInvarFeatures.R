if ( !isGeneric("satInvarFeatures") ) {
  setGeneric("satInvarFeatures", function(x, ...)
    standardGeneric("satInvarFeatures"))
}
#' Identify pseudi-invariant features for a Satellite object
#'
#' @description
#' Identify pseudi-invariant features from a satellite scene based on a 
#' vis, near-infravis and short-wave infravis band.
#' 
#' @param x object of type Satellite
#' 
#' @return Satellite object with added mask
#' 
#' @export satInvarFeatures
#' 
#' @name satInvarFeatures
#'
#' @details 
#' The invariant features are computed using \code{\link{maskInvarFeatures}}. 
#' 
#' Please refer to the respective function for details on the computation.
#' 
#' @references Please refer to the respective function for references.
#'  
#' @seealso This function is a wrapper for \code{\link{maskInvarFeatures}}.
#' 
#' @examples
#' satPathRadDOSModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' satInvarFeatures(sat)
# 
setMethod("satInvarFeatures", 
          signature(x = "Satellite"), 
          function(x){
            bcde_vis <- "B004n"
            bcde_nir <- "B005n"
            bcde_swir <- "B007n"
            mask <- maskInvarFeatures(vis = getSatDataLayer(x, bcde_vis), 
                                      nir = getSatDataLayer(x, bcde_nir), 
                                      swir = getSatDataLayer(x, bcde_swir))
            layer_bcde <- "M0000_InvarFeatures"
            
            meta_param <- getSatSensorInfo(x)
            meta_param$BCDE <- layer_bcde
            meta_param$CALIB <- "BINARY"
            
            info <- sys.calls()[[1]]
            info <- paste0("Add layer from ", info[1], "(", 
                           toString(info[2:length(info)]), ")")
            
            x <- addSatDataLayer(x, bcde = layer_bcde, data = mask,
                                 meta_param = meta_param,
                                 info = info, in_bcde = paste(bcde_vis, 
                                                              bcde_nir, 
                                                              bcde_swir,
                                                              sep = ", "))
            return(x)
          })
