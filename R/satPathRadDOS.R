if ( !isGeneric("satPathRadDOS") ) {
  setGeneric("satPathRadDOS", function(x, ...)
    standardGeneric("satPathRadDOS"))
}
#' Compute path radiance based on dark object method for a Satellite object
#'
#' @description
#' Compute an estimaed path radiance for all sensor band_wls using a dark object 
#' method which can be used to roughly correct the radiance values for 
#' atmospheric scattering.
#' 
#' @param x object of type Satellite
#' @param method name of the method to be used ("Table", "Model", "RadRef)
#' @param model name of the model to be used if method is "Model"
#' (see \code{\link{calcTOAIrradModel}})
#' @param normalize normalize ESun to mean earth sun distance
#' @param esd earth-sun distance (AU), if not supplied and necessary for
#' normalize, it is tried to take it from the metadata, otherwise it is estimated
#' by the day of the year using \code{\link{calcEartSunDist}}.
#' 
#' @return Satellite object with path radiance for each band in the metadata
#' 
#' @export satPathRadDOS
#' 
#' @name satPathRadDOS
#'
#' @details 
#' The path radiance is computed using \code{\link{calcPathRadDOS}}. 
#' 
#' If the TOA solar irradiance is not part of the metadata of the Satellite
#' object, it is computed using \code{\link{satTOAIrrad}}.
#' 
#' Please refer to the respective functions for details on the computation.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for \code{\link{calcPathRadDOS}}.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satPathRadDOSTable(sat)
#' 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' satPathRadDOSModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' satPathRadDOSRadRef(sat)
# 
setMethod("satPathRadDOS", 
          signature(x = "Satellite"), 
          function(x, atmos_model = "DOS2", esun_mode = "RadRef"){
            
            # Take care of TOA solar irradiance information
            if(any(is.na(getSatESUN(x, getSatBCDESolar(x))))){
              # Compute toa irradiance for all solar band layers
              if(esun_mode == "Table"){
                x <- satTOAIrrad(x, method = "Table", normalize = TRUE)
              } else if(esun_mode == "Model"){
                x <- satTOAIrrad(x, method = "Model", model = "MNewKur", 
                                 normalize = TRUE)
              } else if(esun_mode == "RadRef"){
                x <- satTOAIrrad(x, method = "RadRef", normalize = "TRUE")
              }
            }
            
            
            # Get solar bands with calibration information equals scaled counts
            sc_bands <- getSatBCDESolarCalib(x, id = "SC")
            
            # Take care of dark object values
            bcde <- "B002n"
            dn_min <- min(raster::getValues(getSatDataLayer(x, bcde)))
            
            # Take care of path radiance
            path_rad <- calcPathRadDOS(DNmin = dn_min,
                                       bnbr = getSatLNBR(x, bcde),
                                       band_wls = data.frame(LMIN = getSatLMIN(x, sc_bands), 
                                                             LMAX = getSatLMAX(x, sc_bands)),
                                       radm = getSatRADM(x, sc_bands),
                                       rada = getSatRADA(x, sc_bands),
                                       szen = getSatSZEN(x, sc_bands),
                                       esun = getSatESUN(x, sc_bands),
                                       model = "DOS2")
            x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(path_rad),
                                                            PRAD = as.numeric(path_rad)))
            return(x)
          })
