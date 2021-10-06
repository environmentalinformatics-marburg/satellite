if (!isGeneric("convSC2Rad") ) {
  setGeneric("convSC2Rad", function(x, ...)
    standardGeneric("convSC2Rad"))
}
#' Convert a band's scaled counts to radiance
#'
#' @description
#' Convert a band's scaled counts to radiance using a simple linear conversion 
#' without any kind of atmospheric correction etc.
#' 
#' @param x An object of class Satellite, raster::RasterStack or 
#' raster::RasterLayer providing scaled counts (DNs).
#' @param add Additive coefficient for value transformation (i.e. offset).
#' @param mult Multiplicative coefficient for value transformation (i.e. slope).
#' @param szen Cosine of solar zenith angle.
#' @param szen_correction Logical; if \code{TRUE}, sun zenith correction is 
#' being applied.
#' @param subset Logical; if \code{TRUE}, all layers but the cropped 
#' ones are being dropped; if \code{FALSE} (default), cropped layers are appended to the 
#' Satellite object.
#'   
#' @export convSC2Rad
#' 
#' @name convSC2Rad
#'
#' @details 
  #' The conversion functions are taken from USGS' Landsat 8 Data Users Handbook
#' which is available online at 
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-8-data-users-handbook}.
#' 
#' @seealso \code{\link{calcAtmosCorr}} for conversions of scaled counts 
#' to physical units including a scene-based atmospheric correction.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
#' sat <- satellite(files)  
#' sat <- convSC2Rad(sat)
#' 
#' # If you use a raster layer, supply required meta information
#' bcde <- "B002n"
#' convSC2Rad(x = getSatDataLayer(sat, bcde),
#'            mult = getSatRADM(sat, bcde),
#'            add = getSatRADA(sat, bcde))
#' 
NULL


# Function using satellite object ----------------------------------------------
#' @return If x is a Satellite object, a Satellite object with added converted 
#' layers; \cr
#' if x is a \code{raster::Raster*} object, a \code{raster::Raster*} object with
#' converted layer(s).
#' @rdname convSC2Rad
setMethod("convSC2Rad", 
          signature(x = "Satellite"), 
          function(x, szen_correction = "TRUE", subset = FALSE){
            band_codes <- getSatBCDECalib(x, calib = "SC")
            for(bcde in band_codes){
              if(!is.na(getSatRADM(x, bcde))){
                sensor_rad <- convSC2Rad(x = getSatDataLayer(x, bcde),
                                         mult = getSatRADM(x, bcde),
                                         add = getSatRADA(x, bcde))
                layer_bcde <- paste0(bcde, "_RAD")
                
                meta_param <- getSatMetaBCDETemplate(x, bcde)
                meta_param$BCDE <- layer_bcde
                meta_param$CALIB <- "RAD"
                
                info <- sys.calls()[[1]]
                info <- paste0("Add layer from ", info[1], "(", 
                               toString(info[2:length(info)]), ")")
                
                x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_rad,
                                     meta_param = meta_param,
                                     info = info, in_bcde = bcde)
              }
            }
            
            if(subset == TRUE){
              x <- subset(x, cid = "RAD")
              #reset LNBR (dirty hack)
              x@meta$LNBR <- rep(1:nrow(x@meta))
            }
            
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' @rdname convSC2Rad
setMethod("convSC2Rad", 
          signature(x = "RasterStack"), 
          function(x, mult, add, szen){
            for(l in seq(nlayers(x))){
              x[[l]] <- convSC2Rad(x[[l]], mult, add, szen)
            }
            return(x)
          })


# Function using raster::RasterLayer object ------------------------------------
#' @rdname convSC2Rad
setMethod("convSC2Rad", 
          signature(x = "RasterLayer"), 
          function(x, mult, add, szen){
            if(!missing(szen)){
              x <- (mult * x + add) / cos(szen * pi / 180.0)
            } else {
              x <- mult * x + add
            }
            return(x)
          })