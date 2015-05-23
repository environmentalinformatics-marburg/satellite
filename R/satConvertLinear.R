if ( !isGeneric("satConvertLinear") ) {
  setGeneric("satConvertLinear", function(x, ...)
    standardGeneric("satConvertLinear"))
}
#' Convert a band's scaled counts to radiance, reflectance and/or temperature
#'
#' @description
#' Convert a band's scalled counts to radiance, reflectance
#' and or brightness temperature, using a simple linear conversion without
#' any kind of atmospheric correction etc.
#' 
#' @param x An object of type Satellite or a raster::RasterStack
#' @param convert Type of physical output, one of "rad", "ref", "bt" or "all"
#' @param szen_cor Apply sun zenith correction, TRUE or FALSE
#'  
#' @export satConvertLinear
#' 
#' @name satConvertLinear
#'
#' @return Satellite object with added converted layers
#' 
#' @details 
#' The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}
#' 
#' @seealso \code{\link{calcAtmosCorr}} for converions of scaled counts 
#' to physical units including a scene-based atmospheric correction.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' sat <- satConvertLinear(sat)
setMethod("satConvertLinear", 
          signature(x = "Satellite"), 
          function(x, convert = "all", szen_correction = "TRUE"){
            if(convert == "all"){
              convert <- c("Rad", "Ref", "BT")
            }
            
            if("Rad" %in% convert){
              band_codes <- getSatBCDECalib(x, id = "SC")
              for(bcde in band_codes){
                if(!is.na(getSatRADM(x, bcde))){
                  sensor_rad <- convertLinear(band = getSatDataLayer(x, bcde),
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
            }
            
            if("Ref" %in% convert){
              band_codes <- getSatBCDESolarCalib(x, id = "SC")
              for(bcde in band_codes){
                if(!is.na(getSatREFM(x, bcde))){
                  if(szen_correction == TRUE){
                    szen <- getSatSZEN(x, bcde)
                    sensor_ref <- convertLinear(band = getSatDataLayer(x, bcde),
                                              mult = getSatREFM(x, bcde),
                                              add = getSatREFA(x, bcde),
                                              szen = szen)
                    calib = "REF"
                  } else {
                    sensor_ref <- convertLinear(band = getSatDataLayer(x, bcde),
                                              mult = getSatREFM(x, bcde),
                                              add = getSatREFA(x, bcde))
                    calib = "REF_NoSZEN"
                  }
                  layer_bcde <- paste0(bcde, "_", calib)
                  
                  meta_param <- getSatMetaBCDETemplate(x, bcde)
                  meta_param$BCDE <- layer_bcde
                  meta_param$CALIB <- calib
                  
                  info <- sys.calls()[[1]]
                  info <- paste0("Add layer from ", info[1], "(", 
                                 toString(info[2:length(info)]), ")")
                  
                  x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_ref,
                                       meta_param = meta_param,
                                       info = info, in_bcde = bcde)
                }
              }
            }
            
            if("BT" %in% convert){
              band_codes <- getSatBCDEThermalCalib(x, id = "SC")
              for(bcde in band_codes){
                if(!any(is.na(getSatRADM(x, bcde)), is.na(getSatBTK1(x, bcde)))){
                  sensor_ref <- convertLinear(band = getSatDataLayer(x, bcde),
                                            mult = getSatRADM(x, bcde),
                                            add = getSatRADA(x, bcde),
                                            k1 = getSatBTK1(x, bcde),
                                            k2 = getSatBTK2(x, bcde))
                  layer_bcde <- paste0(bcde, "_BT")
                  
                  meta_param <- getSatMetaBCDETemplate(x, bcde)
                  meta_param$BCDE <- layer_bcde
                  meta_param$CALIB <- "BT"
                  
                  info <- sys.calls()[[1]]
                  info <- paste0("Add layer from ", info[1], "(", 
                                 toString(info[2:length(info)]), ")")
                  
                  x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_ref,
                                       meta_param = meta_param,
                                       info = info, in_bcde = bcde)
                }
              }
            }
            return(x)
          })


