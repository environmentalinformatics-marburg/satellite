if ( !isGeneric("calibLinear") ) {
  setGeneric("calibLinear", function(x, ...)
    standardGeneric("calibLinear"))
}
#' Convert DNs to radiance, reflectance and/or brightness temperature.
#'
#' @description
#' The function converts the digital numbers of a sensor band to radiance (rad), 
#' relfectance (ref) and/or brightness temperature (bt) using the calibration 
#' coefficients from the metadata file. The reflectance  conversion can 
#' additionaly be include a sun zenith correction.
#'
#' @param x Satellite, Raster*, or numeric object of the sensor's band values
#' @param bnbr number of the band (required if rasterstack)
#' @param coefs coefficients data frame resulting from compMetaLandsat()
#' @param conv conversion type (one of "rad", "ref", "refsun", "bt")
#'
#' @return Satellite, Raster*, or numeric object object with converted values
#'
#' @name calibLinear
#' 
#' @references The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}

#' @seealso \code{\link{radiometricCorrection}} for converions of the DNs 
#' including scene-based atmospheric correction.
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' bcde <- "002n"
#' 
#' calibLinear(band = getSatDataLayer(sat, bcde), bnbr = 1,
#'             mult = getSatRADM(sat, bcde),
#'             add = getSatRADA(sat, bcde))
#'
NULL

# Function for Satellite objects -----------------------------------------------
#' @rdname calibLinear
#' @export calibLinear
setMethod("calibLinear", 
          signature(x = "Satellite"), 
          function(x, convert = "all", szen_correction = "TRUE"){
            if(convert == "all"){
              convert <- c("Rad", "Ref", "BT")
            }
            
            if("Rad" %in% convert){
              band_codes <- getSatBCDECalib(x, id = "SC")
              for(bcde in band_codes){
                if(!is.na(getSatRADM(x, bcde))){
                  sensor_rad <- calibLinear(x = getSatDataLayer(x, bcde),
                                            bnbr = 1,
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
                    sensor_ref <- calibLinear(x = getSatDataLayer(x, bcde),
                                              bnbr = 1,
                                              mult = getSatREFM(x, bcde),
                                              add = getSatREFA(x, bcde),
                                              szen = szen)
                    calib = "REF"
                  } else {
                    sensor_ref <- calibLinear(x = getSatDataLayer(x, bcde),
                                              bnbr = 1,
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
                  sensor_ref <- calibLinear(x = getSatDataLayer(x, bcde),
                                            bnbr = 1,
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


# Function for Raster objects --------------------------------------------------
#' @rdname calibLinear
#' @export calibLinear
setMethod("calibLinear", 
          signature(x = "Raster"), 
          function(x, bnbr, mult, add, szen, k1, k2){
            .calibLinear(x = x, bnbr = bnbr, mult = mult, add = add, 
                         szen = szen, k1 = k1, k2 = k2)
          })


#' @rdname calibLinear
#' @export calibLinear
setMethod("calibLinear", 
          signature(x = "numeric"), 
          function(x, bnbr, mult, add, szen, k1, k2){
            .calibLinear(x = x, bnbr = bnbr, mult = mult, add = add, 
                         szen = szen, k1 = k1, k2 = k2)
          })


# Base function for calibratin sensor data -------------------------------------
.calibLinear <-  function(x, bnbr = 1, mult, add, szen, k1, k2){
  result <- mult[bnbr] * x + add[bnbr]
  if(!missing(szen)){
    result <- result / cos(szen[bnbr] * pi / 180.0)
  }
  if(!missing(k1)){
    result <- k2[bnbr] / log(k1[bnbr] / result + 1)
  }
  return(result)
}