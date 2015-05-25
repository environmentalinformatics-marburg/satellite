if (!isGeneric("convSC2Ref") ) {
  setGeneric("convSC2Ref", function(x, ...)
    standardGeneric("convSC2Ref"))
}
#' Convert a band's scaled counts or radiance values to reflectance
#'
#' @description
#' Convert a band's scalled counts to reflectance using a simple linear 
#' conversion without any kind of atmospheric correction etc.
#' 
#' @param x An object of type Satellite,  raster::RasterStack or 
#' raster::RasterLayer providing scaled counts (DNs)
#' @param add additive coefficient for value transformation (i.e. offset)
#' @param mult multiplicative coefficient for value transformation (i.e. slope)
#' @param szen cosine of solar zenith angle
#' @param szen_correction Apply sun zenith correction, TRUE or FALSE
#' 
#'   
#' @export convSC2Ref
#' 
#' @name convSC2Ref
#'
#' @details 
#' The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}
#' 
#' If the sensor does not provide linear conversion coefficients for reflectance
#' computation, the reflectance is calculated using the solar irradiance 
#' following the functions taken from USGS' Landsat 7 manual, chapter 11.3.2,
#' which is available online at 
#' \url{http://landsathandbook.gsfc.nasa.gov/data_prod/prog_sect11_3.html}
#' 
#' @seealso \code{\link{calcAtmosCorr}} for converions of scaled counts 
#' to physical units including a scene-based atmospheric correction.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' sat <- convSC2Ref(sat)
#' 
#' # If you use a raster layer, supply required meta information
#' bcde <- "B002n"
#' convSC2Ref(band = getSatDataLayer(sat, bcde),
#'             mult = getSatRADM(sat, bcde),
#'             add = getSatRADA(sat, bcde))
#' 
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with added converted layers
#' 
#' @rdname convSC2Ref
#'
setMethod("convSC2Ref", 
          signature(x = "Satellite"), 
          function(x, szen_correction = "TRUE"){
            band_codes <- getSatBCDESolarCalib(x, id = "SC")
            for(bcde in band_codes){
              if(!is.na(getSatREFM(x, bcde))){
                if(szen_correction == TRUE){
                  szen <- getSatSZEN(x, bcde)
                  sensor_ref <- convSC2Ref(x = getSatDataLayer(x, bcde),
                                           mult = getSatREFM(x, bcde),
                                           add = getSatREFA(x, bcde),
                                           szen = szen)
                  calib = "REF"
                } else {
                  sensor_ref <- convSC2Ref(x = getSatDataLayer(x, bcde),
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
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @return raster::RasterStack object with converted layers
#' 
#' @rdname convSC2Ref
#'
setMethod("convSC2Ref", 
          signature(x = "RasterStack"), 
          function(x, mult, add, szen){
            for(l in seq(nlayers(x))){
              x[[l]] <- convSC2Ref(x[[l]], mult, add, szen)
            }
            return(x)
          })


# Function using raster::RasterLayer object ------------------------------------
#' 
#' @return raster::RasterLayer object with converted layer
#' 
#' @rdname convSC2Ref
#'
setMethod("convSC2Ref", 
          signature(x = "RasterLayer"), 
          function(x, mult, add, szen){
            x <- mult * x + add
            if(!missing(szen)){
              x <- x / cos(szen * pi / 180.0)
            }
            return(x)
          })