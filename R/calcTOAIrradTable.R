if ( !isGeneric("calcTOAIrradTable") ) {
  setGeneric("calcTOAIrradTable", function(x, ...)
    standardGeneric("calcTOAIrradTable"))
}
#' Get top of atmosphere solar irradiance using readily tabulated values
#'
#' @description
#' Get mean extraterrestrial solar irradiance (ESun) using published values.
#' 
#' @param x A Satellite object or sensor id ("LT4, LT5, LE7") as character.
#' @param normalize Logical; if \code{TRUE}, ESun is normalized to mean 
#' earth-sun distance. 
#' @param esd Earth-sun distance (AU, can be estimated using 
#' \code{\link{calcEarthSunDist}}). If x is a Satellite object and esd is not 
#' supplied and necessary for normalization, it is tried to take it from the 
#' metadata, otherwise it is estimated by the day of the year using 
#' \code{\link{calcEarthSunDist}}.
#'
#' @export calcTOAIrradTable
#' 
#' @name calcTOAIrradTable
#' 
#' @details Currently implemented sensors are Landsat 4, 5 and 7.
#' 
#' If results should not be normalized to a mean earth-sun distance, the 
#' actual earth-sun distance is approximated by the day of the year using
#' \code{\link{calcEarthSunDist}}.
#' 
#' Please note that ESun values are not required for converting Landsat 8 data 
#' to reflectance as the corresponding metadata files provide coefficients 
#' necessary to convert digital numbers to radiance and reflectance (taken from 
#' \url{https://www.gisagmaps.com/landsat-8-atco/}.
#' 
#' @references  Tabulated values of the solar irradiance for all Landsat sensors 
#' are taken from \url{https://www.usgs.gov/core-science-systems/nli/landsat/using-usgs-landsat-level-1-data-product}. 
#' 
#' @seealso \code{\link{calcTOAIrradRadRef}} for the computation of the solar 
#' irradiance based on maximum radiation and reflection values of the dataset or
#' \code{\link{calcTOAIrradModel}} for the computation of the solar irradiance 
#' based on look-up tables for the sensor's relative spectral response and solar 
#' irradiation spectral data.
#' 
#' See \code{\link{calcEarthSunDist}} for calculating the earth-sun
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth-sun distance.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE07*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#' calcTOAIrradTable(sat)
#'  
#' calcTOAIrradTable(x = "LE7", normalize = FALSE, 
#'                   calcEarthSunDist("2015-01-01"))
#' 
NULL

# Function using satellite object ----------------------------------------------
#' @return Satellite object with ESun information added to the metadata
#' @rdname calcTOAIrradTable
setMethod("calcTOAIrradTable", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, esd){
            if(normalize == FALSE & missing(esd)){
              esd <- getSatESD(x)
              if(is.na(esd)){
                esd <- calcEarthSunDist(date)
              } 
            }
            if(normalize == TRUE){
              esun <- calcTOAIrradTable(x = getSatSID(x), 
                                        normalize  = normalize)
            } else {
              esun <- calcTOAIrradTable(x = getSatSID(x), 
                                        normalize  = normalize, 
                                        esd = esd)
            }
            bcde <- names(esun)
            x <- addSatMetaParam(x, 
                                 meta_param = data.frame(
                                   BCDE = bcde,
                                   ESUN = as.numeric(esun)))
            return(x)
          })


# Function using factor --------------------------------------------------------
#' @return Vector object containing ESun for the respective band(s)
#' @rdname calcTOAIrradTable
setMethod("calcTOAIrradTable", 
          signature(x = "factor"),
          function(x, normalize = TRUE, esd){
            x <- as.character(x)  
            if(missing(esd)){
              eSun <- calcTOAIrradTable(x, normalize)
            } else {
              eSun <- calcTOAIrradTable(x, normalize, esd)
            }
            return(eSun)
          })            


# Function using character -----------------------------------------------------
#' @return Vector object containing ESun for the respective band(s)
#' @rdname calcTOAIrradTable
setMethod("calcTOAIrradTable", 
          signature(x = "character"),
          function(x, normalize = TRUE, esd){
            if(x %in% c("LE7", "LE07")) {
              eSun <- lut$L7_ESUN
            } else if(x %in% c("LT5", "LT05")) {
              eSun <- lut$L5_ESUN
            } else if(x %in% c("LT4", "LT04")) {
              eSun <- lut$L4_ESUN
            # } else if(x == "GLS5") {
            #   eSun <- lut$GLS5_ESUN
            } else if(x %in% c("LC8", "LC08")) {
              stop("ESun values are not provided for Landsat 8, see ?calcTOAIrradTable.")
            } else {
              stop(paste0("Satellite ID ", x, " is not supported, yet."))
            }
            if(normalize == FALSE){
              if(missing(esd)){
                stop("Variable esd is missing.")
              }
              eSun <- eSun / esd**2
            }
            return(eSun)
          })