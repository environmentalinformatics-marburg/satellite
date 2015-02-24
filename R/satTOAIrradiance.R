if ( !isGeneric("satTOAIrradianceTable") ) {
  setGeneric("satTOAIrradianceTable", function(x, ...)
    standardGeneric("satTOAIrradianceTable"))
}

if ( !isGeneric("satTOAIrradianceModel") ) {
  setGeneric("satTOAIrradianceModel", function(x, ...)
    standardGeneric("satTOAIrradianceModel"))
}

if ( !isGeneric("satTOAIrradianceRadRef") ) {
  setGeneric("satTOAIrradianceRadRef", function(x, ...)
    standardGeneric("satTOAIrradianceRadRef"))
}

#' Get extraterrestrial solar irradiance (ESun) for satellite bands
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, mean solar spectral data and the band 
#' specifiv relative spectral response functions (rsr) are used.
#' 
#' For Landsat 8, no tabulated values are availabe. Instead (and if tab = TRUE)
#' ESun will be calculated based on the actual maximum radiance and reflection 
#' given in the metadata file. Otherwise they will be computed using the 
#' approach discribed above.
#'
#' @param sensor sensor name ("Landsat 8/7/5/4")
#' @param tab use tabulated or measured (Landsat 8) values (TRUE) or compute 
#' values based on rsr (FALSE)
#' @param normalize normalize ESun to mean earth sun distance
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object), only 
#' relevant if normalize = FALSE
#' @param rsr Landsat 8 rsr (see \code{\link{calcTOAIrradianceModel}} for details)
#'
#' @return vector object containing ESun for each band
#'
#' @details 
#' Tabulated values of ESun are taken from the official reference handbooks or 
#' peer-review publications using function \code{\link{calcTOAIrradianceTable}}. 
#' 
#' Instead of returning tabulated values for Landsat 8 which are not available
#' in the official handbook, \code{\link{calcTOAIrradianceRadRef}} is used to 
#' compute the actual eSun value based on the scene's metadata. 
#' 
#' If ESun should be computed (all sensors), \code{\link{calcTOAIrradianceModel}} 
#' will be called by this function.
#' 
#' If eSun should be corrected for the actual earth sun distance, an
#' approximation of this distance is computed based on the day of by
#' \code{\link{earthSun}}. For Landsat 8, the respective earth sun distance is
#' taken from the metadata of the scene.
#' 
#' @references For references of the data sources, please refer to the 
#' documentation of the respective functions given in details or see also.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{calcTOAIrradianceTable}} which is used to get readily published 
#' values of ESun, \code{\link{calcTOAIrradianceRadRef}} which computes ESun based 
#' on the actual radiance and reflectance in the scene and for 
#' \code{\link{calcTOAIrradianceModel}} which computes ESun based on  
#' look-up tables for the sensor's relative spectral resonse and solar 
#' irradiation spectral data.
#' 
#' @name satTOAIrradiance
#'  
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradianceTable(sat)
#' 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradianceModel(sat)
# 
NULL


# Function using Satellite object and tabulated values of eSun -----------------
#' @param x object of type Satellite
#'
#' @export satTOAIrradianceTable
#' 
#' @rdname satTOAIrradiance
#' 
setMethod("satTOAIrradianceTable", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, date){
            if(missing(date)){
              return(calcTOAIrradianceTable(sensor = satSensor(x), 
                                            normalize  = normalize))
            } else {
              return(calcTOAIrradianceTable(sensor = satSensor(x), 
                                            normalize  = normalize, 
                                            date = date))
            }
          })


# Function using Satellite object and modelled values of eSun ------------------
#' @param x object of type Satellite
#' 
#' @export satTOAIrradianceModel
#'
#' @rdname satTOAIrradiance
#' 
setMethod("satTOAIrradianceModel", 
          signature(x = "Satellite"), 
          function(x, model = "MNewKur", normalize = TRUE, date){
            rsr <- lutInfoRSRromSID(sid = satSID(x))
            if(missing(date)){
              return(calcTOAIrradianceModel(rsr = rsr, model = model, normalize = normalize))
            } else {
              return(calcTOAIrradianceModel(rsr = rsr, model = model, normalize = normalize, date = date))
            }
          })


# Function using Satellite object and actual radiance and reflectance values ---
#' @param x object of type Satellite
#' 
#' @export satTOAIrradianceRadRef
#'
#' @rdname satTOAIrradiance
#' 
setMethod("satTOAIrradianceRadRef", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, date){
            if(missing(date)){
              return(calcTOAIrradianceRadRef(rad_max = satRadMax(x), 
                                            ref_max = satRefMax(x), 
                                            esd = satESD(x),
                                            normalize = normalize))
            } else {
              return(calcTOAIrradianceRadRef(rad_max = satRadMax(x), 
                                            ref_max = satRefMax(x), 
                                            esd = satESD(x),
                                            normalize = normalize, date = date))
            }
          })