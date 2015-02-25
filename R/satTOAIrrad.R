if ( !isGeneric("satTOAIrradTable") ) {
  setGeneric("satTOAIrradTable", function(x, ...)
    standardGeneric("satTOAIrradTable"))
}

if ( !isGeneric("satTOAIrradModel") ) {
  setGeneric("satTOAIrradModel", function(x, ...)
    standardGeneric("satTOAIrradModel"))
}

if ( !isGeneric("satTOAIrradRadRef") ) {
  setGeneric("satTOAIrradRadRef", function(x, ...)
    standardGeneric("satTOAIrradRadRef"))
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
#' @param rsr Landsat 8 rsr (see \code{\link{calcTOAIrradModel}} for details)
#'
#' @return vector object containing ESun for each band
#'
#' @details 
#' Tabulated values of ESun are taken from the official reference handbooks or 
#' peer-review publications using function \code{\link{calcTOAIrradRadTable}}. 
#' 
#' Instead of returning tabulated values for Landsat 8 which are not available
#' in the official handbook, \code{\link{calcTOAIrradRadRef}} is used to 
#' compute the actual eSun value based on the scene's metadata. 
#' 
#' If ESun should be computed (all sensors), \code{\link{calcTOAIrradModel}} 
#' will be called by this function.
#' 
#' If eSun should be corrected for the actual earth sun distance, an
#' approximation of this distance is computed based on the day of by
#' \code{\link{calcEartSunDist}}. For Landsat 8, the respective earth sun distance is
#' taken from the metadata of the scene.
#' 
#' @references For references of the data sources, please refer to the 
#' documentation of the respective functions given in details or see also.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{calcTOAIrradRadTable}} which is used to get readily published 
#' values of ESun, \code{\link{calcTOAIrradRadRef}} which computes ESun based 
#' on the actual radiance and reflectance in the scene and for 
#' \code{\link{calcTOAIrradModel}} which computes ESun based on  
#' look-up tables for the sensor's relative spectral resonse and solar 
#' irradiation spectral data.
#' 
#' @name satTOAIrrad
#'  
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradTable(sat)
#' 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' satTOAIrradRadRef(sat)
# 
NULL


# Function using Satellite object and tabulated values of eSun -----------------
#' @param x object of type Satellite
#'
#' @export satTOAIrradTable
#' 
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradTable", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, date){
            if(missing(date)){
              return(calcTOAIrradRadTable(sensor = getSatSensor(x), 
                                          normalize  = normalize))
            } else {
              return(calcTOAIrradRadTable(sensor = getSatSensor(x), 
                                          normalize  = normalize, 
                                          date = date))
            }
          })


# Function using Satellite object and modelled values of eSun ------------------
#' @param x object of type Satellite
#' 
#' @export satTOAIrradModel
#'
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradModel", 
          signature(x = "Satellite"), 
          function(x, model = "MNewKur", normalize = TRUE, date){
            rsr <- lutInfoRSRromSID(sid = getSatSID(x))
            if(missing(date)){
              return(calcTOAIrradModel(rsr = rsr, model = model, 
                                       normalize = normalize))
            } else {
              return(calcTOAIrradModel(rsr = rsr, model = model, 
                                       normalize = normalize, date = date))
            }
          })


# Function using Satellite object and actual radiance and reflectance values ---
#' @param x object of type Satellite
#' 
#' @export satTOAIrradRadRef
#'
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradRadRef", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, date){
            if(missing(date)){
              return(calcTOAIrradRadRef(rad_max = getSatRadMax(x), 
                                        ref_max = getSatRefMax(x), 
                                        esd = getSatESD(x),
                                        normalize = normalize))
            } else {
              return(calcTOAIrradRadRef(rad_max = getSatRadMax(x), 
                                        ref_max = getSatRefMax(x), 
                                        esd = getSatESD(x),
                                        normalize = normalize, date = date))
            }
          })