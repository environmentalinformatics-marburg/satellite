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
#' Get TOA solar irradiance (ESun) for the layers of a Satellite object
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, either (i) the mean solar spectral data and 
#' the band specifiv relative spectral response functions (rsr) are used or (ii)
#' actual maximum radiance and reflection values are taken.
#' 
#' @param x object of type Satellite
#' @param model name of the model to be used (see \code{\link{calcTOAIrradModel}})
#' @param normalize normalize ESun to mean earth sun distance
#' @param esd earth-sun distance (AU), if not supplied and necessary for
#' normalize, it is tried to take it from the metadata, otherwise it is estimated
#' by the day of the year using \code{\link{calcEartSunDist}}.
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object), only
#' necessary if esd is required but not given explicitely or found in the 
#' metadata.
#'
#' @return Satellite object with ESun for each band in the metadata
#'
#' @details 
#' Tabulated values of ESun are compiled using \code{\link{calcTOAIrradRadTable}}. 
#' 
#' Actual values of ESun are computed using \code{\link{calcTOAIrradRadRef}}.
#' 
#' Modeled values of ESun are computed using \code{\link{calcTOAIrradModel}}.
#' 
#' If eSun should be corrected for the actual earth sun distance, the distance 
#' is taken from (i) the function parameter, (ii) the metadata or (iii) 
#' approximation using \code{\link{calcEartSunDist}} which is called from
#' within the functions above.
#' 
#' Please refer to the respective functions for details on the computation.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{calcTOAIrradRadTable}}, \code{\link{calcTOAIrradRadRef}} and 
#' \code{\link{calcTOAIrradModel}}.
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
#' 
#' satTOAIrradModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' satTOAIrradRadRef(sat)
# 
NULL


# Function using Satellite object and tabulated values of eSun -----------------
#'
#' @param meta_param data frame containing the name of the metadata ID field 
#' used to merge with the readily existing metadata information of the
#' Satellite class object in the first column and with the same column header
#' as used in the existing metadata information. All other columns contain the
#' information to be added to the metadata.
#' 
#' @export satTOAIrradTable
#' 
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradTable", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, esd, date){
            if(normalize == TRUE){
              esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                           normalize  = normalize)
            } else {
              if(missing(esd)){
                esd = getSatESD(x)
                if(is.na(esd)){
                  esd = calcEartSunDist(date)
                } 
              }
              esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                           normalize  = normalize, 
                                           esd = esd)
            }
            x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(esun),
                                                            ESUN = as.numeric(esun)))
            return(x)
          })


# Function using Satellite object and modelled values of eSun ------------------
#' 
#' @export satTOAIrradModel
#'
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradModel", 
          signature(x = "Satellite"), 
          function(x, model = "MNewKur", normalize = TRUE, esd, date){
            rsr <- lutInfoRSRromSID(sid = getSatSID(x))
            if(normalize == TRUE){
              esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                        normalize = normalize)
            } else {
              if(missing(esd)){
                esd = getSatESD(x)
                if(is.na(esd)){
                  esd = calcEartSunDist(date)
                } 
              }
              esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                        normalize = normalize, esd = esd)
            }
            x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(esun),
                                                            ESUN = as.numeric(esun)))
            return(x)
          })


# Function using Satellite object and actual radiance and reflectance values ---
#' 
#' @export satTOAIrradRadRef
#'
#' @rdname satTOAIrrad
#' 
setMethod("satTOAIrradRadRef", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, esd, date){
            if(normalize == TRUE){
              if(missing(esd)){
                esd = getSatESD(x)
                if(is.na(esd)){
                  esd = calcEartSunDist(date)
                } 
              }
              esun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                                         ref_max = getSatRefMax(x, getSatBCDESolar(x)),
                                         esd = esd,
                                         normalize = normalize)
            } else {
              esun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                                         ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
                                         normalize = normalize)
            }
            x <- addSatMetaParam(x, meta_param = data.frame(BCDE = getSatBCDESolar(x),
                                                            ESUN = as.numeric(esun)))
            return(x)
          })