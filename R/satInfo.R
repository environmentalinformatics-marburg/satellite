#' Get or access Satellite object information used by various functions
#'
#' @description
#' Get information from class Satellite.
#' 
#' @param sat Satellite object (see \code{\link{satellite}})
#' 
#' @return Objects of respective type (see \code{\link{satellite}})
#'
#' @details The functions above return the following information:
#' \itemize{
#'   \item \code{satESD} returns 
#'   \item \code{satInfo} returns the entire metadata
#'   \item \code{satRadMax} returns the maximum radiance in band(s)
#'   \item \code{satRadMin} returns the minimum radiance in band(s) 
#'   \item \code{satRefMax} returns the maximum reflectance in band(s) 
#'   \item \code{satRefMin} returns the minimum reflectance in band(s) 
#'   \item \code{satSensor} returns the sensor name
#'   \item \code{satSID} returns the sensor ID
#' }
#' 
#' @name satInfo
#' 
#' @examples
#' satInfo()
#' 
NULL

# Return Satellite object metadata ---------------------------------------------
#' @export satMeta
#'
#' @rdname satInfo
#'
satMeta <- function(sat){
  return(sat@meta)
}


# Return Satellite data layers -------------------------------------------------
#' @export satLayers
#'
#' @rdname satInfo
#'
satLayers <- function(sat){
  return(sat@layers)
}


# Return Sensor ID -------------------------------------------------------------
#' @export satSID
#'
#' @rdname satInfo
#' 
satSID <- function(sat){
  return(satMeta(sat)$SID[1])
}


# Return Sensor ----------------------------------------------------------------
#' @export satSensor
#'
#' @rdname satInfo
#' 
satSensor <- function(sat){
  return(satMeta(sat)$SENSOR[1])
}


# Return RAD_MAX ---------------------------------------------------------------
#' @export satRadMax
#'
#' @rdname satInfo
#' 
satRadMax <- function(sat){
  return(satMeta(sat)$RADMAX)
}


# Return RAD_MIN ---------------------------------------------------------------
#' @export satRadMin
#'
#' @rdname satInfo
#' 
satRadMin <- function(sat){
  return(satMeta(sat)$RADMIN)
}


# Return REF_MAX ---------------------------------------------------------------
#' @export satRefMax
#'
#' @rdname satInfo
#' 
satRefMax <- function(sat){
  return(satMeta(sat)$REFMAX)
}


# Return REF_MIN ---------------------------------------------------------------
#' @export satRefMin
#'
#' @rdname satInfo
#' 
satRefMin <- function(sat){
  return(satMeta(sat)$REFMIN)
}


# Return ESD -------------------------------------------------------------------
#' @export satESD
#'
#' @rdname satInfo
#' 
satESD <- function(sat){
  return(satMeta(sat)$ESD)
}
