#' Get or access Satellite object information used by various functions
#'
#' @description
#' Get information from class Satellite.
#' 
#' @param sat Satellite object (see \code{\link{satellite}})
#' 
#' @return Objects of respective type (see \code{\link{satellite}})
#'
#' @export satInfo
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
#' @examples
#' satInfo()
#' 
satInfo <- function(sat){
  return(sat@meta)
}

# Return Sensor ID --------------------------------------------------------------
#' @export satSID
#'
#' @describeIn satInfo
#' 
satSID <- function(sat){
  return(sat@meta$SID[1])
}


# Return Sensor ----------------------------------------------------------------
#' @export satSensor
#'
#' @describeIn satInfo
#' 
satSensor <- function(sat){
  return(sat@meta$SENSOR[1])
}


# Return RAD_MAX ---------------------------------------------------------------
#' @export satRadMax
#'
#' @describeIn satInfo
#' 
satRadMax <- function(sat){
  return(sat@meta$RADMAX)
}


# Return RAD_MIN ---------------------------------------------------------------
#' @export satRadMin
#'
#' @describeIn satInfo
#' 
satRadMin <- function(sat){
  return(sat@meta$RADMIN)
}


# Return REF_MAX ---------------------------------------------------------------
#' @export satRefMax
#'
#' @describeIn satInfo
#' 
satRefMax <- function(sat){
  return(sat@meta$REFMAX)
}


# Return REF_MIN ---------------------------------------------------------------
#' @export satRefMin
#'
#' @describeIn satInfo
#' 
satRefMin <- function(sat){
  return(sat@meta$REFMIN)
}


# Return ESD -------------------------------------------------------------------
#' @export satESD
#'
#' @describeIn satInfo
#' 
satESD <- function(sat){
  return(sat@meta$ESD)
}
