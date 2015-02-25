#' Get or access Satellite object information used by various functions
#'
#' @description
#' Get information from class Satellite.
#' 
#' @param sat Satellite object (see \code{\link{satellite}})
#' 
#' @return Objects of respective type (see \code{\link{satellite}})
#'
#' @details The functions are generally self explaining in that sence that
#' \code{get*} returns the respective information and \code{set*} sets the
#' respective information from/in the Satellite object.
#'  
#' \code{addSatLog} adds a log entry to the Satellite object
#' 
#' @name satInfo
#' 
#' @examples
#' # List of input files
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' # Raster stack l8
#' sat <- satellite(l8)
#' 
NULL


# Return Satellite data layers -------------------------------------------------
#' @export getSatLayers
#'
#' @rdname satInfo
#'
getSatLayers <- function(sat){
  return(sat@layers)
}


# Return Satellite object metadata ---------------------------------------------
#' @export getSatMeta
#'
#' @rdname satInfo
#'
getSatMeta <- function(sat){
  return(sat@meta)
}



# Return Satellite object log info ---------------------------------------------
#' @export getSatLog
#'
#' @rdname satInfo
#'
getSatLog <- function(sat){
  return(sat@log)
}


# Add Satellite object log info ------------------------------------------------
#' @export addSatLog
#'
#' @rdname satInfo
#'
addSatLog <- function(sat, info = NA_character_, layers = NA_character_, 
                      output = NA_character_){
  new_length <- length(getSatLog(sat)) + 1
  ps <- sprintf("ps%04d", new_length)
  sat@log <- append(sat@log, list(list(time = Sys.time(), info = info, 
                                       layers = layers, output = output)))
  names(sat@log)[new_length] <- ps
  return(sat)
}


# Return Sensor ID -------------------------------------------------------------
#' @export getSatSID
#'
#' @rdname satInfo
#' 
getSatSID <- function(sat){
  return(getSatMeta(sat)$SID[1])
}


# Return Sensor ----------------------------------------------------------------
#' @export getSatSensor
#'
#' @rdname satInfo
#' 
getSatSensor <- function(sat){
  return(getSatMeta(sat)$SENSOR[1])
}


# Return RAD_MAX ---------------------------------------------------------------
#' @export getSatRadMax
#'
#' @rdname satInfo
#' 
getSatRadMax <- function(sat){
  return(getSatMeta(sat)$RADMAX)
}


# Return RAD_MIN ---------------------------------------------------------------
#' @export getSatRadMin
#'
#' @rdname satInfo
#' 
getSatRadMin <- function(sat){
  return(getSatMeta(sat)$RADMIN)
}


# Return REF_MAX ---------------------------------------------------------------
#' @export getSatRefMax
#'
#' @rdname satInfo
#' 
getSatRefMax <- function(sat){
  return(getSatMeta(sat)$REFMAX)
}


# Return REF_MIN ---------------------------------------------------------------
#' @export getSatRefMin
#'
#' @rdname satInfo
#' 
getSatRefMin <- function(sat){
  return(getSatMeta(sat)$REFMIN)
}


# Return ESD -------------------------------------------------------------------
#' @export getSatESD
#'
#' @rdname satInfo
#' 
getSatESD <- function(sat){
  return(getSatMeta(sat)$ESD)
}
