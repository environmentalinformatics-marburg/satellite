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


# Add additional metainformation parameter to Satellite object -----------------
#' @export addSatMetaParam
#'
#' @rdname satInfo
#'
addSatMetaParam <- function(sat, meta_param){
  id <- colnames(meta_param)[1]
  sat@meta <- merge(sat@meta, meta_param, by = id)
  return(sat)
}


# Return Band IDs --------------------------------------------------------------
#' Needs to be a separate function since it is called from within
#' \code{getSatParam}. 
#' 
#' @export getSatBIDS
#'
#' @rdname satInfo
#' 
getSatBIDS <- function(sat){
  return(sat@meta$BIDS)
}


# Return parameter -------------------------------------------------------------
#' @param bids band ids
#' @export getSatParam
#'
#' @rdname satInfo
#' 
getSatParam <- function(sat, param, bids, return_bids = TRUE){
  if(missing(bids)){
    param <- getSatMeta(sat)[, which(param == colnames(getSatMeta(sat)))]
    bids <- as.character(getSatBIDS(sat))
  } else {
    param <- 
      getSatMeta(sat)[, 
                      which(param == colnames(getSatMeta(sat)))][match(
                        bids, getSatMeta(sat)$BIDS)]
    bids <- as.character(bids)
  }
  if(return_bids == TRUE){
    attr(param, "names") <- bids
  }
  return(param)
}


# Return Sensor ID -------------------------------------------------------------
#' @export getSatSID
#'
#' @rdname satInfo
#' 
getSatSID <- function(sat){
  getSatParam(sat, "SID", return_bids = FALSE)[1]
}


# Return Sensor ----------------------------------------------------------------
#' @export getSatSensor
#'
#' @rdname satInfo
#' 
getSatSensor <- function(sat){
  getSatParam(sat, "SENSOR", return_bids = FALSE)[1]
}


# Return solar bands -----------------------------------------------------------
#' @export getSatBIDSSolar
#'
#' @rdname satInfo
#' 
getSatBIDSSolar <- function(sat){
  spectrum <- getSatParam(sat, "SPECTRUM")
  
  return(spectrum[grep("solar", spectrum)])
}


# Return RAD_MAX ---------------------------------------------------------------
#' @export getSatRadMax
#'
#' @rdname satInfo
#' 
getSatRadMax <- function(sat, bids){
  getSatParam(sat, "RADMAX", bids)
}


# Return RAD_MIN ---------------------------------------------------------------
#' @export getSatRadMin
#'
#' @rdname satInfo
#' 
getSatRadMin <- function(sat, bids){
  getSatParam(sat, "RADMIN", bids)
}



# Return REF_MAX ---------------------------------------------------------------
#' @export getSatRefMax
#'
#' @rdname satInfo
#' 
getSatRefMax <- function(sat, bids){
  getSatParam(sat, "REFMAX", bids)
}



# Return REF_MIN ---------------------------------------------------------------
#' @export getSatRefMin
#'
#' @rdname satInfo
#' 
getSatRefMin <- function(sat){
  getSatParam(sat, "REFMIN", bids)
}


# Return ESD -------------------------------------------------------------------
#' @export getSatESD
#'
#' @rdname satInfo
#' 
getSatESD <- function(sat){
  getSatParam(sat, "ESD")[1]
}
