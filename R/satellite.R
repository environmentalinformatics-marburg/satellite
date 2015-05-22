if ( !isGeneric("satellite") ) {
  setGeneric("satellite", function(x, ...)
    standardGeneric("satellite"))
}

#' Create a Satellite object
#'
#' @description
#' Method to create a Satellite Object
#' 
#' @param x A vector of one or more satellite data files or a 
#' raster::RasterStack
#' @param meta Optional supply a metadata object (e.g. returned from 
#' \code{\link{compMetaLandsat}}). If x is a satellite data file and recognised
#' as "Landsat", then the meta data is automatically extracted from the 
#' respective meta information file if both the satellite data and the meta
#' data file follow the naming from the USGS Earth Explorer.
#' #' @param log Optional supply a log entry.
#' 
#' @return Satellite object
#' 
#' @export satellite
#' 
#' @details A satellite object consists of three data sections:
#' (i) a raster data section which holds the actual data values of the 
#' respective sensor bands, (ii) a meta data grid which holds meta information
#' for each of the sensor band layers (e.g. calibration coefficients, type of
#' sensor band etc.) and (iii) a list of log information which records the
#' processing history of the entire data set.
#' 
#' @seealso \code{\link{compMetaLandsat}} to get more information about the
#' structure of the metadata component.
#' 
#' @name satellite
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
NULL


# Function using vector of filenames -------------------------------------------
#' 
#' @rdname satellite
#' 
setMethod("satellite", 
          signature(x = "character"), 
          function(x, meta, log){
            if(missing(meta)){
              if(lutInfoSGRPfromFilename(x) == "Landsat"){
                meta <- compMetaLandsat(x)
              } else {
                meta <- data.frame(LAYER = 
                                     tools::file_path_sans_ext(basename(x)),
                                   FILE = x)
              }
            }
            layers <- lapply(meta$FILE, function(y){
              raster(y)
            })
            if(missing(log)){
              ps <- list(time = Sys.time(), info = "Initial import", 
                         layers = "all", output = "all")
              log <- list(ps0001 = ps)
            }
            return(new("Satellite", layers = layers, meta = meta, log = log))
          })


# Function using readily existing raster layers --------------------------------
#'
#' @rdname satellite
#' 
setMethod("satellite", 
          signature(x = "RasterStack"), 
          function(x, meta, log){
            if(missing(meta)){
              meta <- data.frame(DATE = as.POSIXlt(Sys.Date(), tz = "UTC"),
                                 FILE = names(x))
            }
            layers <- lapply(seq(nlayers(x)), function(y){
              x[[y]]
            })
            if(missing(log)){
              ps <- list(time = Sys.time(), info = "Initial import", 
                         layers = "all", output = "all")
              log <- list(ps0001 = ps)
            }
            return(new("Satellite", layers = layers, meta = meta, log = log))
          })
