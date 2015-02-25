if ( !isGeneric("satellite") ) {
  setGeneric("satellite", function(x, ...)
    standardGeneric("satellite"))
}

#' Create a Satellite object
#'
#' @description
#' Method to create a Satellite Object
#' 
#' @param x vector of one or more satellite data files, raster::RasterStack
#' @param meta optional supply a metadata object (e.g. returned from 
#' \code{\link{compMetaLandsat}})
#' @param log optional supply a log entry
#' 
#' @return Satellite object
#' 
#' @export satellite
#' 
#' @details Computation of ESun is taken from Updike and Comp (2011). Sun-earth
#' distance is computed using \code{\link{calcEartSunDist}}.
#' 
#' @references The formulas are taken from the following sources:
#' 
#' Spencer: Spencer JW (1971) Fourier series representation of the position of 
#' the sun. Search 2/5. Taken from 
#' \url{http://www.mail-archive.com/sundialuni-koeln.de/msg01050.html}. See
#' also: Bird R, Riordan C (1984) Simple solar spectral model for direct and 
#' diffuse irradiance on horizontal and tilted planes at the Earth's surface for
#' cloudless atmospheres. \url{http://www.nrel.gov/docs/legosti/old/2436.pdf}.
#' 
#' Mather:  Paul M. Mather (2005) Computer Processing of Remotely-Sensed Images:
#' An Introduction. Wiley, ISBN: 978-0-470-02101-9, 
#' \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470021012.html}.
#' 
#' ESA: ESA Earth Observation Quality Control: Landsat frequently asked questions. 
#' 
#' @seealso \code{\link{calcEartSunDist}} for calculating the sun-earth distance based
#' on the day of the year and \code{\link{eSun}} for wrapping this function and
#' alternative derivation of ESun.
#' 
#' @name satellite
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' satellite(files)
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
              if(lutInfoSGRPfromFilename(files) == "Landsat"){
                meta <- compMetaLandsat(x)
              } else {
                meta <- data.frame(NULL)
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
              meta <- data.frame(FILE = names(x))
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
