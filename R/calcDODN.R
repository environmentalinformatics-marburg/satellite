if ( !isGeneric("calcDODN") ) {
  setGeneric("calcDODN", function(x, ...)
    standardGeneric("calcDODN"))
}
#' Compile dark object DN for given sensor band
#'
#' @description
#' The function estimates the DN value of a "dark object" which is used for 
#' atmospheric correction using the DOS2 and DOS4 model. Therefore, the 
#' frequency distribution of the smallest 1\% of the data values is analyzed 
#' and the value for which the first derivate has the absolute maximum is
#' taken as the DN for a dark object.
#'
#' @param x Satellite object or RasterLayer with sensor band data, e.g. returned 
#' by \code{\link{getSatDataLayer}}. 
#' @param bcde If 'x' is a Satellite object, a band code as character.
#'
#' @return Numeric value of the DN for the dark object.
#' 
#' @details The DN for a dark object is extracted from a histogram similar to
#' Chavez (1988).
#'  
#' @references Chavez Jr PS (1988) An improved dark-object subtraction technique 
#' for atmospheric scattering correction of multispectral data. Remote Sensing 
#' of Environment 24/3, \doi{10.1016/0034-4257(88)90019-3}.
#'  
#' @seealso The DN is used by \code{\link{calcPathRadDOS}} for computing the 
#' path radiance based on the dark object method.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' calcDODN(sat, bcde = "B002n")
#' calcDODN(getSatDataLayer(sat, bcde = "B002n"))
#' 
#' @export calcDODN
#' @name calcDODN
NULL

# Function using Satellite object ----------------------------------------------
#' @rdname calcDODN
setMethod("calcDODN", 
          signature(x = "Satellite"), 
          function(x, bcde) {
  calcDODN(getSatDataLayer(x, bcde))
})


# Function using Satellite object ----------------------------------------------
#' @rdname calcDODN
setMethod("calcDODN", 
          signature(x = "RasterLayer"), 
          function(x) {
  
  ## stop if a multi-layer raster is supplied
  if (!inherits(x, "RasterLayer"))
    stop("Please supply a single-layer object.")
  
  vals <- raster::getValues(x)
  #use only values > 0 (if using whole satellite images e.g. landsat no data values
  #may be zero or negative, which will lead to a value of zero in calcDODN function)
  vals <- vals[vals > 0]
  freq <- plyr::count(vals)
  q01 <- quantile(vals, probs = 0.01, na.rm = TRUE)
  freq_q01 <- freq[freq$x <= q01, ]
  return(freq_q01$x[which.max(diff(freq_q01$freq))])
})
