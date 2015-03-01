#' Compile dark object DN for given sensor band
#'
#' @description
#' The function estimates the DN value of a "dark object" which is used for 
#' atmospheric correction using the DOS2 and DOS4 model. Therefore, the 
#' frequency distribution of the smalest 1% of the data values is analyzed 
#' and the value for which the first derivate has the absolute maximum is
#' taken as the DN for a dark object.
#'
#' @param band satellite sensor band data (raster layer)
#'
#' @return Numeric value of the DN for the dark object.
#'
#' @export calcDODN
#' 
#' @details The DN for a dark object is extracted from a histogram similar to
#' Chavez (1988).
#'  
#' @references Chavez Jr PS (1988) An improved dark-object subtraction technique 
#' for atmospheric scattering correction of multispectral data. Remote Sensing 
#' of Environment 24/3, doi:10.1016/0034-4257(88)90019-3, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788900193}
#'  
#' @seealso The DN is used by \code{\link{calcPathRadDOS}} for computing the 
#' path radiance based on the dark object method.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' calcDODN(getSatDataLayer(sat, bcde = "B002n"))
#' 
calcDODN <- function(band){
  freq <- plyr::count(raster::getValues(band))
  q01 <- raster::quantile(band, probs = 0.01)
  freq_q01 <- freq[freq$x <= q01, ]
  return(freq_q01$x[which.max(diff(freq_q01$freq))])
}
