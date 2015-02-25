#' Identify pseudi-invariant features from a satellite scene
#'
#' @description
#' Identify pseudi-invariant features from a satellite scene based on a 
#' red, near-infrared and short-wave infrared band.
#'
#' @param red a raster of the sensor's red band
#' @param nir a raster of the sensor's nir band
#' @param swir a raster of the sensor's swir band
#'
#' @return raster object with invariant pixels marked with 1, 0 otherwise
#'
#' @export invariantFeatures
#' 
#' @references This function is taken and only slightly adapted from the PIF
#' function of Sarah C. Goslee (2011). Analyzing Remote Sensing Data in R: The 
#' landsat Package. Journal of Statistical Software,43(4), 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' The underlaying theory has been published by Schott RJ, Salvaggio C and 
#' Volchok WJ (1988) Radiometric scene normalization using pseudoinvariant 
#' features. Remote Sensing of Environment 26/1, 
#' doi:10.1016/0034-4257(88)90116-2, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788901162}
#'
#' @examples
#' not run:
#' invariantFeatures(red, nir, swir, level=.99)
invariantFeatures <-
  function(red, nir, swir, level=.99) {
    # identify pseudo-invariant features after SSV1988
    
    if(is.character(red)) {
      red <- read.asciigrid(red)
      pifgrid <- red
      red <- red@data[,1]
    } else {
      pifgrid <- red
      red <- as.vector(as.matrix(red))
    } 
    
    if(is.character(nir)) {
      nir <- read.asciigrid(nir)@data[,1]
    } else {
      nir <- as.vector(as.matrix(nir))
    }
    
    if(is.character(swir)) {
      swir <- read.asciigrid(swir)@data[,1]
    } else {
      swir <- as.vector(as.matrix(swir))
    }
    
    nir3 <- nir/red
    
    nir3.level <- quantile(nir3, 1-level, na.rm=TRUE)
    swir.level <- quantile(swir, level, na.rm=TRUE)
    
    pifmask <- ifelse(nir3 < nir3.level & swir > swir.level & swir < 255, 1, 0)
    
    # return the same structure as the input values
    if(class(pifgrid) == "SpatialGridDataFrame")
      pifgrid@data[,1] <- pifmask
    else if(is.data.frame(pifgrid))
      pifgrid <- data.frame(matrix(pifmask, nrow=nrow(pifgrid), ncol=ncol(pifgrid)))
    else if(is.matrix(pifgrid))
      pifgrid <- matrix(pifmask, nrow=nrow(pifgrid), ncol=ncol(pifgrid))
    else # return a vector 
      pifgrid <- pifmask
    
    pifgrid
  }
