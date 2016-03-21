if ( !isGeneric("surfacePressure") ) {
  setGeneric("surfacePressure", function(gp, ta, dem, p, ...)
    standardGeneric("surfacePressure"))
}
#' Compute near-surface pressure
#' 
#' @description 
#' Compute the near-surface air pressure from the barometric formula using the
#' geopotential height of certain pressure levels, the referring temperature 
#' profile and the underlying digital elevation model as input data.
#' 
#' @param gp Geopotential heights as \code{Raster*} or, if a spatially distinct 
#' location is processed, as \code{numeric} vector.
#' @param ta Temperature profile of the same class as \code{gp}.
#' @param dem If \code{gp} is a \code{Raster*} object, a digital elevation model 
#' as \code{RasterLayer} or \code{numeric} vector (e.g. derived from 
#' \code{\link{getValues}}); else a single \code{numeric} elevation value.
#' @param p \code{numeric} pressure levels associated with \code{gp}. 
#' @param ... If \code{gp} is a \code{Raster*} object, further arguments passed 
#' on to \code{\link{writeRaster}}.
#' 
#' @return 
#' If \code{gp} is a \code{Raster*} object, a \code{RasterLayer} with 
#' near-surface air pressure; else a single \code{numeric} value.
#' 
#' @references 
#' Roedel W, Wagner T (2011) Physik unserer Umwelt: Die Atmosphaere. Springer: 
#' Heidelberg, Dordrecht, London, New York, ISBN: 978-3-642-15728-8. Available 
#' online at \url{http://cost733class.geo.uni-augsburg.de/moin/iguawiki/data/pages/KursmaterialWS1213_HsKlivar/attachments/Roedel_Die_Atmosphaere.pdf}.
#' 
#' @examples 
#' ### example from MYD07_L2 (20 levels) ---
#' 
#' ## pressure levels             
#' p <- c(5, 10, 20, 30, 50, 70, 100, 150, 200, 250, 300, 400, 500, 620, 700, 
#'        780, 850, 920, 950, 1000) 
#'        
#' ## geopotential heights
#' gp <- c(35005.573, 30329.573, 25786.936, 23204.563, 20074.706, 18113.499, 
#'         16109.901, 13765.185, 11966.857, 10484.984, 9218.005, 7115.921, 
#'         5399.528, 3679.846, 2676.816, 1760.488, 1018.150, NA, NA, NA)
#'         
#' ## temperature profile         
#' ta <- c(234.6088, 226.9892, 220.2491, 214.4910, 203.8553, 194.2660, 191.1759, 
#'         205.9155, 220.9708, 232.4576, 241.8924, 256.9488, 267.5008, 277.0947, 
#'         283.7734, 289.8672, 294.1234, NA, NA, NA)   
#'     
#' ## pixel elevation     
#' z <- 3625.708                                                    
#' 
#' ## compute near-surface air pressure
#' surfacePressure(gp, ta, z, p)
#' 
#' @export surfacePressure
#' @name surfacePressure
NULL

### 'Raster' method ----- 
#' @aliases surfacePressure,Raster-method
#' @rdname surfacePressure
setMethod("surfacePressure", 
          signature(gp = "Raster"), 
          function(gp, ta, dem, p, ...) {
  
  ## if 'dem' is a 'Raster*' object, extract values 
  if (length(grep("^Raster", class(dem))) > 0) {
    if (raster::nlayers(dem) > 1) 
      warning("The provided DEM has more than one layer and only the first will be used.\n")
    
    dem <- raster::getValues(dem[[1]])
  }
  
  ## compute near-surface air pressure
  raster::overlay(gp, ta, fun = function(x, y) {
    val <- run_barometricFormula(x[], y[], dem, p)
    val[val == -999] <- NA
    return(val)
  }, unstack = FALSE, ...)
})

### 'numeric' method ----- 
#' @aliases surfacePressure,numeric-method
#' @rdname surfacePressure
setMethod("surfacePressure", 
          signature(gp = "numeric"), 
          function(gp, ta, dem, p) {
  
  ## compute near-surface air pressure
  barometricFormula(dem, gp, ta, p)
})