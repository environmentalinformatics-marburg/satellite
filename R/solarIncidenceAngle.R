if ( !isGeneric("solarIncidenceAngle") ) {
  setGeneric("solarIncidenceAngle", function(x, ...)
    standardGeneric("solarIncidenceAngle"))
}
#' Compute solar incidence angle
#'
#' @description
#' Compute the solar incidence angle over non-planar surfaces following the
#' equation from Duffie and Beckman (2013).
#'
#' @param x A date-time object of class \code{POSIX*}, or a \code{character} 
#' object that can be coerced by \code{\link{strptime}}, or a \code{Raster*} 
#' object holding such information.
#' @param dem \code{RasterLayer}, digital elevation model in EPSG:4326
#' (see \url{http://spatialreference.org/ref/epsg/wgs-84/}).
#' @param formula \code{character}, formula from which to calculate the angular
#' declination of the sun, see \code{\link[satellite]{declination}}.
#' @param neighbors \code{integer}, number of focal cells from which to 
#' calculate surface slope and aspect, see \code{\link{terrain}}.
#' @param unit \code{character}, determines whether to return the solar 
#' incidence angle in radians (default) or degrees.
#' @param ... If \code{x} is a \code{character} object, formatting arguments 
#' passed on to \code{\link{strptime}}, or if \code{x} is a \code{Raster*} 
#' object, formatting arguments related with the specified \code{origin}.
#' @param origin A \code{POSIX*} or \code{character} object that defines the 
#' origin of \code{x}, see \code{\link{solarHourAngle}}.
#'
#' @return
#' A \code{Raster*} object with pixel-wise solar incidence angles.
#'
#' @note 
#' An angle of more than 90 degrees, or likewise \code{pi/2}, indicates that the 
#' sun is behind the surface.
#' 
#' @references
#' Duffie JA, Beckman WA (2013) Solar Engineering of Thermal Processes. Wiley:
#' Hoboken, New Jersey, ISBN: 978-0-470-87366-3,
#' \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470873663.html}.
#'
#' @seealso
#' \code{\link[satellite]{declination}}, \code{\link{solarHourAngle}}, 
#' \code{\link{terrain}}.
#'
#' @examples 
#' ## import sample data
#' dem <- system.file("extdata/DEM.TIF", package = "satellite")
#' dem <- trim(projectRaster(raster(dem), crs = "+init=epsg:4326"))
#' 
#' ## current solar incidence angle
#' solarIncidenceAngle(Sys.time(), dem, formula = "Spencer", 
#'                     unit = "degrees")
#' 
#' @export solarIncidenceAngle
#' @name solarIncidenceAngle
NULL

### 'POSIXt' method ----- 
#' @aliases solarIncidenceAngle,POSIXt-method
#' @rdname solarIncidenceAngle
setMethod("solarIncidenceAngle", 
          signature(x = "POSIXt"), 
          function(x, dem, formula = c("Cooper", "Spencer"),
                   neighbors = 8L, unit = c("radians", "degrees")) {

  ## compute angular declination of the sun
  delta <- declination(as.Date(x), formula = formula[1], unit = "radians")

  ## pixel-wise latitude
  yrs <- raster::res(dem)[2]
  lts <- seq(raster::ymax(dem) - .5 * yrs, raster::ymin(dem), -yrs)

  mat <- matrix(ncol = raster::ncol(dem), nrow = raster::nrow(dem))
  for (i in 1:nrow(mat)) mat[i, ] <- lts[i]

  phi <- raster::raster(mat, template = dem)

  ## compute surface slope and aspect
  slp <- raster::terrain(dem, opt = "slope", neighbors = neighbors, 
                         unit = "radians")

  asp <- raster::terrain(dem, opt = "aspect", neighbors = neighbors, 
                         unit = "radians")
  asp <- raster::calc(asp, fun = function(x) x - pi)

  ## compute solar hour angle
  omega <- solarHourAngle(x, unit = "radians")

  ## compute solar incidence angle
  theta <- acos(sin(delta) * sin(phi) * cos(slp) - 
                  sin(delta) * cos(phi) * sin(slp) * cos(asp) + 
                  cos(delta) * cos(phi) * cos(slp) * cos(omega) + 
                  cos(delta) * sin(phi) * sin(slp) * cos(asp) * cos(omega) + 
                  cos(delta) * sin(asp) * sin(slp) * sin(omega))

  ## convert to degrees (optional) and return
  if (unit[1] == "degrees")
    return(theta * 180 / pi)
  else
    return(theta)
})

### 'character' method ----- 
#' @aliases solarIncidenceAngle,character-method
#' @rdname solarIncidenceAngle
setMethod("solarIncidenceAngle", 
          signature(x = "character"), 
          function(x, dem, formula = c("Cooper", "Spencer"),
                   neighbors = 8L, unit = c("radians", "degrees"), 
                   ...) {
            
  ## convert to 'POSIXt' object
  x <- strptime(x, ...)
  
  ## calculate solar incidence angle
  solarIncidenceAngle(x, dem, formula[1], neighbors, unit[1])
})
          
### 'Raster' method -----
#' @aliases solarIncidenceAngle,Raster-method
#' @rdname solarIncidenceAngle
setMethod("solarIncidenceAngle", 
          signature(x = "Raster"), 
          function(x, origin = strptime("1993-01-01", "%Y-%m-%d"), dem, 
                   formula = c("Cooper", "Spencer"), neighbors = 8L, 
                   unit = c("radians", "degrees"), ...) {

  ## convert 'origin' to 'POSIXt'
  if (!any(class(origin) == "POSIXt"))
    origin <- strptime(origin, ...)
            
  ## compute pixel-wise angular declination of the sun
  delta <- declination(as.Date(origin + raster::getValues(x)), 
                       formula = formula[1], unit = "radians")          
  delta <- raster::setValues(x, delta)          
  
  ## pixel-wise latitude
  yrs <- raster::res(dem)[2]
  lts <- seq(raster::ymax(dem) - .5 * yrs, raster::ymin(dem), -yrs)
  
  mat <- matrix(ncol = raster::ncol(dem), nrow = raster::nrow(dem))
  for (i in 1:nrow(mat)) mat[i, ] <- lts[i]
  
  phi <- raster::raster(mat, template = dem) * pi / 180
  
  ## compute surface slope and aspect
  slp <- raster::terrain(dem, opt = "slope", neighbors = neighbors, 
                         unit = "radians")
  
  asp <- raster::terrain(dem, opt = "aspect", neighbors = neighbors, 
                         unit = "radians")
  asp <- raster::calc(asp, fun = function(x) x - pi)
  
  ## compute solar hour angle
  omega <- solarHourAngle(x, unit = "radians")
  
  ## compute solar incidence angle
  theta <- acos(sin(delta) * sin(phi) * cos(slp) - 
                  sin(delta) * cos(phi) * sin(slp) * cos(asp) + 
                  cos(delta) * cos(phi) * cos(slp) * cos(omega) + 
                  cos(delta) * sin(phi) * sin(slp) * cos(asp) * cos(omega) + 
                  cos(delta) * sin(asp) * sin(slp) * sin(omega))
  
  ## convert to degrees (optional) and return
  if (unit[1] == "degrees")
    return(theta * 180 / pi)
  else
    return(theta)
})
