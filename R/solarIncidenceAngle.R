#' Compute solar incidence and hour angle
#'
#' @description
#' Compute the solar incidence angle over non-planar surfaces following the
#' equation from Duffie and Beckman (2013) and the solar hour angle defined as 
#' the time difference from noon times 15 degrees per hour.
#'
#' @param x A date-time object of class \code{POSIX*} or, alternatively, a
#' \code{character} object that can be coerced by \code{\link{strptime}}.
#' @param dem \code{RasterLayer}, digital elevation model in EPSG:4326
#' \url{http://spatialreference.org/ref/epsg/wgs-84/}.
#' @param formula Formula to deploy for the calculation of the angular
#' declination of the sun, see \code{\link[satellite]{declination}}.
#' @param neighbors \code{integer}, number of focal cells to use for the
#' computation of surface slope and aspect, see \code{\link{terrain}}.
#' @param unit \code{character}, determines whether the solar incidence angle is 
#' returned in radians (default) or degrees.
#' @param ... If \code{x} is a \code{character} object, further arguments passed
#' on to \code{\link{strptime}}.
#'
#' @return
#' \itemize{
#' \item \emph{solarIncidenceAngle}: A \code{RasterLayer} with pixel-wise solar 
#' incidence angles.
#' \item \emph{hourAngle}: A \code{numeric} solar hour angle.
#' }
#'
#' @references
#' Duffie JA, Beckman WA (2013) Solar Engineering of Thermal Processes. Wiley:
#' Hoboken, New Jersey, ISBN: 978-0-470-87366-3,
#' \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470873663.html}.
#'
#' @seealso
#' \code{\link[satellite]{declination}}.
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
#' ## current solar hour angle
#' hourAngle(Sys.time())
#' 
#' @export solarIncidenceAngle
#' @name solarIncidenceAngle
solarIncidenceAngle <- function(x, dem, formula = c("Cooper", "Spencer"),
                                neighbors = 8L, unit = c("radians", "degrees"), 
                                ...) {

  ## convert 'x' to 'POSIXt' object (optional)
  if (!any(class(x) == "POSIXt"))
    x <- strptime(x, ...)

  ## compute angular declination of the sun
  delta <- declination(as.Date(x), formula = formula[1])

  ## pixel-wise latitude
  yrs <- raster::res(dem)[2]
  lts <- seq(raster::ymax(dem) - .5 * yrs, raster::ymin(dem), -yrs)

  mat <- matrix(ncol = raster::ncol(dem), nrow = raster::nrow(dem))
  for (i in 1:nrow(mat)) mat[i, ] <- lts[i]

  phi <- raster::raster(mat, template = dem)

  ## compute surface slope and aspect
  slp <- raster::terrain(dem, opt = "slope", neighbors = neighbors)

  asp <- raster::terrain(dem, opt = "aspect", neighbors = neighbors)
  asp <- raster::calc(asp, fun = function(x) x - pi)

  ## compute hour angle
  omega <- hourAngle(x)

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
}


### compute hour angle ---------------------------------------------------------
#' @rdname solarIncidenceAngle
#' @export hourAngle
hourAngle <- function(x, ...) {

  ## convert 'x' to 'POSIXt' object (optional)
  if (!any(class(x) == "POSIXt"))
    x <- strptime(x, ...)

  ## convert to decimal hours
  dtm <- strftime(x, "%H:%M")
  dcm <- sapply(strsplit(dtm, ":"), function(x) {
    x <- as.numeric(x)
    x[1] + x[2] / 60
  })

  ## calculate hour angle (15 degrees per hour from noon)
  (dcm - 12) * 15
}
