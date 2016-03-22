if ( !isGeneric("fromSolarZenith") ) {
  setGeneric("fromSolarZenith", function(theta, ...)
    standardGeneric("fromSolarZenith"))
}
#' Compute hour angle and time from the solar zenith angle
#' 
#' @description 
#' Compute the solar hour angle and the time difference from noon associated 
#' therewith from the sun zenith angle.
#' 
#' @param theta \code{numeric} or \code{Raster*} object, sun zenith angle in 
#' degrees (see Note). 
#' @param phi \code{numeric} or \code{Raster*} object, latitude in EPSG:4326 
#' (see \url{http://spatialreference.org/ref/epsg/wgs-84/}).
#' @param n \code{Date} object from which to extract the day of the year (DOY), 
#' passed on to \code{\link[satellite]{declination}}.
#' @param formula \code{character}, formula from which to calculate the angular 
#' declination of the sun, see \code{\link[satellite]{declination}}.
#' @param daytime \code{character}, possible options are "AM" (i.e., morning 
#' scene) and "PM".
#' @param origin A \code{POSIX*} object that defines the origin of \code{x}, see 
#' \code{\link{solarHourAngle}}.
#' @param ... If \code{theta} is a \code{Raster*} object, further arguments 
#' passed on to \code{\link{writeRaster}}.
#' 
#' @note 
#' Negative and positive solar hour angles indicate solar zenith angles during 
#' the morning and afternoon, respectively. Note that the sun zenith angle can 
#' easily be derived from the elevation angle through 
#' \code{sun zenith = 1 - elevation}. 
#' 
#' @references 
#' PVEducation (2016) Elevation angle. Available online at 
#' \url{http://www.pveducation.org/pvcdrom/properties-of-sunlight/elevation-angle}.
#' 
#' @seealso 
#' \code{\link[satellite]{declination}}, \code{\link{solarHourAngle}}.
#' 
#' @examples 
#' theta <- 30 # 30 deg zenith angle
#' delta <- declination(Sys.Date(), "Spencer", "degrees") # angular declination
#' phi <- 0    # geographic latitude, i.e. equator
#' 
#' fromSolarZenith(theta, delta, phi) 
#' 
#' @export fromSolarZenith
#' @name fromSolarZenith
NULL

### 'numeric' method ----- 
#' @aliases fromSolarZenith,numeric-method
#' @rdname fromSolarZenith
setMethod("fromSolarZenith", 
          signature(theta = "numeric"), 
          function(theta, phi, n, formula = c("Cooper", "Spencer")) {
            
  
  ## convert angles to radians
  theta <- theta * pi / 180
  delta <- declination(n, formula, "radians")
  phi <- phi * pi / 180
  
  ## compute solar hour angle and convert back to degrees
  ## (taken from duffie and beckman 2013, p. 15)
  sha <- acos((cos(theta) - sin(phi) * sin(delta)) / (cos(phi) * cos(delta)))
  sha <- sha * 180 / pi
  
  ## decimal time (morning and afternoon)
  dcm <- numeric(2L)
  
  lst <- lapply(sha, function(h) {
    for (i in 1:2) 
      dcm[i] <- abs(h / 15 + ifelse(i == 1, 12, -12))
    
    ## create decimal minutes
    tms <- sapply(strsplit(as.character(dcm), "\\."), function(x) {
      
      # full hour
      if (length(x) == 1) {
        out <- paste0(formatC(x, width = 2, flag = 0), ":00")
        
        # no full hour  
      } else {
        x[2] <- paste(substr(x[2], 1, 2), substr(x[2], 3, nchar(x[2])), sep = ".")
        x <- as.numeric(x)
        
        out <- paste0(formatC(x[1], width = 2, flag = 0), ":",
                      formatC(round(x[2] / 100 * 60), width = 2, flag = 0))
        
        # round up to the next full hour is rounded minutes equal '60'
        if (substr(out, 4, 5) == "60") {
          out <- gsub("60", "00", out)
          out <- gsub(substr(out, 1, 2), formatC(as.integer(substr(out, 1, 2)) + 1, 
                                                 width = 2, flag = 0), out)
        }
      }
      
      return(out)
    })
    
    data.frame("SolarHourAngle" = h, "AM" = tms[2], "PM" = tms[1], 
               stringsAsFactors = FALSE)
  })
  
  do.call("rbind", lst)
})
            
### 'Raster' method ----- 
#' @aliases fromSolarZenith,Raster-method
#' @rdname fromSolarZenith
setMethod("fromSolarZenith", 
          signature(theta = "Raster"), 
          function(theta, phi, n, formula = c("Cooper", "Spencer"), 
                   daytime = c("AM", "PM"), 
                   origin = strptime("1993-01-01", "%Y-%m-%d"), ...) {
            
            raster::overlay(theta, phi, fun = function(x, y) {
              tms <- fromSolarZenith(x[], y[], n, formula)
              tms <- tms[, daytime]
              
              dts <- paste(n, tms)
              dts <- strptime(dts, "%Y-%m-%d %H:%M")
              
              difftime(dts, origin, units = "secs")
            }, unstack = TRUE, ...)                
          })