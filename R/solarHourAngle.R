if ( !isGeneric("solarHourAngle") ) {
  setGeneric("solarHourAngle", function(x, ...)
    standardGeneric("solarHourAngle"))
}
#' Compute solar hour angle
#' 
#' @description 
#' Compute the solar hour angle which is defined as the time difference from 
#' noon times 15 degrees per hour (i.e., negative during the morning and 
#' positive during the afternoon). 
#' 
#' @param x A date-time object of class \code{POSIX*}, or a \code{character} 
#' object that can be coerced by \code{\link{strptime}}, or a \code{Raster*} 
#' object holding such information.
#' @param origin A \code{POSIX*} or \code{character} object that defines the 
#' starting point of \code{x}, defaults to the origin of MOD05_L2 
#' 'Scan_Start_Time' (midnight of 1 January 1993, see 
#' \url{http://modis-atmos.gsfc.nasa.gov/MOD05_L2/format.html}).
#' @param ... If \code{x} is a \code{character} object, formatting arguments 
#' passed on to \code{\link{strptime}}, or if \code{x} is a \code{Raster*} 
#' object, formatting arguments related with the specified \code{origin}.
#' @param unit \code{character}, determines whether to return the hour angle in 
#' radians (default) or degrees.
#' 
#' @return 
#' \code{numeric} solar hour angle(s), or if \code{x} is a \code{Raster*} 
#' object, a \code{Raster*} object with pixel-wise solar hour angles.
#'
#' @examples 
#' ## current solar hour angle
#' solarHourAngle(Sys.time(), unit = "degrees")
#' 
#' @name solarHourAngle
#' @export solarHourAngle
NULL

### 'POSIXt' method ----- 
#' @aliases solarHourAngle,POSIXt-method
#' @rdname solarHourAngle
setMethod("solarHourAngle", 
          signature(x = "POSIXt"), 
          function(x, unit = c("radians", "degrees")) {
            
            ## convert to decimal hours
            dtm <- strftime(x, "%H:%M")
            dcm <- sapply(strsplit(dtm, ":"), function(x) {
              x <- as.numeric(x)
              x[1] + x[2] / 60
            })
            
            ## calculate hour angle (15 degrees per hour from noon)
            sha <- (dcm - 12) * 15
            
            ## convert to degrees (optional) and return
            if (unit[1] == "radians")
              return(sha * pi / 180)
            else
              return(sha)
          })

### 'character' method -----
#' @aliases solarHourAngle,character-method
#' @rdname solarHourAngle
setMethod("solarHourAngle", 
          signature(x = "character"), 
          function(x, unit = c("radians", "degrees"), ...) {
            
            ## convert to 'POSIXt' object
            x <- strptime(x, ...)
            
            ## calculate solar hour angle
            solarHourAngle(x, unit)
          })

### 'Raster' method -----
#' @aliases solarHourAngle,Raster-method
#' @rdname solarHourAngle
setMethod("solarHourAngle", 
          signature(x = "Raster"), 
          function(x, origin = strptime("1993-01-01", "%Y-%m-%d"), 
                   unit = c("radians", "degrees"), ...) {
            
            ## convert 'origin' to 'POSIXt'
            if (!any(class(origin) == "POSIXt"))
              origin <- strptime(origin, ...)
            
            ## calculate pixel-wise hour angle
            raster::calc(x, fun = function(y) {
              solarHourAngle(origin + y, unit)
            })
          })
