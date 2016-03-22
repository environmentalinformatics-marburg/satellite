#' Compute hour angle and time from the solar zenith angle
#' 
#' @description 
#' Compute the solar hour angle and the time difference from noon associated 
#' therewith from the sun zenith angle.
#' 
#' @param theta \code{numeric} sun zenith angle in degrees (see Note). 
#' @param delta \code{numeric} angular declination of the sun in degrees.
#' @param phi \code{numeric} latitude in EPSG:4326 (see 
#' \url{http://spatialreference.org/ref/epsg/wgs-84/}).
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
fromSolarZenith <- function(theta, delta, phi) {
  
  ## convert angles to radians
  theta <- theta * pi / 180
  delta <- delta * pi / 180
  phi <- phi * pi / 180

  ## compute solar hour angle and convert back to degrees
  ## (taken from duffie and beckman 2013, p. 15)
  sha <- acos((cos(theta) - sin(phi) * sin(delta)) / (cos(phi) * cos(delta)))
  sha <- sha * 180 / pi
  
  ## decimal time (morning and afternoon)
  dcm <- numeric(2L)
  
  for (i in 1:2) {
    dcm[i] <- abs(sha / 15 + ifelse(i == 1, 12, -12))
  }

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

  data.frame("SolarHourAngle" = sha, "AM" = tms[2], "PM" = tms[1])
}