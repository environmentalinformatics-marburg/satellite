#' Atmospheric correction of remote sensing data
#'
#' @description
#' The function computes an atmospheric scattering correction and converts
#' the sensors digital numbers to reflectances using
#' - absolute radiance correction \cr
#' - DOS2: a dark object substraction model by Chavez (1996)
#' - DOS4: a dark object substratcion model by Moran et al. (1992)
#'
#' @param sensor_rad radiance at the sensor
#' @param path_rad path radiance, e.g. returned from \code{\link{calcPathRadDOS}}
#' @param eSun actual (i.e. non-normalized) TOA solar irradianc, e.g. returned 
#' from \code{\link{calcTOAIrradTable}}, \code{\link{calcTOAIrradModel}}, or
#' \code{\link{calcTOAIrradRadRef}} with normalization settings equal FALSE
#' @param szen sun zenith angle
#' @param model model to be used (DOS2, DOS4), must be the same as used for
#' \code{\link{calcPathRadDOS}}
#'
#' @return Raster object containing converted data.
#'
#' @export calcAtmosCorr
#' 
#' @details The radiometric correction is based on a dark object approach using
#' either the DOS2 (Chavez 1996) or DOS4 (Moran et al. 1992) model.
#' 
#' The minimum reflectance values for the dark object are identified using the
#' approximation of Chavez (1988, see \code{\link{calcPathRadDOS}} for details).
#' 
#' The estimated values of the solar irradiance required for the path radiance
#' can be computed by one of \code{\link{calcTOAIrradRadTable}} which is used to
#' get readily published values of ESun, \code{\link{calcTOAIrradRadRef}} which 
#' computes ESun based on the actual radiance and reflectance in the scene or  
#' \code{\link{calcTOAIrradModel}}, which computes ESun based on  look-up tables 
#' for the sensor's relative spectral resonse and solar irradiation spectral data.
#' 
#' The atmospheric transmittance towards the sensor (Tv) is approximated by 
#' 1.0 (DOS2, Chavez 1996) or rayleigh scattering (DOS4, Moran et al. 1992)
#' 
#' The atmospheric transmittance from the sun (Tz) is approximated by the 
#' cosine of the sun zenith angle (DOS2, Chavez 1996) or again using rayleigh
#' scattering (DOS4, Moran et al. 1992).
#' 
#' The downwelling diffuse irradiance is approximated by 0.0 (DOS2, Chavez 1996)
#' or the hemispherical integral of the path radiance (DOS4, Moran et al. 1992).
#' 
#' Equations are taken from Song et al. (2001).
#'  
#' @references Chavez Jr PS (1988) An improved dark-object subtraction technique 
#' for atmospheric scattering correction of multispectral data. Remote Sensing 
#' of Environment 24/3, doi:10.1016/0034-4257(88)90019-3, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788900193}
#'  
#' Chavez Jr PS (1996) Image-based atmospheric corrections revisited and
#' improved. Photogrammetric Engineering and Remote Sensing 62/9,
#' available online at 
#' \url{http://www.asprs.org/PE-RS-Journals-1996/PE-RS-September-1996.html}
#'  
#' Goslee SC (2011) Analyzing Remote Sensing Data in R: The landsat 
#' Package. Journal of Statistical Software,43/4, 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' Moran MS, Jackson RD, Slater PN, Teillet PM (1992) Evlauation of simplified
#' procedures for rretrieval of land surface reflectane factors from satellite
#' sensor output.Remote Sensing of Environment 41/2-3, 169-184, 
#' doi:10.1016/0034-4257(92)90076-V, 
#' URL \url{http://www.sciencedirect.com/science/article/pii/003442579290076V}.
#' 
#' Song C, Woodcock CE, Seto KC, Lenney MP, Macomber SA (2001) Classification 
#' and Change Detection Using Landsat TM Data: When and How to Correct 
#' Atmospheric Effects? Remote Sensing of Environment 75/2, 
#' doi:10.1016/S0034-4257(00)00169-3, URL
#' \url{http://www.sciencedirect.com/science/article/pii/S0034425700001693}
#' 
#' @seealso \code{\link{satAtmosCorr}} which can be used as a wrapper function 
#' if the data is organized as a Satellite object.
#'
#' @examples
#' not run:
#' calcAtmosCorr(filepath = "Name_of_Landsat_Metadata_File")

calcAtmosCorr <- function(sensor_rad, path_rad, esun, szen, model = "DOS2"){
  cos_szen <- cos(szen * pi / 180.0)
  if(model == "DOS2"){
    tv <- 1.0
    tz <- cos_szen
    edown <- 0.0
  } else if(model == "DOS4"){
    tv <- NA
    tz <- NA
    edown <- NA
  }
  
  ref <- pi * (sensor_rad - path_rad) / 
    ( tv * (esun * cos_szen * tz + edown) )
  
  return(ref)
}
 