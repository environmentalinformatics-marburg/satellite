#' Get extraterrestrial solar irradiance (ESun) for satellite bands
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, mean solar spectral data and the band 
#' specifiv relative spectral response functions (rsr) are used.
#' 
#' For Landsat 8, no tabulated values are availabe. Instead (and if tab = TRUE)
#' ESun will be calculated based on the actual maximum radiance and reflection 
#' given in the metadata file. Otherwise they will be computed using the 
#' approach discribed above.
#'
#' @param sensor sensor name ("Landsat 8/7/5/4")
#' @param tab use tabulated or measured (Landsat 8) values (TRUE) or compute 
#' values based on rsr (FALSE)
#' @param normalize normalize ESun to mean earth sun distance
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object), only 
#' relevant if normalize = FALSE
#' @param rsr Landsat 8 rsr (see \code{\link{toaIrradianceModel}} for details)
#'
#' @return vector object containing ESun for each band
#'
#' @export eSun
#' 
#' @details 
#' Tabulated values of ESun are taken from the official reference handbooks or 
#' peer-review publications using function \code{\link{toaIrradianceTable}}. 
#' 
#' Instead of returning tabulated values for Landsat 8 which are not available
#' in the official handbook, \code{\link{toaIrradianceRadRef}} is used to 
#' compute the actual eSun value based on the scene's metadata. 
#' 
#' If ESun should be computed (all sensors), \code{\link{toaIrradianceModel}} 
#' will be called by this function.
#' 
#' If eSun should be corrected for the actual earth sun distance, an
#' approximation of this distance is computed based on the day of by
#' \code{\link{earthSun}}. For Landsat 8, the respective earth sun distance is
#' taken from the metadata of the scene.
#' 
#' @references For references of the data sources, please refer to the 
#' documentation of the respective functions given in details or see also.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{toaIrradianceTable}} which is used to get readily published 
#' values of ESun, \code{\link{toaIrradianceRadRef}} which computes ESun based 
#' on the actual radiance and reflectance in the scene and for 
#' \code{\link{toaIrradianceModel}} which computes ESun based on  
#' look-up tables for the sensor's relative spectral resonse and solar 
#' irradiation spectral data.
#'  
#' @examples
#' landsat8_metadatafile <-   system.file("extdata", 
#' "LC81950252013188LGN00_MTL.txt", package = "satellite")
#' coefs8 <- collectLandsat8Metadata(landsat8_metadatafile)
#' eSun(sensor = "Landsat 8", tab = TRUE, rsr = lut$l8_rsr)
#' 
eSun <- function(sensor, tab = TRUE, normalize = TRUE, rsr, date){
  if(tab == TRUE){
    if(sensor == "Landsat 8"){
      if(missing(rsr)){
        stop("Variable rsr is missing.")
      }
      # todo: replace by toaIrradianceRadRef
      eSun <- toaIrradianceModel(rsr, normalize = normalize)
    } else {
      if(normalize == TRUE){
        eSun <- toaIrradianceTable(sensor = sensor, normalize = normalize)  
      } else {
        if(missing(date)){
          stop("Variable date is missing.")
        }
        eSun <- toaIrradianceTable(sensor = sensor, normalize = normalize,
                                   date = date)  
      }
    }
  } else {
    if(normalize == TRUE){
      eSun <- toaIrradianceModel(rsr, normalize = normalize) 
    } else {
      if(missing(date)){
        stop("Variable date is missing.")
      }
      eSun <- toaIrradianceModel(rsr, normalize = normalize,
                                 date = date)  
    }
  }
  return(eSun)
}