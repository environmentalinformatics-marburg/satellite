if ( !isGeneric("satAtmosCorr") ) {
  setGeneric("satAtmosCorr", function(x, ...)
    standardGeneric("satAtmosCorr"))
}
#' Calculate an atmospheric correction for the layers of a Satellite object
#'
#' @description
#' The function computes an atmospheric scattering correction and converts
#' the sensors digital numbers to reflectances using
#' - absolute radiance correction \cr
#' - DOS2: a dark object substraction model by Chavez (1996)
#' - DOS4: a dark object substratcion model by Moran et al. (1992)
#' 
#' If not passed to the function, the necessary estimates of the TOA solar
#' irradiance are computed, too.
#'
#' @param x object of type Satellite
#' @param model model model to be used (DOS2, DOS4)
#' @param calc_esun method used for computation of eSun if not already part of
#' the Satellite object's metadata (Table, Model, RadRef)
#' @param meta_param data frame containing the name of the metadata ID field 
#' used to merge with the readily existing metadata information of the
#' Satellite class object in the first column and with the same column header
#' as used in the existing metadata information. All other columns contain the
#' information to be added to the metadata.
#'
#' @return Satellite object with added, atmospheric corrected layers
#' 
#' @export satAtmosCorr
#' 
#' @name satAtmosCorr
#'
#' @details 
#' Atmospheric correction is computed using \code{\link{calcAtmosCorr}}. 
#' 
#' If necessary (i.e. Satellite object does not contain the respective 
#' information) the following is computed before the actual atmospheric
#' correction:
#' 
#' The TOA solar irradiance is computed using \code{\link{satTOAIrrad}}.
#' 
#' The path radiance is computed based on a dark object's DN using 
#' \code{\link{calcPathRadDOS}}.
#' 
#' The bands' DNs are converted to radiance using \code{\link{satCalib}}.
#' 
#' Please refer to the respective functions for details on the computation.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{satTOAIrrad}} which is used to get TOA solar irradiance if 
#' nesessary and \code{\link{calcAtmosCorr}} which corrects the sensor data for 
#' atmospheric influences.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradTable(sat)
#' 
setMethod("satAtmosCorr", 
          signature(x = "Satellite"), 
          # Take care of earth sun distance information
          function(x, atmos_model = "DOS2", esun_mode = "RadRef"){
            
            # Take care of TOA solar irradiance calculation if necessary
            if(any(is.na(getSatESUN(x, getSatBCDESolar(x))))){
              # Compute toa irradiance for all solar band layers
              if(esun_mode == "Table"){
                x <- satTOAIrrad(x, method = "Table", normalize = TRUE)
              } else if(esun_mode == "Model"){
                x <- satTOAIrrad(x, method = "Model", model = "MNewKur", 
                                 normalize = TRUE)
              } else if(esun_mode == "RadRef"){
                x <- satTOAIrrad(x, method = "RadRef", normalize = "TRUE")
              }
            }
            
            
            # Get solar bands with calibration information equals scaled counts
            sc_bands <- getSatBCDESolarCalib(x, id = "SC")
            
            # Take care of path radiance computation if necessary
            if(any(is.na(getSatPRAD(x, sc_bands)))){
              x <- satPathRadDOS(x, atmos_model = "DOS2")
            }
            
            # Take care of radiance calibration if necessary
            if(any(is.na(getSatBCDESolarCalib(x, id = "RAD")))){
              x <- satCalib(x, convert = "Rad")
            }
            
            # Compute atmospheric correction (reflectance)
            rad_bands <- getSatBCDESolarCalib(x, id = "RAD")
            for(bcde_rad in rad_bands){
              ref <-calcAtmosCorr(sensor_rad = getSatDataLayer(x, bcde_rad),
                                  path_rad = getSatPRAD(x, bcde_rad),
                                  esun = getSatESUN(x, bcde_rad),
                                  szen = getSatSZEN(x, bcde_rad), 
                                  model = "DOS2")
              layer_bcde <- paste0(substr(bcde_rad, 1, nchar(bcde_rad) - 4),
                                   "_REF_AtmosCorr")
              meta_param <- data.frame(getSatSensorInfo(x),
                                       getSatBandInfo(x, bcde_rad, 
                                                      return_calib = FALSE),
                                       CALIB = "REF_AtmosCorr")
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = layer_bcde, data = ref,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde_rad)
            }
            return(x)
          })
