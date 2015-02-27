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
#' @details 
#' If necessary, TOA solar irradiance is computed using 
#' \code{\link{satTOAIrrad}}.
#' 
#' Atmospheric correction is computed using \code{\link{calcAtmosCorr}}. 
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
            if(is.na(getSatESUN(x))){
              esd = getSatESD(x)
              if(is.na(esd)){
                esd = calcEartSunDist(getSatDate(x))
                x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(getSatBCDE(x)),
                                                                ESD = as.numeric(esd)))
              }
            }
            
            # Take care of TOA solar irradiance information
            if(any(is.na(getSatESUN(x, getSatBCDESolar(x))))){
              # Compute toa irradiance for all solar band layers
              sc_solar <- getSatBCDESolar(x)
              if(esun_mode == "Table"){
                esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                             normalize  = TRUE, esd = esd)
              } else if(esun_mode == "Model"){
                esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                          normalize = TRUE, esd = esd)
              } else if(esun_mode == "RadRef"){
                esun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, sc_solar), 
                                           ref_max = getSatRefMax(x, sc_solar),
                                           normalize = TRUE, esd = esd)
              }
              x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(esun),
                                                              ESUN = as.numeric(esun)))
            }
            
            
            # Get solar bands with calibration information equals scaled counts
            sc_bands <- getSatBCDESolarCalib(x, id = "SC")
            
            # Take care of dark object values
            bcde <- "B002n"
            dn_min <- min(raster::getValues(getSatDataLayer(x, bcde)))
              
            # Take care of path radiance
            path_rad <- calcPathRadDOS(DNmin = dn_min,
                                       bnbr = getSatLNBR(x, bcde),
                                       band_wls = data.frame(LMIN = getSatLMIN(x, sc_bands), 
                                                             LMAX = getSatLMAX(x, sc_bands)),
                                       radm = getSatRADM(x, sc_bands),
                                       rada = getSatRADA(x, sc_bands),
                                       szen = getSatSZEN(x, sc_bands),
                                       esun = getSatESUN(x, sc_bands),
                                       model = "DOS2")
            x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(path_rad),
                                                            PRAD = as.numeric(path_rad)))
            
            # Compute atmospheric correction (reflectance)
            for(bcde in sc_bands){
              ref <-calcAtmosCorr(sensor_rad = getSatDataLayer(x, bcde),
                                  path_rad = getSatPRAD(x, bcde),
                                  esun = getSatESUN(x, bcde),
                                  szen = getSatSZEN(x, bcde), 
                                  model = "DOS2")
              layer_bcde <- paste0(bcde, "_ref_cA")
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = layer_bcde, data = ref,
                                   info = info,
                                   in_bcde = bcde)
              #               x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(path_rad),
              #                                                               PRAD = as.numeric(path_rad)))                                        
            }
            return(x)
          })
test <- function(x, y, z){
  print(x)
  sys.call()
}
