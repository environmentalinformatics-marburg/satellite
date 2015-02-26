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
#'
#' @return Satellite object with added, atmospheric corrected layers
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
#' @name satAtmosCorr
#'  
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrradTable(sat)
# 
NULL

# Function using Satellite object and tabulated values of eSun -----------------
#'
#' @param meta_param data frame containing the name of the metadata ID field 
#' used to merge with the readily existing metadata information of the
#' Satellite class object in the first column and with the same column header
#' as used in the existing metadata information. All other columns contain the
#' information to be added to the metadata.
#' 
#' @export satTOAIrradTable
#' 
#' @rdname satAtmosCorr
#' 
setMethod("satAtmosCorr", 
          signature(x = "Satellite"), 
          function(x, model, calc_esun){
            if(is.na(getSatESUN(x))){
              esd = getSatESD(x)
              if(is.na(esd)){
                esd = calcEartSunDist(date)
              }
              if(calc_esun == "Table"){
                esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                             normalize  = TRUE, esd = esd)
              } else if(calc_esun == "Model"){
                esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                          normalize = TRUE, esd = esd)
              } else if(calc_esun == "RadRef"){
                esun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                                           ref_max = getSatRefMax(x, getSatBCDESolar(x)),
                                           normalize = TRUE, esd = esd)
              }
              x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(esun),
                                                              ESUN = as.numeric(esun)))
            }
            
            path_rad <- calcPathRadDOS(DNmin, bnbr, band_wls, coefs, model = "DOS2", 
                                       ESun, scat_coef = -4.0, dos_adjust = 0.01)
            for(bnds in seq(5)){
              ref <- calcAtmosCorr(sensor_rad = getSatLayer(x, bnds), 
                                   path_rad = path_rad[i], 
                                   esun = getSatESUN(x, bnds),
                                   cos_szen = getSatSZEN(x, bnds), 
                                   model = "DOS2")
              x <- addSatLayer(ref)
#               x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(eSun),
#                                                                                         ESUN = as.numeric(eSun)))
                                        
            }
            return(x)
          })

# 
# # Function using Satellite object and modelled values of eSun ------------------
# #' 
# #' @export satTOAIrradModel
# #'
# #' @rdname satTOAIrrad
# #' 
# setMethod("satTOAIrradModel", 
#           signature(x = "Satellite"), 
#           function(x, model = "MNewKur", normalize = TRUE, date){
#             rsr <- lutInfoRSRromSID(sid = getSatSID(x))
#             if(missing(date)){
#               eSun <- calcTOAIrradModel(rsr = rsr, model = model, 
#                                         normalize = normalize)
#             } else {
#               eSun <- calcTOAIrradModel(rsr = rsr, model = model, 
#                                         normalize = normalize, date = date)
#             }
#             x <- addSatMetaParam(x, meta_param = data.frame(BCDE = names(eSun),
#                                                             ESUN = as.numeric(eSun)))
#             return(x)
#           })
# 
# 
# # Function using Satellite object and actual radiance and reflectance values ---
# #' 
# #' @export satTOAIrradRadRef
# #'
# #' @rdname satTOAIrrad
# #' 
# setMethod("satTOAIrradRadRef", 
#           signature(x = "Satellite"), 
#           function(x, normalize = TRUE, date){
#             if(missing(date)){
#               eSun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
#                                          ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
#                                          esd = getSatESD(x),
#                                          normalize = normalize)
#             } else {
#               eSun <- calcTOAIrradRadRef(rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
#                                          ref_max = getSatRefMax(x, getSatBCDESolar(x)),
#                                          esd = getSatESD(x),
#                                          normalize = normalize, date = date)
#             }
#             x <- addSatMetaParam(x, meta_param = data.frame(BCDE = getSatBCDESolar(x),
#                                                             ESUN = as.numeric(eSun)))
#             return(x)
#           })