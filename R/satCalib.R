# if ( !isGeneric("satCalib") ) {
#   setGeneric("satCalib", function(x, ...)
#     standardGeneric("satCalib"))
# }
# #' Calibrate SC of Satellite object to radiance, reflectance and/or temperature
# #'
# #' @description
# #' Calibrate the scalled counts of a Satellite object to radiance, reflectance
# #' and or brightness temperature, using standard calibration procedures without
# #' atmospheric correction etc.
# #' 
# #' @param x object of type Satellite
# #' @param method name of the method to be used ("Table", "Model", "RadRef)
# #' @param model name of the model to be used if method is "Model"
# #' (see \code{\link{calcTOAIrradModel}})
# #' @param normalize normalize ESun to mean earth sun distance
# #' @param esd earth-sun distance (AU), if not supplied and necessary for
# #' normalize, it is tried to take it from the metadata, otherwise it is estimated
# #' by the day of the year using \code{\link{calcEartSunDist}}.
# #' 
# #' @return Satellite object with added data layers for the respective conversions
# #' 
# #' @export satCalib
# #' 
# #' @name satCalib
# #'
# #' @details 
# #' The calibration is computed using \code{\link{calibLinear}}. 
# #' 
# #' Please refer to the respective functions for details on the computation.
# #' 
# #' @references Please refer to the respective functions for references.
# #'  
# #' @seealso This function is a wrapper for \code{\link{calibLinear}}.
# #' 
# #' @examples
# #' satPathRadDOSModel(sat)
# #' path <- system.file("extdata", package = "satellite")
# #' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
# #' sat <- satellite(files)  
# #' satPathRadDOSRadRef(sat)
# # 
# setMethod("satCalib", 
#           signature(x = "Satellite"), 
#           function(x, convert = "all", szen_correction = "TRUE"){
#             if(convert == "all"){
#               convert <- c("Rad", "Ref", "BT")
#             }
#             
#             if("Rad" %in% convert){
#               band_codes <- getSatBCDECalib(x, id = "SC")
#               for(bcde in band_codes){
#                 if(!is.na(getSatRADM(x, bcde))){
#                   sensor_rad <- calibLinear(band = getSatDataLayer(x, bcde),
#                                             mult = getSatRADM(x, bcde),
#                                             add = getSatRADA(x, bcde))
#                   layer_bcde <- paste0(bcde, "_RAD")
#                   
#                   meta_param <- getSatMetaBCDETemplate(x, bcde)
#                   meta_param$BCDE <- layer_bcde
#                   meta_param$CALIB <- "RAD"
#                   
#                   info <- sys.calls()[[1]]
#                   info <- paste0("Add layer from ", info[1], "(", 
#                                  toString(info[2:length(info)]), ")")
#                   
#                   x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_rad,
#                                        meta_param = meta_param,
#                                        info = info, in_bcde = bcde)
#                 }
#               }
#             }
#             
#             if("Ref" %in% convert){
#               band_codes <- getSatBCDESolarCalib(x, id = "SC")
#               for(bcde in band_codes){
#                 if(!is.na(getSatREFM(x, bcde))){
#                   if(szen_correction == TRUE){
#                     szen <- getSatSZEN(x, bcde)
#                     sensor_ref <- calibLinear(band = getSatDataLayer(x, bcde),
#                                               mult = getSatREFM(x, bcde),
#                                               add = getSatREFA(x, bcde),
#                                               szen = szen)
#                     calib = "REF"
#                   } else {
#                     sensor_ref <- calibLinear(band = getSatDataLayer(x, bcde),
#                                               mult = getSatREFM(x, bcde),
#                                               add = getSatREFA(x, bcde))
#                     calib = "REF_NoSZEN"
#                   }
#                   layer_bcde <- paste0(bcde, "_", calib)
#                   
#                   meta_param <- getSatMetaBCDETemplate(x, bcde)
#                   meta_param$BCDE <- layer_bcde
#                   meta_param$CALIB <- calib
#                   
#                   info <- sys.calls()[[1]]
#                   info <- paste0("Add layer from ", info[1], "(", 
#                                  toString(info[2:length(info)]), ")")
#                   
#                   x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_ref,
#                                        meta_param = meta_param,
#                                        info = info, in_bcde = bcde)
#                 }
#               }
#             }
#             
#             if("BT" %in% convert){
#               band_codes <- getSatBCDEThermalCalib(x, id = "SC")
#               for(bcde in band_codes){
#                 if(!any(is.na(getSatRADM(x, bcde)), is.na(getSatBTK1(x, bcde)))){
#                   sensor_ref <- calibLinear(band = getSatDataLayer(x, bcde),
#                                             mult = getSatRADM(x, bcde),
#                                             add = getSatRADA(x, bcde),
#                                             k1 = getSatBTK1(x, bcde),
#                                             k2 = getSatBTK2(x, bcde))
#                   layer_bcde <- paste0(bcde, "_BT")
#                   
#                   meta_param <- getSatMetaBCDETemplate(x, bcde)
#                   meta_param$BCDE <- layer_bcde
#                   meta_param$CALIB <- "BT"
#                   
#                   info <- sys.calls()[[1]]
#                   info <- paste0("Add layer from ", info[1], "(", 
#                                  toString(info[2:length(info)]), ")")
#                   
#                   x <- addSatDataLayer(x, bcde = layer_bcde, data = sensor_ref,
#                                        meta_param = meta_param,
#                                        info = info, in_bcde = bcde)
#                 }
#               }
#             }
#             return(x)
#           })
#           