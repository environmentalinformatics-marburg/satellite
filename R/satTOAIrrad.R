if ( !isGeneric("satTOAIrrad") ) {
  setGeneric("satTOAIrrad", function(x, ...)
    standardGeneric("satTOAIrrad"))
}
#' Get TOA solar irradiance (ESun) for the layers of a Satellite object
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, either (i) the mean solar spectral data and 
#' the band specifiv relative spectral response functions (rsr) are used or (ii)
#' actual maximum radiance and reflection values are taken.
#' 
#' @param x object of type Satellite
#' @param method name of the method to be used ("Table", "Model", "RadRef)
#' @param model name of the model to be used if method is "Model"
#' (see \code{\link{calcTOAIrradModel}})
#' @param normalize normalize ESun to mean earth sun distance
#' @param esd earth-sun distance (AU), if not supplied and necessary for
#' normalize, it is tried to take it from the metadata, otherwise it is estimated
#' by the day of the year using \code{\link{calcEartSunDist}}.
#' 
#' @return Satellite object with ESun for each band in the metadata
#' 
#' @export satTOAIrrad
#' 
#' @name satTOAIrrad
#'
#' @details 
#' Tabulated values of ESun are compiled using \code{\link{calcTOAIrradRadTable}}. 
#' 
#' Actual values of ESun are computed using \code{\link{calcTOAIrradRadRef}}.
#' 
#' Modeled values of ESun are computed using \code{\link{calcTOAIrradModel}}.
#' 
#' If eSun should be corrected for the actual earth sun distance, the distance 
#' is taken from (i) the function parameter, (ii) the metadata or (iii) 
#' approximation using \code{\link{calcEartSunDist}} which is called from
#' within the functions above.
#' 
#' Please refer to the respective functions for details on the computation.
#' 
#' @references Please refer to the respective functions for references.
#'  
#' @seealso This function is a wrapper for 
#' \code{\link{calcTOAIrradRadTable}}, \code{\link{calcTOAIrradRadRef}} and 
#' \code{\link{calcTOAIrradModel}}.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrrad(sat, method = "Table")
#' 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satTOAIrrad(sat, method = "Model")
#' 
#' satTOAIrradModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' satTOAIrrad(sat, method = "RadRef")
# 
setMethod("satTOAIrrad", 
          signature(x = "Satellite"), 
          function(x, method = "Table", model = "MNewKur", 
                   normalize = TRUE, esd){
            
            if((method != "RadRef" & normalize == FALSE & missing(esd)) |
                 (method == "RadRef" & normalize == TRUE & missing(esd))){
              esd = getSatESD(x)
              if(is.na(esd)){
                esd = calcEartSunDist(date)
              } 
            }
            
            if(method == "Table"){
              if(normalize == TRUE){
                esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                             normalize  = normalize)
              } else {
                esun <- calcTOAIrradRadTable(sid = getSatSID(x), 
                                             normalize  = normalize, 
                                             esd = esd)
              }
              bcde = names(esun)
            } else if(method == "Model"){
              rsr <- lutInfoRSRromSID(sid = getSatSID(x))
              if(normalize == TRUE){
                esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                          normalize = normalize)
              } else {
                esun <- calcTOAIrradModel(rsr = rsr, model = model, 
                                          normalize = normalize, esd = esd)
              }
              bcde = names(esun)
            } else if(method == "RadRef"){
              if(normalize == TRUE){
                esun <- 
                  calcTOAIrradRadRef(
                    rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                    ref_max = getSatRefMax(x, getSatBCDESolar(x)),
                    esd = esd, normalize = normalize)
              } else {
                esun <- 
                  calcTOAIrradRadRef(
                    rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                    ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
                    normalize = normalize)
              }
              bcde = getSatBCDESolar(x)
            }
            x <- addSatMetaParam(x, 
                                 meta_param = data.frame(
                                   BCDE = bcde,
                                   ESUN = as.numeric(esun)))
            return(x)
          })
