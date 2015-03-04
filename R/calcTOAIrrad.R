if ( !isGeneric("calcTOAIrrad") ) {
  setGeneric("calcTOAIrrad", function(x, ...)
    standardGeneric("calcTOAIrrad"))
}
#' Calculate or look-up TOA solar irradiance (ESun) for the sensor bands
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, either (i) the mean solar spectral data and 
#' the band specifiv relative spectral response functions (rsr) are used or (ii)
#' actual maximum radiance and reflection values are taken.
#' 
#' @param x Satellite, Raster*, or numeric object of the sensor's band values
#' @param method name of the method to be used ("Table", "Model", "RadRef)
#' @param model name of the model to be used if method is "Model"
#' (one of "MCebKur_MChKur", "MNewKur", "MthKur", "MoldKur", "MODWherli_WMO")
#' @param rsr relative spectral response function (see details for structure)
#' @param rad_max maximum radiance of satellite band(s)
#' @param rad_min minimum radiance of satellite band(s)
#' @param normalize normalize ESun to mean earth sun distance
#' @param esd earth-sun distance (AU), if not supplied and necessary for
#' normalize, it is tried to take it from the metadata, otherwise it is estimated
#' by the day of the year using \code{\link{calcEartSunDist}}.
#' 
#' @return Satellite object with ESun for each band in the metadata
#' 
#' @name calcTOAIrrad
#'
#' @details 
#' Table-based retrieval of TOA irradiance is currently implemented for 
#' Landsat 4, 5 and 7. For the data sources, see references.
#' 
#' Model-based computation of TOA irradiance is based on the formula taken from 
#' Updike and Comp (2011).
#' 
#' Relative spectral response values have to be supplied as a as a data frame
#' which has at least these three columns: (i) a column "Band" for the sensor
#' band number (i.e. 1, 2, etc.), (ii) a column "WAVELENGTH" for the WAVELENGTH 
#' data in full nm steps, and (iii) a column "RSR" for the 
#' response information [0...1].
#' 
#' Tabulated values for mean solar irradiance are taken from the National 
#' Renewable Energy Laboratory. 
#' 
#' Actual values of the solar irradiance are compute using the following formula 
#' taken from GRASS' 
#' \href{http://grass.osgeo.org/grass65/manuals/i.landsat.toar.html}{i.landsat.toar module}
#' \deqn{ESun = (pi d^2) RADIANCE_MAXIMUM / REFLECTANCE_MAXIMUM}
#' where d is the sun-earth distance in astronomical units and RADIANCE_MAXIMUM,
#' REFLECTANCE_MAXIMUM are the maximum radiance and reflection values of the
#' respective band. All these parameters are taken from the scene's metadata
#' file.
#' 
#' By default, the resulting actual ESun will be normalized to a mean earth sun 
#' distance to be compatible with other default results (see next paragraph).
#' 
#' If results should not be normalized to a mean earth sun distance, the 
#' actual earth sun distance is approximated by the day of the year using
#' \code{\link{calcEartSunDist}}.
#' 
#' The TOA irradiance looked-up or computed by the look-up table or model 
#' approach is valid for a mean earth sun distance. If it should be corrected
#' for the actual earth sun distance, the latter is approximated by the day
#' of the year if not supplied explicitly or in the metadata of a Satellite
#' object.
#' 
#' In contrast, the TOA irradiance computed by the maximum radiance to 
#' reflectance ratio is valid for the actual earth sun distance. Hence, if it
#' should be corrected for a mean earth sun distance, the latter is again
#' approximated by the day of the year if not supplied otherwise.
#' 
#' For the approximation of the earth sun distance based on the day of the year
#' see \code{\link{calcEartSunDist}}.
#' 
#' @references
#' Updike T, Comp C (2011) Radiometric use of WorldView-2 imagery. 
#' Technical Note, URL 
#' \url{https://www.digitalglobe.com/sites/default/files/Radiometric_Use_of_WorldView-2_Imagery(1).pdf}.
#' 
#' Tabulated values of the solar irradiance for the table-based approach 
#' for Landsat 4 and 5 are taken from Chander G, Markham B (2003) Revised 
#' Landsat-5 TM radiometric calibration procedures and postcalibration dynamic 
#' ranges.  IEEE Transaction on Geoscience and Remote Sensing 41/11, 
#' doi:10.1109/LGRS.2007.898285, URL
#' \url{http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf}.
#' 
#' Tabulated values of the solar irradiance for the table-based approach for 
#' Landsat 7 are taken from 
#' \href{http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf}{NASA's
#' Landsat7 handbook, tab 11.3 (Thuillier spectrum)}
#' 
#' Tabulated relative spectral response functions (nm-1) for the model approach 
#' are taken from \href{http://landsat.usgs.gov/instructions.php}{spectral viewer}
#' of the USGS Landsat FAQ.
#' 
#' Tabulated solar irradiance (W m-2 nm-1) for the model approach is taken from
#' \href{http://rredc.nrel.gov/solar/spectra/am0/modtran.html}{National Renewable 
#' Energy Laboratory}.
#' 
#' See \code{\link{calcEartSunDist}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#'  
#'  
#' @seealso \code{\link{calcEartSunDist}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' TOA irradiance should be corrected for actual earth sun distance.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' calcTOAIrrad(sat, method = "Table")
#' 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' calcTOAIrrad(sat, method = "Model")
#' 
#' .calcTOAIrradModel(sat)
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' calcTOAIrrad(sat, method = "RadRef")
# 
NULL

# Function for Satellite objects -----------------------------------------------
#' @rdname calcTOAIrrad
#' @export calcTOAIrrad
setMethod("calcTOAIrrad", 
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
#               if(normalize == TRUE){
#                 esun <- .calcTOAIrradRadTable(sid = getSatSID(x), 
#                                              normalize  = normalize)
#               } else {
                esun <- .calcTOAIrradRadTable(sid = getSatSID(x), 
                                             normalize  = normalize, 
                                             esd = esd)
#               }
              bcde = names(esun)
            } else if(method == "Model"){
              rsr <- lutInfoRSRromSID(sid = getSatSID(x))
#               if(normalize == TRUE){
#                 esun <- .calcTOAIrradModel(rsr = rsr, model = model, 
#                                           normalize = normalize)
#               } else {
                esun <- .calcTOAIrradModel(rsr = rsr, model = model, 
                                          normalize = normalize, esd = esd)
#               }
              bcde = names(esun)
            } else if(method == "RadRef"){
#               if(normalize == TRUE){
                esun <- 
                  .calcTOAIrradRadRef(
                    rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
                    ref_max = getSatRefMax(x, getSatBCDESolar(x)),
                    esd = esd, normalize = normalize)
#               } else {
#                 esun <- 
#                   .calcTOAIrradRadRef(
#                     rad_max = getSatRadMax(x, getSatBCDESolar(x)), 
#                     ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
#                     normalize = normalize)
#               }
              bcde = getSatBCDESolar(x)
            }
            x <- addSatMetaParam(x, 
                                 meta_param = data.frame(
                                   BCDE = bcde,
                                   ESUN = as.numeric(esun)))
            return(x)
          })


# Function for Raster objects --------------------------------------------------
#' @rdname calcTOAIrrad
#' @export calcTOAIrrad
setMethod("calcTOAIrrad", 
          signature(x = "Raster"), 
          function(x, sid, method = "Table", model = "MNewKur", 
                   normalize = TRUE, esd, rad_max, ref_max){
            
            if(method == "Table"){
              esun <- .calcTOAIrradRadTable(sid = sid, 
                                            normalize  = normalize, esd = esd)
            } else if(method == "Model"){
              rsr <- lutInfoRSRromSID(sid = sid)
              esun <- .calcTOAIrradModel(rsr = rsr, model = model, 
                                         normalize = normalize, esd = esd)
            } else if(method == "RadRef"){
              esun <- 
                .calcTOAIrradRadRef(
                  rad_max = rad_max, 
                  ref_max = ref_max,
                  esd = esd, normalize = normalize)
            }
            return(x)
          })



# Base function used for retrieving table-based TOA irradiance -----------------
.calcTOAIrradRadTable <- function(sid, normalize = TRUE, esd){
  if(sid == "LE7") {
    eSun <- lut$L7_ESUN
  } else if(sid == "LE5") {
    eSun <- lut$L5_ESUN
  } else if(sid == "LE4") {
    eSun <- lut$L4_ESUN
  } else {
    stop(paste0("Satellite ID ", sid, " is not supported, yet."))
  }
  if(normalize == FALSE){
    if(missing(esd)){
      stop("Variable esd is missing.")
    }
    eSun <- esd * eSun
  }
  return(eSun)
}


# Base function used for computing modell-based TOA irradiance -----------------
.calcTOAIrradModel <- function(rsr, model = "MNewKur", 
                              normalize = TRUE, esd){
  toa <- lut$SOLAR
  toa$WAVELENGTH <- round(toa$WAVELENGTH, 0)
  toa_aggregated <- aggregate(toa, by = list(toa$WAVELENGTH), FUN = "mean")
  
  rsr <- rsr[, c(grep("BCDE", colnames(rsr)), grep("WAVELENGTH", colnames(rsr)), 
                 grep("RSR", colnames(rsr)))]
  
  eSun <- lapply(unique(rsr$BCDE), function(x){
    rsr_solar <- merge(rsr[rsr$BCDE == x,], toa_aggregated, by = "WAVELENGTH")
    if(nrow(rsr_solar) > 0){
      act_eSun <- aggregate(rsr_solar$RSR * rsr_solar[,grep(model, names(rsr_solar))], 
                            by = list(rsr_solar$BCDE), FUN = "sum")[2] /
        aggregate(rsr_solar$RSR, by = list(rsr_solar$BCDE), FUN = "sum")[2] * 1000
      act_eSun <- unlist(act_eSun)
    } else {
      act_eSun <- c(NA)
    }
    names(act_eSun) <- paste0(x)
    return(act_eSun)
  })
  eSun <- unlist(eSun)
  
  if(normalize == FALSE){
    if(missing(esd)){
      stop("Variable esd is missing.")
    }
    eSun <- esd * eSun
  }
  return(eSun)
}


# Base function used for computing actual TOA irradiance based on rad/ref ------
.calcTOAIrradRadRef <- function(rad_max, ref_max, normalize = TRUE, esd){
  eSun <- pi * esd * rad_max / ref_max
  if(normalize == TRUE){
    if(missing(esd)){
      stop("Variable esd is missing.")
    }
    eSun <- 1/esd * eSun
  }
  return(eSun)
}