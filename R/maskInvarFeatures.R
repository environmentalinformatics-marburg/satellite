#' Identify pseudi-invariant features from a satellite scene
#'
#' @description
#' Identify pseudi-invariant features from a satellite scene based on a 
#' vis, near-infravis and short-wave infravis band.
#'
#' @param vis a raster of the sensor's vis band
#' @param nir a raster of the sensor's nir band
#' @param swir a raster of the sensor's swir band
#' @param quant a value v = [0...1] which is used to define the percentage
#' threshold values (thv) for invariant features (nir/vis ratio < thv, 
#' swir band values > 1-thv)
#' 
#'
#' @return raster object with invariant pixels marked with 1, 0 otherwise
#'
#' @export maskInvarFeatures
#' 
#' @details Invariant features are identified as pixels which belong to the 
#' group of (i) the n lowest VIS/NIR ratios and of (ii) the highest n
#' SWIR values. The value of n is given by the parameter quant (0...1).
#' 
#' @references This function is taken and only slightly adapted from the PIF
#' function of Sarah C. Goslee (2011). Analyzing Remote Sensing Data in R: The 
#' landsat Package. Journal of Statistical Software,43(4), 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' The underlaying theory has been published by Schott RJ, Salvaggio C and 
#' Volchok WJ (1988) Radiometric scene normalization using pseudoinvariant 
#' features. Remote Sensing of Environment 26/1, 
#' doi:10.1016/0034-4257(88)90116-2, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788901162}
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' maskInvarFeatures(vis = getSatDataLayer(sat, "B004n"), 
#'                   nir = getSatDataLayer(sat, "B005n"), 
#'                   swir = getSatDataLayer(sat, "B007n"))
#' 
maskInvarFeatures <-function(vis, nir, swir, quant=0.01) {
    ratio_nir_vis <- nir/vis
    
    ratio_nir_vis_quant <- quantile(ratio_nir_vis, probs = quant, na.rm=TRUE)
    swir_quant <- quantile(swir, probs = 1-quant, na.rm=TRUE)
    
    invar_feats <- ratio_nir_vis < ratio_nir_vis_quant & swir > swir_quant
    #     invar_feats <- ratio_nir_vis < ratio_nir_vis_quant & 
    #       swir > swir_quant & swir < 255
        
    return(invar_feats)
  }
