if ( !isGeneric("calcHistMatch") ) {
  setGeneric("calcHistMatch", function(x, ...)
    standardGeneric("calcHistMatch"))
}
#' Illumination correction across scenes using histogram matching
#'
#' @description
#' This function adjusts the illumination of individual bands across two scenes
#' using a histogram match.
#'
#' @param x Satellite or raster::Raster* object providing the source band(s) to 
#' be adjusted.
#' @param bcde Band code which should be alligned
#' @param target The target band as raster::RasterLayer.
#' @param ttab Logical. If TRUE, the transformation table is being returned.
#' @param minv Lower limit of the possible range for transformation (if not 
#' provided, defaults to the minimum of both layers).
#' @param maxv Upper limit of the possible range for transformation (if not 
#' provided, defaults to the maximum of both layers).
#' @param step Step size used to build the new histogram
#' (if not provided, defaults to 1 for integer master layer and 0.01 for float 
#' master layer).
#' @param use_cpp Logical. If \code{TRUE}, C++ functionality (via \strong{Rcpp}) 
#' is enabled, which leads to a considerable reduction of both computation time
#' and memory usage.
#'
#' @name calcHistMatch
#' @export calcHistMatch
#' 
#' @return
#' If \code{ttab = FALSE} a RasterLayer; \cr 
#' if \code{ttab = TRUE} a list containing
#' \code{recode} the transformation table used to match the histograms
#' \code{newraster} the transformed RasterLayer
#'
#' @references This function is taken and only slightly adapted from the 
#' \code{landsat::histmatch} function by Sarah C. Goslee (2011). 
#' Analyzing Remote Sensing Data in R: The landsat Package. Journal of 
#' Statistical Software, 43(4), 1-25. URL \url{http://www.jstatsoft.org/v43/i04/}.
#'
#' @examples
#' ## sample data
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' ## extraction of source and targed bands
#' x <- getSatDataLayer(sat, "B004n")
#' target <- getSatDataLayer(sat, "B005n")
#' 
#' ## histogram matching
#' calcHistMatch(x, target)
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with added atmospheric corrected layers
#' 
#' @rdname calcHistMatch
#'
setMethod("calcHistMatch", 
          signature(x = "Satellite"), 
          function(x, target, bcde = NULL, minv = NULL, maxv = NULL, step = NULL, 
                   ttab = FALSE, use_cpp = TRUE){
            
            if(is.null(bcde)){
              bcde <- c(as.character(getSatBCDESolar(x)), 
                        as.character(getSatBCDEThermal(x)))
            }
            
            for(act_bcde in bcde){
              hm <- calcHistMatch(x = getSatDataLayer(x, act_bcde),
                                  target = target,
                                  bcde = act_bcde,
                                  minv = minv,
                                  maxv = maxv,
                                  step = step,
                                  ttab = ttab, 
                                  use_cpp = use_cpp)
              
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


# Function using raster::RasterStack object ------------------------------------
#' 
#' @return raster::RasterStack object with atmospheric corrected layers
#' 
#' @rdname calcHistMatch
#'
setMethod("calcHistMatch", 
          signature(x = "RasterStack"), 
          function(x, target, bcde = NULL, minv = NULL, maxv = NULL, step = NULL, 
                   ttab = FALSE, use_cpp = TRUE){
            # If not supplied, 'model' defaults to DOS2
            model <- model[1]
            
            for(l in seq(nlayers(x))){
              x[[l]] <- calcAtmosCorr(x, path_rad, esun, szen, model = "DOS2")
            }
            return(x)
          })









# Function using raster::RasterLayer object ------------------------------------
#' 
#' @return raster::RasterLayer object with atmospheric corrected layer
#' 
#' @rdname calcHistMatch
#'
setMethod("calcHistMatch", 
          signature(x = "RasterLayer"), 
          function(x, target, bcde = NULL, minv = NULL, maxv = NULL,
                   step = NULL, ttab = FALSE, use_cpp = TRUE){
            
            
            # path <- system.file("extdata", package = "satellite")
            # files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
            # sat <- satellite(files)
            # x <- getSatDataLayer(sat, "B004n")
            
            # target <- x * sample(100, ncell(x), replace = TRUE)/100
            
            # target <- getSatDataLayer(sat, "B005n")
            
            minv <- 1L
            maxv <- 256L
            x <- round((x - minValue(x)) * (maxv - minv) / 
                         (maxValue(x) - minValue(x)) + minv)
            
            target <- round((target - minValue(target)) * (maxv - minv) / 
                         (maxValue(target) - minValue(target)) + minv)
            
            hs <- hist(x, maxpixels = 1000000, 
                       breaks = seq(minValue(x), maxValue(x), 
                                    length.out = 256))
            ht <- hist(target, maxpixels = 1000000, 
                       breaks = seq(minValue(target), maxValue(target), 
                                    length.out = 256))
            
            ## enable c++ functionality
            if (use_cpp) {
              t <- insertMinReqRem(hs$counts, ht$counts)
              
            ## or stick to base-r version  
            } else {
              t <- matrix(data = 0, nrow = length(hs$counts), 
                          ncol = length(ht$counts))
              
              for(j in seq(length(ht$counts))){
                for(i in seq(length(hs$counts))){
                  pixelsreq <- ht$counts[j] - sum(t[1:i,j], na.rm = TRUE)
                  pixelsrem <- hs$counts[i] - sum(t[i,1:j], na.rm = TRUE)
                  t[i,j] <- min(pixelsreq, pixelsrem)
                }
              }
              df <-getValues(x)
              df[which(df <= 1)] <- 1
              for(i in seq(length(df))){
                cpf <- cumsum(t[df[i],])
                set.seed(1)
                p <- sample(1:max(cpf), 1)
                j <- which(p <= cpf)[1]
                df[i] <- ht$breaks[j]
                t[i,j] <- t[i,j]
              }
              
              df
              x <- round(setValues(x, df))
              plot(x)
              plot(target)
              hist(target)
              hist(x)
            }
          })  
