#' Illumination correction across scenes using histogram matching
#'
#' @description
#' This function adjusts the illumination of individual bands across two scenes
#' using a histogram match.
#'
#' @param x A Satellite object or a raster::RasterStack or raster::RasterLayer
#' providing the source band(s) which should be adjusted.
#' @param target The target band as raster::RasterLayer
#' @param mask An invariant feature mask as raster::RasterLayer
#' @param ttab Logical, whether to return the transformation table
#' @param minv Lower bound of the possible range for transformation
#' (if not provided, the minimum of both layers)
#' @param maxv Upper bound of the possible range for transformation
#' (if not provided, the maximum of both layers)
#' @param by Step size used to build the new historgram
#' (if not provided, 1 for integer master layer, 0.01 for float master layer)
#'
#' @export calcHistMatch
#' 
#' @return
#' if \code{ttab = FALSE} a RasterLayer, if \code{ttab = TRUE} a list
#' with components
#' \code{recode} the trasnsformation table used to match the histograms
#' \code{newraster} the transformed RasterLayer
#'
#' @references This function is taken and only slightly adapted from the histmatch
#' function of Sarah C. Goslee (2011). Analyzing Remote Sensing Data in R: The 
#' landsat Package. Journal of Statistical Software,43(4), 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' 
#' x = getSatDataLayer(sat, "B004n")
#' target = getSatDataLayer(sat, "B005n")
#'

calcHistMatch <- function(x, target, mask, ttab = FALSE, 
                            minv = NULL, maxv = NULL, step = NULL){

  
  n <- 256
  m <- 20
  ho <- hist(x, maxpixels = 1000000, breaks = n)
  ht <- hist(target, maxpixels = 1000000, breaks = m)
  
#   pixelsreq <- matrix(data = NA, nrow = n, ncol = m)
#   pixelsrem <- matrix(data = NA, nrow = n, ncol = m)
#   subtotal1 <- matrix(data = NA, nrow = n, ncol = m)
#   subtotal2 <- matrix(data = NA, nrow = n, ncol = m)
  t <- matrix(data = NA, nrow = n, ncol = m)
  for(j in seq(m)){
    for(i in seq(n)){
      pixelsreq <- ht$counts[j] - sum(t[1:i,j], na.rm = TRUE)
      pixelsrem <- ho$counts[i] - sum(t[i,1:j], na.rm = TRUE)
      t[i,j] <- min(pixelsreq, pixelsrem)
    }
  }
  t

  if (missing(minv)) {
    minv <- floor(min(minValue(x), minValue(target), na.rm = TRUE))
  }
  
  if (missing(maxv)) {
    maxval <- ceiling(max(maxValue(x), maxValue(target), na.rm = TRUE))
  }
  
  if (missing(step)) {
    step <- switch(typeof(minValue(x)),
                   double = 0.01,
                   integer = 1L)
  }
  
  minv <- round_any(minv, step, f = floor)
  maxv <- round_any(maxv, step, f = ceiling)
  

  results <- tofix
  master <- as.vector(as.matrix(master))
  tofix <- as.vector(as.matrix(tofix))
  if (missing(mask)) 
    mask <- rep(NA, length(master))
  else mask <- as.vector(as.matrix(mask))
  results.final <- rep(NA, length(mask))
  master <- master[is.na(mask)]
  tofix <- tofix[is.na(mask)]
  breaks <- seq(minval, maxval, by = by)
  master.cdf <- hist(master, breaks = breaks, plot = FALSE)
  master.cdf <- c(0, cumsum(master.cdf$counts/sum(master.cdf$counts)))
  tofix.cdf <- hist(tofix, breaks = breaks, plot = FALSE)
  tofix.cdf <- c(0, cumsum(tofix.cdf$counts/sum(tofix.cdf$counts)))
  results.recode <- breaks
  results.values <- rep(NA, length(tofix))
  for (i in 2:length(breaks)) {
    testvals <- breaks[master.cdf < tofix.cdf[i]]
    if (length(testvals) > 0) 
      results.recode[i] <- max(testvals)
    results.values[tofix > breaks[i - 1] & tofix <= breaks[i]] <- results.recode[i]
  }
  results.final[is.na(mask)] <- results.values
  if (class(results) == "SpatialGridDataFrame") 
    results@data[, 1] <- results.final
  else if (is.data.frame(results)) 
    results <- data.frame(matrix(results.final, nrow = nrow(results), 
                                 ncol = ncol(results)))
  else if (is.matrix(results)) 
    results <- matrix(results.final, nrow = nrow(results), 
                      ncol = ncol(results))
  else results <- results.final
  list(recode = results.recode, newimage = results)
  
  
  
  
  
  
  fix <- histmatch(target, x, minval = minv,
                   maxval = maxv, by = step, ...)
  fix_rst <- raster(fix$newimage)
  
  if (ttab) {
    return(list(recode = fix$recode,
                newraster = fix_rst))
  } else {
    return(fix_rst)
  }
}
