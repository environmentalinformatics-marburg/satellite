#' Illumination correction across scenes using histogram matching
#'
#' @description
#' This function adjusts the illumination of individual bands across two scenes
#' using a histogram match.
#'
#' @param x Satellite or raster::Raster* object providing the source band(s) to 
#' be adjusted.
#' @param target The target band as raster::RasterLayer.
#' @param mask An invariant feature mask as raster::RasterLayer.
#' @param ttab Logical. If TRUE, the transformation table is being returned.
#' @param minv Lower limit of the possible range for transformation (if not 
#' provided, defaults to the minimum of both layers).
#' @param maxv Upper limit of the possible range for transformation (if not 
#' provided, defaults to the maximum of both layers).
#' @param by Step size used to build the new histogram
#' (if not provided, defaults to 1 for integer master layer and 0.01 for float 
#' master layer).
#'
#' @export calcHistMatch
#' 
#' @return
#' If \code{ttab = FALSE} a RasterLayer; \cr 
#' if \code{ttab = TRUE} a list containing
#' \code{recode} the transformation table used to match the histograms
#' \code{newraster} the transformed RasterLayer
#'
#' @references This function is taken and only slightly adapted from the 
#' \code{\link{histmatch}} function by Sarah C. Goslee (2011). Analyzing Remote 
#' Sensing Data in R: The landsat Package. Journal of Statistical Software, 
#' 43(4), 1-25. URL \url{http://www.jstatsoft.org/v43/i04/}.
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

calcHistMatch <- function(x, target, mask, ttab = FALSE, 
                          minv = NULL, maxv = NULL, step = NULL){
  
  #   x <- getSatDataLayer(sat, "B002n")
  #   target <- getSatDataLayer(sat, "B003n")
  #   target <- getSatDataLayer(sat, "B002n")*1.12
  
  n <- 500
  nmin <- 1
  nmax <- 500
  m <- 500
  
  x <- round((x - minValue(x)) * (nmax - nmin) / (maxValue(x) - minValue(x)) + nmin)
  
  #   target <- (target - minValue(target)) * 
  #     (nmax - nmin) / (maxValue(target) - minValue(target)) + nmin
  #   
  
  ho <- hist(x, maxpixels = 1000000, 
             breaks = seq(minValue(x), maxValue(x), length.out = n+1))
  ht <- hist(target, maxpixels = 1000000, 
             breaks = seq(minValue(target), maxValue(target), length.out = m+1))
  
  t <- matrix(data = 0, nrow = length(ho$counts), ncol = length(ht$counts))
  for(j in seq(length(ht$counts))){
    for(i in seq(length(ho$counts))){
      pixelsreq <- ht$counts[j] - sum(t[1:i,j], na.rm = TRUE)
      pixelsrem <- ho$counts[i] - sum(t[i,1:j], na.rm = TRUE)
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
    t[v,j] <- t[v,j]
  }
  df
  x <- round(setValues(x, df))
  plot(x)
  plot(target)
  hist(target)
  hist(x)
  
  
  
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
