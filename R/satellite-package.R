#' Smorgasbord for remote sensing functions
#'
#' The package provides a variety of functions which are useful for 
#' handling, manipulating and visualizing remote sensing data.
#'
#' @name satellite-package
#' @aliases satellitepackage
#' @docType package
#' @title Smorgasboard for remote sensing functions.
#' @author Thomas Nauss, Hanna Meyer, Florian Detsch, Tim Appelhans \cr
#' \cr
#' \emph{Maintainer:} Florian Detsch \email{fdetsch@@web.de}
#'
#' @import methods raster Rcpp
#' @importFrom stats mvfft
#' @importFrom stats4 plot
#' 
#' @rawNamespace useDynLib(satellite, .registration = TRUE)
#' 
#' @references Some functions are taken and/or adopted from Sarah C. Goslee 
#' (2011). Analyzing Remote Sensing Data in R: The landsat Package. Journal of 
#' Statistical Software, 43(4), 1-25, \doi{10.18637/jss.v043.i04}.
#' 
#' @keywords package
#'
NULL
#' 
#' @docType data
#' @name l7
#' @title Landsat 7 sample data
#' @description This dataset comes from the USGS. It contains part of the 
#' Landsat 7 scene LE07_L1TP_195025_20010730_20170204_01_T1 (Collection 1 
#' Level-1) from 2001-07-30 over Maburg, Germany.
#' @details Use of this data requires your agreement to the USGS regulations on 
#' using Landsat data.
#' @format \code{RasterStack} with bands 1-8 (incl. QA) of 41 by 41 pixels.
#' @source
#' \url{https://earthexplorer.usgs.gov/}
#' @examples 
#' plotRGB(l7, r = 3, b = 1, stretch = "hist")
NULL
#'
#' @docType data
#' @name l8
#' @title Landsat 8 sample data
#' @description This dataset comes from the USGS. It contains part of the 
#' Landsat 8 scene LC08_L1TP_195025_20130707_20170503_01_T1 (Collection 1 
#' Level-1) from 2013-07-07 over Maburg, Germany.
#' @details Use of this data requires your agreement to the USGS regulations on 
#' using Landsat data.
#' @format \code{RasterStack} with bands 1-7, 9-11 (incl. QA) of 41 by 41 pixels.
#' @source
#' \url{https://earthexplorer.usgs.gov/}
#' @examples 
#' plotRGB(l8, r = 4, g = 3, b = 2, stretch = "hist") # true-color composite
#' plotRGB(l8, r = 5, g = 4, b = 3, stretch = "hist") # false-color composite
NULL
