#' An S4 class to represent a satellite data file
#'
#' @slot name name of the data file without extension
#' @slot filepath full path and file of the data file
#' @slot path path to the data file
#' @slot file filename inkl. extension of the data file
#' @slot extension extension of the data file
#' 
#' @exportClass SatelliteFilepath
#' 
setClass("SatelliteFilepath", 
         representation(
           name ="character",
           filepath = "character",
           path = "character",
           file = "character",
           extension = "character"
         )
)


#' An S4 class to represent satellite data
#'
#' @slot data a data.frame object containing the metadata
#' 
#' @exportClass SatelliteData
#' 
setClass("SatelliteData",
         representation(
           data = "RasterStack"
         ),
         contains = "SatelliteFilepath"
)


#' An S4 class to represent satellite metadata
#'
#' @slot meta a  object containing the data
#' 
#' @exportClass SatelliteMetaData
#' 
setClass("SatelliteMetaData",
         representation(
           meta = "data.frame"
         ),
         contains = "SatelliteFilepath"
)


#' An S4 class to represent a complete satellite dataset
#' 
#' @exportClass Satellite
#' 
setClass("Satellite", 
         contains = c("SatelliteFilepath", "SatelliteData", "SatelliteMetaData")
)         



# # 
# # t <- new("Satellite", data = l8)
# # 
# # f <- function(x){
# #   names(x)
# # }
# # 
# # f1 <- function(x){
# #   f(x@data)
# # }
# # 
# # 
# # setGeneric('getNames', function(x, ...)
# #   standardGeneric('getNames'))
# # 
# # setMethod("getNames", signature("Satellite"), f1)
# # setMethod("getNames", signature("RasterStack"), f1)
# # 
# # 
# # f1(l8)
# # f1(t)
