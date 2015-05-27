if ( !isGeneric("TopoCorr") ) {
  setGeneric("TopoCorr", function(x, ...)
    standardGeneric("TopoCorr"))
}

#' Correct for topographic effects
#' @param x Object of class satellite
#' @param dem Object of class RasterLayer. 
#' If not specified, an srtm elevation model will be downloaded.
#' @param cloudmask Logical. If TRUE then the cloudmask from the 
#' satellite object (if available) will be considered in the regression model.
#' @details 
#' The method of Civco (1989) is applied:
#' First, an analytical hillshade image is created based on a DEM and sun elevation and
#' sun zenith information from the metadata. A regression between
#' the hillshade (independent variable) and each channel is then calculated 
#' with consideration of a cloudmask (if available).
#' The regression coefficents are used to calibrate the hillshade raster 
#' (for each channel individually). 
#' Finally the calibrated hillshade image is substracted from the corresponding
#' channel and the mean value of the channel is added.
#' @return Satellite object with added, topographic corrected layers
#' @export topoC
#' @references CIVCO, D.L. (1989): Topographic normalization of Landsat Thematic Mapper digital
#imagery.In: Photogrammetric Engineering & Remote Sensing, 55, S. 1303–1309.
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' x<-satellite(files)
#' TopoCorr(x)






# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with added converted layers
#' 
#' @rdname TopoCorr
#'
# setMethod("TopoCorr", 
#           signature(x = "Satellite"), 
#           )





# Function using raster::RasterStack object ------------------------------------
#' 
#' @return raster::RasterStack object with converted layers
#' 
#' @rdname TopoCorr
#'
setMethod("TopoCorr", 
          signature(x = "RasterStack"), 
          function(x, hillsh, cloudmask=NULL){
            for(l in seq(nlayers(x))){
              x[[l]] <- TopoCorr(x[[l]], hillsh, cloudmask)
            }
            return(x)
          })



# Function using raster::RasterLayer object ------------------------------------
#' 
#' @return raster::RasterLayer object with converted layer
#' 
#' @rdname TopoCorr
#'
setMethod("TopoCorr", 
          signature(x = "RasterLayer"), 
          function(x, hillsh, cloudmask=NULL){
            xtmp<-x
            if (!is.null(cloudmask)){
              xtmp<-mask(x,cloudmask)
            }
            model<-summary(lm(values(xtmp)~values(hillsh)))
            calib<-hillsh*model$coefficients[[2]]+model$coefficients[[1]]
            x <- x-calib+mean(values(xtmp),na.rm=TRUE)
            return(x)
          })

