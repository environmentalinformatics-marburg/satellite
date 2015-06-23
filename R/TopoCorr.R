if ( !isGeneric("TopoCorr") ) {
  setGeneric("TopoCorr", function(x, ...)
    standardGeneric("TopoCorr"))
}
#' Correct for topographic effects
#' 
#' @param x Object of class satellite
#' @param mask Logical. If TRUE then the cloudmask from the 
#' satellite object (if available) will be considered in the regression model.
#' 
#' @details 
#' The method of Civco (1989) is applied on Atmospherically corrected bands 
#' (if not already available in the satellite object, 
#' \code{\link{calcAtmosCorr}} is performed with its default settings.:
#' First, an analytical hillshade image is created based on a DEM and sun 
#' elevation and sun zenith information from the metadata. A regression between
#' the hillshade (independent variable) and each channel is then calculated 
#' with consideration of a cloudmask (if available).
#' The regression coefficents are used to calibrate the hillshade raster 
#' (for each channel individually). 
#' Finally the calibrated hillshade image is substracted from the corresponding
#' channel and the mean value of the channel is added.
#' 
#' @return Satellite object with added, topographic corrected layers
#' 
#' @export TopoCorr
#' 
#' @name TopoCorr
#' 
#' @references CIVCO, D.L. (1989): Topographic normalization of Landsat Thematic
#' Mapper digitalimagery.In: Photogrammetric Engineering & Remote Sensing, 55, 
#' S. 1303–1309.
#' 
#' @examples
#' #' \dontrun{
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' x<-satellite(files)
#' TopoCorr(x)
#' }
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with added converted layers
#' 
#' @rdname TopoCorr
#'
setMethod("TopoCorr", 
          signature(x = "Satellite"), 
          function(x, mask=TRUE){
            if (is.null(getSatDataLayer(x, "hillShade"))){
              if (is.null(getSatDataLayer(x, "DEM"))){stop(
                "please provide a DEM in the satellite object")}
              x <- demTools(x, method = "hillShade")
            }
            atmoscbands <- getSatDataLayers(x)[grepl("_REF_AtmosCorr$", 
                                                     getSatBCDE(x))]
            if (length(atmoscbands)==0){
              x <- calcAtmosCorr(x)
              atmoscbands <- getSatDataLayers(x)[grepl("_REF_AtmosCorr$", 
                                                       getSatBCDE(x))]
            }
            for(i in 1:length(atmoscbands)){
              hillsh <- getSatDataLayer(x, "hillShade")
              hillsh <- raster::resample (hillsh, atmoscbands[[i]])
              if (mask){
                cloudmask <- getSatDataLayer(x, "cloudmask")[[1]]
              }
              layer_bcde <- gsub("AtmosCorr","TopoCorr",getSatBCDE(x)[
                grepl("_REF_AtmosCorr$", getSatBCDE(x))][i])
              tmp  <- TopoCorr(atmoscbands[[i]], hillsh, cloudmask)
              x <- addSatDataLayer(x, bcde = layer_bcde, data = tmp, 
                                   info="Add layer from TopoCorr(x)", 
                                   in_bcde=getSatBCDE(x)[
                                     grepl("_REF_AtmosCorr$", 
                                           getSatBCDE(x))][i])
            }
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @return raster::RasterStack object with converted layers
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
#' @param hillsh A rasterLayer created with \code{\link{hillShade}}
#' @param cloudmask A rasterLayer in which clouds are masked with NA values
#' @rdname TopoCorr
#'
setMethod("TopoCorr", 
          signature(x = "RasterLayer"), 
          function(x, hillsh, cloudmask=NULL){
            xtmp <- x
            if (!is.null(cloudmask)){
              xtmp <- raster::mask(x, cloudmask)
            }
            model <- summary(lm(raster::values(xtmp) ~ raster::values(hillsh)))
            calib <- hillsh * model$coefficients[[2]] + model$coefficients[[1]]
            x <- x - calib + mean(raster::values(xtmp), na.rm=TRUE)
            return(x)
          })

