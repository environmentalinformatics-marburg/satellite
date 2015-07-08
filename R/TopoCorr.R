if ( !isGeneric("TopoCorr") ) {
  setGeneric("TopoCorr", function(x, ...)
    standardGeneric("TopoCorr"))
}
#' Correct for topographic effects.
#' 
#' @param x Satellite object.
#' @param mask Logical. If \code{TRUE}, the cloudmask from the Satellite object 
#' (if available) will be considered in the regression model.
#' 
#' @details 
#' The method of Civco (1989) is applied on atmospherically corrected bands 
#' (if not already available in the Satellite object, 
#' \code{\link{calcAtmosCorr}} is performed with its default settings.):
#' First, an analytical hillshade image is created based on a DEM and sun 
#' elevation and sun zenith information from the metadata. A regression between
#' the hillshade (independent variable) and each channel is then calculated 
#' with consideration of a cloudmask (if available).
#' The regression coefficents are used to calibrate the hillshade raster 
#' (for each channel individually). 
#' Finally, the calibrated hillshade image is subtracted from the corresponding
#' channel and the mean value of the channel is added.
#' 
#' @return If x is a Satellite object, a Satellite object with added, 
#' topographic corrected layers; if x is a \code{raster::Raster*} object, a 
#' \code{raster::Raster*} object with converted layer(s).
#' 
#' @export TopoCorr
#' 
#' @name TopoCorr
#' 
#' @references CIVCO, D.L. (1989): Topographic normalization of Landsat Thematic
#' Mapper digitalimagery. \emph{Photogrammetric Engineering & Remote Sensing}, 
#' 55, 1303–1309.
#' 
#' @examples
#' #' \dontrun{
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' x <- satellite(files)
#' TopoCorr(x)
#' }
NULL


# Function using satellite object ----------------------------------------------
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
#' @param hillsh A \code{raster::RasterLayer} created with 
#' \code{\link{raster::hillShade}}. 
#' @param cloudmask A \code{raster::RasterLayer} in which clouds are masked with 
#' NA values. 
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

