if ( !isGeneric("allignGeometry") ) {
  setGeneric("allignGeometry", function(x, ...)
    standardGeneric("allignGeometry"))
}
#' Allign raster geometry between two data sets
#'
#' @description
#' Allign raster data by bringing it in the same geometry and extend.
#' If the data set is not in the same projection as the template, the allignment
#' will be computed by reprojection. If the data has already the same
#' projection, the data set will be croped and aggregated prior to resampling
#' in order to reduce computation time.
#'
#' @param data Raster layer to be resampled
#' @param template Raster or spatial data set from which geometry can be extracted
#' @param method Method for resampling ("ngb" or "bilinear")
#'  
#' @export allignGeometry
#' 
#' @name allignGeometry
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' allignGeometry(sat, template = getSatDataLayer(sat, "B008n"), 
#'                band_codes = "B001n")
NULL


# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with path radiance for each band in the metadata
#' (W m-2 micrometer-1)
#' 
#' @rdname allignGeometry
#'
setMethod("allignGeometry", 
          signature(x = "Satellite"), 
          function(x, template, band_codes, type, method = "bilinear"){
            if(!missing(type)){
              band_codes <- getSatBCDE(x)[which(getSatType(sat) == type)]
            }
            else if(missing(band_codes)){
              band_codes <- getSatBCDE(sat)
            } 
            for(bcde in band_codes){
              ag <- allignGeometry(x = getSatDataLayer(x, bcde),
                                   template = template, method = method)
              layer_bcde <- paste0(bcde, "_AG")
              meta_param <- getSatMetaBCDETemplate(x, bcde)
              meta_param$BCDE <- layer_bcde
              meta_param$SRES <- xres(template)
              
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = layer_bcde, data = ag,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde)
            }
            return(x)
          })


# Function using raster::RasterStack object ------------------------------------
#' 
#' @return raster::RasterStack object with converted layers
#' 
#' @rdname allignGeometry
#'
setMethod("allignGeometry", 
          signature(x = "RasterStack"), 
          function(x, template, method = "bilinear"){
            for(l in seq(nlayers(x))){
              x[[l]] <- allignGeometry(x[[l]], template, method)
            }
            return(x)
          })


# Function using raster::RasterLayer object ------------------------------------
#' 
#' @return raster::RasterLayer object with converted layer
#' 
#' @rdname allignGeometry
#'
setMethod("allignGeometry", 
          signature(x = "RasterLayer"),
          function(x, template, method = "bilinear"){
            if(projection(x) == projection(template)){
              x <- crop(x, template, snap = "out")
              if(class(template) == "RasterLayer"){
                if(x@ncols / template@ncols >= 2){
                  factor <- floor(x@ncols/template@ncols)
                  x <- aggregate(x, fact = factor, fun = mean, 
                                 expand=TRUE)
                }
                x <- resample(x, template, method = method)
              }
            } else {
              x <- projectRaster(x, template, method = method)
            }
            return(x)
          })
