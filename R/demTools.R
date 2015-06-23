if ( !isGeneric("demTools") ) {
  setGeneric("demTools", function(x, ...)
    standardGeneric("demTools"))
}

#' Compute terrain characteristics from digital elevation models
#'
#' @description
#' Compute terrain characteristics from digital elevation models using the 
#' raster::terrain function or raster::hillSahde function.
#' @param x An object of type Satellite containing a DEM or a DEM provided as 
#' RasterLayer.
#' @param method currently "slope", "aspect" and "hillshade" are implemented
#' @param bcde The name of the DEM layer in the satellite object. 
#' @export demTools
#' @name demTools
#' @examples
#' \dontrun{
#' ## Only works with a dem
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' sat <- addSatDataLayer(sat, method, data = DEM, info = NULL, in_bcde = NULL)
#' sat <- demTools(sat)
#' }

NULL




# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with added Layer of calculated terrain information
#' 
#' @rdname demTools
#'
setMethod("demTools", 
          signature(x  =  "Satellite"), 
          function(x,method = "hillShade",bcde = "DEM"){
            sunElev <- NULL
            sunAzim <- NULL
            if (method == "hillShade"){
              if (sum(complete.cases(unique(getSatSELV(x))))>1||sum(complete.cases(unique(getSatSAZM(x))))>1){
                print("Warning: Satellite data have different Sun elevation or 
                      Sun azimuth values. Only the first element is used")}
              sunElev <- as.numeric(getSatSELV(x)[1])
              sunAzim <- as.numeric(getSatSAZM(x)[1])
            }
            result <- demTools(getSatDataLayer(x, bcde), sunElev = sunElev, 
                               sunAzim = sunAzim, 
                               method = method)
            x <- addSatDataLayer(x, bcde = method, data = result, info = paste0(
              "Add layer ", method), in_bcde = bcde)
          }
)




# Function using raster::RasterLayer object ------------------------------------
#' 
#' @return raster::RasterLayer object of calculated terrain information
#' @param sunElev if \code{method = "hillShade"}: The the elevation angle of the 
#' sun in degrees. 
#' See parameter angle in \code{\link{hillShade}}
#' @param sunAzim if \code{method = "hillShade"}: The sun azimuth angle in 
#' degree. See parameter direction in \code{\link{hillShade}}
#' 
#' @rdname demTools
#'
setMethod("demTools", 
          signature(x  =  "RasterLayer"), 
          function(x, sunElev = NULL, sunAzim = NULL, method = "hillShade"){
            if (method == "slope"){
              result <- raster::terrain(x, opt = "slope") 
            }
            if (method == "aspect"){
              result <- raster::terrain(x, opt = "aspect") 
            }
            if(method == "hillShade"){
              slope <- raster::terrain(x, opt  =  "slope")
              aspect <- raster::terrain(x, opt  =  "aspect")
              result <- raster::hillShade(slope = slope, aspect = aspect, 
                                  angle = sunElev, direction = sunAzim)
            }
            return(result)
          })