if ( !isGeneric("satPanSharp") ) {
  setGeneric("satPanSharp", function(x, ...)
    standardGeneric("satPanSharp"))
}

#' Pan sharpen low resolution satellite channels by using the high resolution panchromatic channel.
#'
#' @description The function PAN sharpens the low resolution channels with the pachromatic channel.
#' This is done by multiplying the normlized XS channel with the PAN channel (see Details).
#' 
#'
#' @param x Object of type Satellite
#' @param filter type of filter to be used for smoothing the PAN raster (e.g. mean (default), Gauss, median).
#' @param winsize with n integer size of filter window n x n in pixels. Defaults to 3.
# #' @param subset logical, defaults to TRUE. Drops all layers but the cropped ones.
#' If set to false appends cropped layers to Satellite object.
#'
#' @return Satellite object
#' 
#' @export satPanSharp
#' 
#' @name satPanSharp
#'
#' @details This is a wrapper function for panSharp applicable to satellite objects. See \code{\link{panSharp}}
#' for details. 
#'
#' @examples 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' satPanSharp(sat)
#' 
setMethod("satPanSharp", 
          signature(x = "Satellite"), 
          function(x, filter = "mean", winsize = 3, subset = FALSE){
            pan <- getSatDataLayer(x, getSatBCDEType(x, id = "PCM"))
            #create low frequency component of pan
            pan_lf <- panlf(pan, filter, winsize)
            
            #pan sharpen all low resolution channels of sat object
            #todo: - make id for resolution more general (e.g. scan meta data of sat object for min/ max resolution!?)
            res_bands <- getSatBCDESres(x, id = "30")
            #maybe loop can be generalizd since it is similar to loops found in other function e.g. satAtmosCorr
            #maybe building raster brick instead of looping over layers of sat object will speed up processing?
            for(bcde_res in res_bands){
              #pan sharpen
              ref <- sharp(pan, getSatDataLayer(x, bcde_res), pan_lf)
              layer_bcde <- paste0(substr(bcde_res, 1, nchar(bcde_res) - 1),
                                   "_PAN_sharpend")
              meta_param <- data.frame(getSatSensorInfo(x),
                                       getSatBandInfo(x, bcde_res, 
                                                      return_calib = FALSE),
                                       CALIB = "PAN_sharpend")
              info <- sys.calls()[[1]]
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = layer_bcde, data = ref,
                                   meta_param = meta_param,
                                   info = info, in_bcde = bcde_res)
            }
            if(subset == TRUE){
              x <- satSubset(x,"PAN_sharpend")
            }
            #set new resolution to old column (dirty hack)
            x@meta$SRES[x@meta$CALIB == "PAN_sharpend"] <- res(pan)[1]
            return(x)
          })
