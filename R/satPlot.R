if ( !isGeneric("panSharpening") ) {
  setGeneric("panSharpening", function(x, ...)
    standardGeneric("panSharpening"))
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
#' @export panSharpening
#' 
#' @name panSharpening
#'
#' @details Pan sharpen low resolution satellite channels by using the high resolution panchromatic channel. This function uses
#' the same algorithm as the OTB Toolbox where "The idea is to apply a low pass filter to the panchromatic band to give it a spectral
#' content (in the Fourier domain) equivalent to the XS data. Then we normalize the XS data with this low-pass panchromatic and multiply
#' the result with the original panchromatic band." (see \url{https://www.orfeo-toolbox.org/SoftwareGuide/SoftwareGuidech13.html#x41-2140011}).
#' 
#' @references 
#' Al-amri, Salem Saleh, Namdeo V. Kalyankar, and Santosh D. Khamitkar. "A comparative study of removal noise from remote sensing image." 
#' \url{http://ijcsi.org/articles/A-Comparative-Study-of-Removal-Noise-from-Remote-Sensing-Image.php}
#' 
#' Bhattacharya, Amit K., P. K. Srivastava, and Anil Bhagat. "A modified texture filtering technique for satellite images."
#' Paper presented at the 22nd Asian Conference on Remote Sensing. Vol. 5. 2001.
#' \url{http://a-a-r-s.org/aars/proceeding/ACRS2001/Papers/DPA3-08.pdf}
#' 
#' Randen, Trygve, and John Hakon Husoy. "Filtering for texture classification: A comparative study."
#' Pattern Analysis and Machine Intelligence, IEEE Transactions on 21.4 (1999): 291-310.
#' \url{http://dx.doi.org/10.1109/34.761261}.
#' 
#' PAN Sharpening articles
#' http://remotesensing.spiedigitallibrary.org/article.aspx?articleid=1726558
#' 
#' http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1368950&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D1368950

#'
#' @examples 
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' panSharpening(sat)
#' 
setMethod("panSharpening", 
          signature(x = "Satellite"), 
          function(x, filter = "mean", winsize = 3, subset = FALSE){
            pan <- getSatDataLayer(x, getSatBCDEType(x, id = "PCM"))
            d2 <- winsize*res(pan)[1]
            sigma <- 3
            #select type of filter for PAN smoothing
            switch(filter,
                   mean = {
                     #Since filtering is done using the raster::focal function raster::focalWeight function
                     #is used to define the weight matrix for focal.
                     ftype <- focalWeight(pan, d = d2, type = "rectangle")
                     fun <- sum #sum is default to focal. just set to be sure.
                   },
                   Gauss = {
                     #set sigma default to 3 for Gauss function. Maybe make sigma subject to user choice?
                     ftype <- focalWeight(pan, d=c(sigma, d2), type = "Gauss")
                     fun <- sum
                   },
                   median = {
                     #since median filtering can not be defined by the weights matrix for the focal function
                     #the function to be applied when envocing focal is set to median (this is computationally inefficiant see raster::focal)
                     ftype <- focalWeight(pan, d = d2, type = "rectangle" )
                     fun <- median
                   }
            )
            
            pan_lf <- focal(pan, w = ftype, fun = fun)
            
            #pan sharpen all low resolution channels of sat object
            #todo: - make id for resolution more general (e.g. scan meta data of sat object for min/ max resolution!?)
            res_bands <- getSatBCDESres(x, id = "30")
            #maybe loop can be generalizd since it is similar to loops found in other function e.g. satAtmosCorr
            #maybe building raster brick instead of looping over layers of sat object will speed up processing?
            for(bcde_res in res_bands){
              #resample low resolution layer to resolution of panchromatic layer
              interpol <- resample(getSatDataLayer(x, bcde_res), pan, method = "ngb")
              #pan sharpen
              ref <-  interpol / pan_lf * pan
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
              x <- subsetSat(x,"PAN_sharpend")
            }
            #set new resolution to old column (dirty hack)
            x@meta$SRES[x@meta$CALIB == "PAN_sharpend"] <- res(pan)[1]
            return(x)
          })


test <- stack(sat3@layers[[13]],sat3@layers[[14]],sat3@layers[[15]])
raster_stack <- test
raster_stack[[1]] <- stretch(raster_stack[[1]], minv = 0, maxv = 255, minq = 0.01, maxq = 0.75)
raster_stack[[2]] <- stretch(raster_stack[[2]], minv = 0, maxv = 255, minq = 0.01, maxq = 0.72)
raster_stack[[3]] <- stretch(raster_stack[[3]], minv = 0, maxv = 255, minq = 0.01, maxq = 0.80)
plotRGB(raster_stack,r=3,g=2,b=1,stretch=lin)

plotRGB(raster_stack,r=3,g=2,b=1,stretch="lin")