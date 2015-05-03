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
#' @examples test
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

# http://ssrebelious.blogspot.de/2015/02/pan-sharpening-using-r.html
# # Create needed functions -------------------------------------------------
# 
# pansharpFun <- function(raster){
#   '
#   This function pansharpens a raster
#   '
#   # @param raster - Raster object with 3 bands (to-be-pansharpened, high-res and low-frequency component of the high-res image)
#   # @param band - band numver, integer
#   # @return pansharpened_raster - pansharpened Raster object
#   # pansharp = Lowres * Highres / LPF[Highres]
#   
#   pansharpened_raster <- (raster[,1] * raster[,2]) / raster[,3]
# }
# 
# extractLPF <- function(pan, multi, filter = 'auto', fun = mean) {
#   '
#   Returns a low-frequency component of the high-resolution raster by the
#   filter adjusted to the low-resolution raster
#   '
#   # @param pan - a high-resolution panchromatic raster - Raster object
#   # @param multi - low-resolution raster to be pansharpened - Raster object
#   # @param filter - a smoothing wondow - matrix
#   # @param fun - a function to process filter (part of the focal() function)
#   # @return LPF - a low-frequency component of the high-resolution raster - Raster object
#   
#   # Adjust filter size
#   if (filter == 'auto') {
#     pan_res <- res(pan) # (x, y) resolution of the panchromatic raster in CRS units (?)
#     multi_res <- res(multi) # (x, y) resolution of the lowres raster in CRS units (?)
#     x_res_ratio <- round(multi_res[1]/pan_res[1])
#     y_res_ratio <- round(multi_res[2]/pan_res[2])
#     total <- x_res_ratio + y_res_ratio
#     filter <- matrix(1, nc = x_res_ratio, nr = y_res_ratio)
#     
#     # Enshure that the matrix has an uneven number of colums and rows (needed by focal())
#     if (nrow(filter)%%2 == 0) {
#       filter <- rbind(filter, 0)
#     }
#     if (ncol(filter)%%2 == 0) {
#       filter <- cbind(filter, 0)
#     }
#     
#     LPF <- focal(pan, w = filter, fun = fun) # low-frequency component
#     
#   } 
#   
#   
#   processingPansharp <- function(pan, multi, filter = 'auto', fun = mean){
#     '
#     Pansharpening routine
#     '
#     # @param pan - a high-resolution panchromatic raster - Raster object
#     # @param multi - low-resolution raster to be pansharpened - Raster object
#     # @param filter - a smoothing wondow - matrix
#     # @param fun - a function to process filter (part of the focal() function)
#     # @return pansharp - pansharpened 'multi' raster - Raster object
#     
#     # Check if input parameters are valid - we can loose a lot of time if some of the inputs is wrong
#     
#     LPF <- extractLPF(pan, multi, filter, fun)
#     
#     multi <- resample(multi, pan) # resample low-resolution image to match high-res one
#     
#     all <- stack(multi, pan, LPF)
#     
#     bands <- nbands(multi)
#     pan_band <- bands + 1
#     lpf_band <- bands + 2
#     
#     # Pansharpen layers from low-resolution raster one by one
#     pansharp_bands <- list()
#     for (band in 1:bands) {
#       subset <- all[[c(band, pan_band, lpf_band)]]
#       raster <- calc(subset, pansharpFun)
#       pansharp_bands[[band]] <- raster
#     }
#     
#     pansharp <- stack(pansharp_bands)
#   }
#   
#   saveResult <- function(raster, path, format = 'GTiff', datatype = 'INT2S'){
#     '
#     Saves Raster object to location
#     '
#     # @param raster - raster to be saved - Raser object
#     # @param path - path including filename without extention - string
#     # @param format - format of the output raster accordingly to writeRaster() function - string
#     # @param datatype - datatype of the raster accordingly to writeRaster() - string
#     
#     writeRaster(raster,
#                 path,
#                 format = format,
#                 datatype = datatype,
#                 overwrite = T)
#   }
#   
#   
#   # Do pansharpening --------------------------------------------------------
#   
#   
#   pan <- raster('pan.tif')
#   multi <- brick('multi.tif')
#   pansharp <- processingPansharp(pan, multi)
#   output_path <- 'r_pansharp-new' # includes path and filename but not the extention
#   saveResult(pansharp, output_path)