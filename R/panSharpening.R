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