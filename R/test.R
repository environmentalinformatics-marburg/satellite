# test_read <- function(...){
#   library(raster)
#   library(rgdal)
#   library(sp)
#   
#   inpath_l7 <- 
#     "D:/active/moc/am-remote-sensing/examples/data/landsat/l7_2001-07-30/"
#   files_l7 <- list.files(inpath_l7, pattern = glob2rx("*.TIF"), full.names = TRUE)
#   
#   inpath_l8 <- 
#     "D:/active/moc/am-remote-sensing/examples/data/landsat/l8_2013-07-07/"
#   files_l8 <- list.files(inpath_l8, pattern = glob2rx("*.TIF"), full.names = TRUE)
#   files_l8 <- files_l8[-12]
#   
#   l <- 8
#   if(l == 8){
#     files <- files_l8
#     data <- stack(paste0(inpath_l8, "l8_2013-07-07_30m_crop.tif"))
#   } else {
#     files <- files_l7
#     data <- stack(paste0(inpath_l7, "l7_2001-07-30_30m_crop.tif"))
#   }
#   return(list(files, data))
# }
# 
# # Test metaFilePathLandsat()
# test_metaFilePathLandsat <- function(...){
#   for(i in seq(length(files))){
#     print(metaFilePathLandsat(files[i]))
#   }
# }
# 
# # Test landsatCoefficients()
# test_landsatCoefficients <- function(...){
#   test <- metaFilePathLandsat(files[3])
#   coefs <- landsatCoefficients(test[[1]])
#   # coefs$RADA[1:9] <- c(-6.97874, -7.19882, -5.62165, -6.06929, -1.12622, 
#   #                      -0.06709, 3.16280, -0.39390, -5.67559)
#   # coefs$RADM[1:9] <- c(0.779, 0.799, 0.622, 0.969, 0.126, 0.067, 0.037, 
#   #                      0.044, 0.976)
# }
# 
# 
# # Test landsatCalibration()
# test_landsatCalibration <- function(...){
#   for(i in seq(length(files))){
#     test <- metaFilePathLandsat(files[i])
#     coefs <- landsatCoefficients(test[[1]])
#     print(test[[2]])
#     result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "rad")  
#     print(result)
#     if(coefs$SOLAR[test[[2]]] == TRUE){
#       result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "ref")
#       print(result)
#       result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "refsun")
#       print(result)
#     } else {
#       result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "bt")
#       print(result)
#     }
#   }
# }
# 
# temp <- test_read()
# files <- temp[[1]]
# data <- temp[[2]]
# act_file <- files[1]
# act_data <- data[[2]]
# 
# test_metaFilePathLandsat()
# 
# test_landsatCoefficients()
# 
# test_landsatCalibration()
# 
