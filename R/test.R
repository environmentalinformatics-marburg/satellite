library(raster)
library(rgdal)
library(sp)

inpath_l7 <- 
  "D:/active/moc/am-remote-sensing/examples/data/landsat/l7_2001-07-30/"
files_l7 <- list.files(inpath_l7, pattern = glob2rx("*.TIF"), full.names = TRUE)

inpath_l8 <- 
  "D:/active/moc/am-remote-sensing/examples/data/landsat/l8_2013-07-07/"
files_l8 <- list.files(inpath_l8, pattern = glob2rx("*.TIF"), full.names = TRUE)
files_l8 <- files_l8[-12]

l <- 7
if(l == 8){
  files <- files_l8
  data <- stack(paste0(inpath_l8, "l8_2013-07-07_30m_crop.tif"))
} else {
  files <- files_l7
  data <- stack(paste0(inpath_l7, "l7_2001-07-30_30m_crop.tif"))
}
act_file <- files[1]
act_data <- data[[1]]


# Test metaFilePathLandsat()
for(i in seq(length(files))){
  print(metaFilePathLandsat(files[i]))
}

# Test landsatCoefficients()
test <- metaFilePathLandsat(files[3])
landsatCoefficients(test[[1]])

# Test landsatCalibration()
for(i in seq(length(files))){
  test <- metaFilePathLandsat(files[i])
  coefs <- landsatCoefficients(test[[1]])
  print(test[[2]])
  result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "rad")  
  print(result)
  if(coefs$SOLAR[test[[2]]] == TRUE){
    result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "ref")
    print(result)
    result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "refsun")
    print(result)
  } else {
    result <- landsatCalibration(data[[1]], test[[2]], coefs, conv = "bt")
    print(result)
  }
}
