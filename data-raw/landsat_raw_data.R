mask <- l8[[2]]

inpath <- "D:/active/moc/am-remote-sensing/examples/data/landsat/l8_2013-07-07"
files <- list.files(inpath, pattern = glob2rx("*.TIF"), full.names = TRUE)

lapply(files, function(x){
  print(x)
  writeRaster(crop(raster(x), mask), 
              filename = paste0("inst/extdata/",basename(x)), format = "GTiff")
})


inpath <- "D:/active/moc/am-remote-sensing/examples/data/landsat/l7_2001-07-30"
files <- list.files(inpath, pattern = glob2rx("*.TIF"), full.names = TRUE)

lapply(files, function(x){
  print(x)
  writeRaster(crop(raster(x), mask), 
              filename = paste0("inst/extdata/",basename(x)), format = "GTiff")
})
