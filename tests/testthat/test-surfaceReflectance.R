context("surfaceReflectance")

test_that("surfaceReflectance works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  filesl8 <- list.files(path, 
                        pattern = glob2rx("LC8*.tif"), 
                        full.names = TRUE)
  
  coefs8 <- collectLandsat8Metadata(filesl8)
  
  ESun <-  eSun(sensor = "Landsat 8", tab = TRUE, rsr = lut$l8_rsr)
  Lp <- pathRadianceDOS(DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = lut$l8_band_wl, coefs = coefs8,
                        ESun = ESun, scat_coef = -4)
  
  bands <- l8
  for(i in seq(raster::nlayers(bands))){
    bands[[i]] <- landsatCalibration(bands[[i]], bnbr = i, coefs = coefs8)
  }
  
  sr <- surfaceReflectance(bands_rad = bands, coefs = coefs8, ESun = ESun, Lp = Lp)
  
})

