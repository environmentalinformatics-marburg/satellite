context("calcAtmosCorr")

test_that("calcAtmosCorr works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  sat <- satTOAIrradRadRef(sat)
  
  bcde <- "002n"
  path_rad <- calcPathRadDOS(DNmin = min(getValues(getSatDataLayer(sat, bcde))),
                             bnbr = getSatLNBR(sat, bcde),
                             band_wls = data.frame(LMIN = getSatLMIN(sat, getSatBCDESolar(sat)), 
                                                   LMAX = getSatLMAX(sat, getSatBCDESolar(sat))),
                             radm = getSatRADM(sat, getSatBCDESolar(sat)),
                             rada = getSatRADA(sat, getSatBCDESolar(sat)),
                             szen = getSatSZEN(sat, getSatBCDESolar(sat)),
                             esun = getSatESUN(sat, getSatBCDESolar(sat)),
                             model = "DOS2")
  
  sensor_rad <- calibLinear(band = getSatDataLayer(sat, bcde),
                            bnbr = 1,
                            mult = getSatRADM(sat, bcde),
                            add = getSatRADA(sat, bcde))
  
  ref_atmos <- calcAtmosCorr(sensor_rad = sensor_rad,
                             path_rad = path_rad[names(path_rad) == bcde],
                             esun = getSatESUN(sat, bcde),
                             szen = getSatSZEN(sat, bcde), 
                             model = "DOS2")
  
  expect_equal(round(raster::getValues(ref_atmos)[50],4), round(0.04668206, 4))
})
