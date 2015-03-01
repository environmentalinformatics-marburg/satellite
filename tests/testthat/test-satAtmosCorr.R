context("satAtmosCorr")

test_that("satAtmosCorr works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  sat_atmos <- satAtmosCorr(sat, atmos_model = "DOS2", esun_mode = "RadRef")
  
  expect_equal(
    round(raster::getValues(
      getSatDataLayer(sat_atmos, bcde = "B002n_REF_AtmosCorr"))[50],4), 
    round(0.04668206, 4))
})






