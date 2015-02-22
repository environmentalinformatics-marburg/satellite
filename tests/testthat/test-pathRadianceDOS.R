context("pathRadianceDOS")

test_that("pathRadianceDOS works as expected", {
#   landsat7_metadatafile <- system.file("extdata", 
#                                        "LE71950252001211EDC00_MTL.txt", 
#                                        package = "satellite")
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")
  
  # coefs7 <- landsatMetadata(landsat7_metadatafile)
  coefs8 <- landsatMetadata(landsat8_metadatafile)
  
  t1 <- pathRadianceDOS(DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
                        eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
                        scat_coef = -4)
  
  t2 <- pathRadianceDOS(sensor = "Landsat 8", 
                        DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
                        eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
                        scat_coef = -2)

  t3 <- pathRadianceDOS(sensor = "Landsat 8", 
                        DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
                        eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
                        scat_coef = -1)
  
  t4 <- pathRadianceDOS(sensor = "Landsat 8", 
                        DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
                        eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
                        scat_coef = -0.7)
  
  t5 <- pathRadianceDOS(sensor = "Landsat 8", 
                        DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
                        eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
                        scat_coef = -0.5)
  expect_equal(round(t1[1],4), c(Band_1 = round(59.8861832583,4)))
  expect_equal(round(t2[3],4), c(Band_3 = round(29.24265456,4)))
  expect_equal(round(t3[4],4), c(Band_4 = round(29.856403,4)))
  expect_equal(round(t4[5],4), c(Band_5 = round(28.156769,4)))
  expect_equal(round(t5[6],4), c(Band_6 = round(24.579342,4)))
  
  #   c(coef-4, coef-2, coef-1  coef-0.7  coef-0.5
  #   1  59.8861832583 50.01006637 45.566175 44.293677 43.460494
  #   2  41.2138869229 41.21388692 41.213887 41.213887 41.213887
  #   3  20.1958822971 29.24265456 34.915089 36.792434 38.092498
  #   4   9.2349957403 20.77494563 29.856403 33.175119 35.564547
  #   5   1.9468918394 11.78153912 23.199239 28.156769 31.982744
  #   6  -0.2445099344  3.48253659 13.136381 19.160386 24.579342
  #   7  -0.1021297427  1.98553024  9.853833 15.682434 21.341225
  #   8  17.2188865978 26.78113766 33.418198 35.724149 37.352906
  #   9  -0.2151743576  4.74046853 15.244152 21.225972 26.397953
  #   10 -0.0002675506  0.08882900  2.030244  5.183168  9.680882
  #   11 -0.0001497598  0.07333347  1.843295  4.843989  9.223823
})

