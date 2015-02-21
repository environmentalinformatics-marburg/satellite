context("eSun")

test_that("eSun works as expected", {
  expect_equal(eSun(sensor = "Landsat 7", tab = TRUE), 
               c(1997.0, 1812.0, 1533.0, 1039.0, 230.8, NA, 84.9, 1362.0))
  
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")
  coefs8 <- landsatCoefficients(landsat8_metadatafile)
  eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8)
})
