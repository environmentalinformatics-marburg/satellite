context("landsatMetadata")

test_that("landsatMetadata works as expected", {
  landsat7_metadatafile <- system.file("extdata", 
                                       "LE71950252001211EDC00_MTL.txt", 
                                       package = "satellite")
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")

  # coefs7 <- landsatMetadata(landsat7_metadatafile)
  coefs8 <- landsatMetadata(landsat8_metadatafile)
  
  expect_output(class(coefs8), "data.frame")
  expect_equal(coefs8[1,5], 0.012147)

  # coefs$RADA[1:9] <- c(-6.97874, -7.19882, -5.62165, -6.06929, -1.12622, 
  #                      -0.06709, 3.16280, -0.39390, -5.67559)
  # coefs$RADM[1:9] <- c(0.779, 0.799, 0.622, 0.969, 0.126, 0.067, 0.037, 
  #                      0.044, 0.976)
})
