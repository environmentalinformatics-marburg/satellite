context("eSun")

test_that("eSun works as expected", {
  target <- c(1997.0, 1812.0, 1533.0, 1039.0, 230.8, NA, 84.9, 1362.0)
  names(target) <- paste0("Band_", seq(length(target)))
  expect_equal(eSun(sensor = "Landsat 7", tab = TRUE), target)

  target <- c(2038.6023, 1906.7442, 1601.6983, 1080.7457, 233.9853, NA, 84.8355, 
              1417.6534 )
  names(target) <- paste0("Band_", seq(length(target)))
  expect_equal(eSun(sensor = "Landsat 7", tab = FALSE, normalize = FALSE,
                    rsr = lut$l7_rsr, date = "2015-01-01"), target)
  
#   landsat8_metadatafile <- system.file("extdata", 
#                                        "LC81950252013188LGN00_MTL.txt", 
#                                        package = "satellite")
#   coefs8 <- landsatMetadata(landsat8_metadatafile)
#   eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8)
})
