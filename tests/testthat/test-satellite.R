context("satellite")

test_that("satellite works as expected", {
  landsat7_filepath <- system.file("extdata", 
                                   "LE71950252001211EDC00_B2.tif", 
                                   package = "satellite")
  landsat8_filepath <-   system.file("extdata", 
                                     "LC81950252013188LGN00_B1.tif", 
                                     package = "satellite")
  
  
  landsat8_filelist <- list.files("inst/extdata/", 
                                  pattern = glob2rx("LC8*.tif"), 
                                  full.names = TRUE)
  
  
})