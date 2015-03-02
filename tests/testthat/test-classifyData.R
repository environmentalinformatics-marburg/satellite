context("classifyData")

test_that("classifyData works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  training_sites <- lcs
  bands <- raster::stack(getSatDataLayer(sat, "B001n"), getSatDataLayer(sat, "B002n"),
                         getSatDataLayer(sat, "B003n"), getSatDataLayer(sat, "B004n"),
                         getSatDataLayer(sat, "B005n"), getSatDataLayer(sat, "B006n"),
                         getSatDataLayer(sat, "B007n"))
  classifyData(bands = bands, training_sites = training_sites)
})
