# devtools::test(".", "calcHistMatch")
context("calcHistMatch")


#-------------------------------------------------------------------------------
test_that("calcHistMatch for raster works as expected", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
  sat <- satellite(files)
  
  x <- getSatDataLayer(sat, "B002n")
  target <- getSatDataLayer(sat, "B003n")
})