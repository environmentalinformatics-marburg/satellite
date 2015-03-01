context("satPathRadDOS")

test_that("satPathRadDOS works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  sat_pathrad <- satPathRadDOS(sat, atmos_model = "DOS2", esun_mode = "RadRef")
  
  expect_equal(round(getSatPRAD(sat_pathrad, bcde = "B002n"),3), round(c(B002n = 42.21805), 3))
  expect_equal(round(getSatPRAD(sat_pathrad, bcde = "B009n"),3), round(c(B009n = -0.1560062), 3))
})






