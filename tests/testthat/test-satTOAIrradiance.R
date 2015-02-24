context("satTOAIrradiance")

test_that("satTOAIrradianceTable works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradianceTable(sat)
})


test_that("satTOAIrradianceModel works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradianceModel(sat)
})

test_that("satTOAIrradianceRadRef works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradianceRadRef(sat)
})
