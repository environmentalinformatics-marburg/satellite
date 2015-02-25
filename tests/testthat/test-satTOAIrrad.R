context("satTOAIrrad")

test_that("satTOAIrradTable works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradTable(sat)
})


test_that("satTOAIrradModel works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradModel(sat)
})

test_that("satTOAIrradRadRef works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  satTOAIrradRadRef(sat)
})
