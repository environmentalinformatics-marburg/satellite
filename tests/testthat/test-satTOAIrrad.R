context("satTOAIrrad")

test_that("satTOAIrradTable works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  satTOAIrradTable(sat)

  sat <- satellite(files[c(1:3, 6)])
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

  sat <- satellite(files[c(1,2,10)])  
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
  
  sat <- satellite(files[c(1,3,10)])  
  satTOAIrradRadRef(sat)
})
