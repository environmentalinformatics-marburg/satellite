# devtools::test(".", "calcTOAIrrad")
context("Solar irradiation (ESun) using LUTs")

#-------------------------------------------------------------------------------
test_that("calcTOAIrradModel for 'data.frame' objects works as expected", {
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEarthSunDist("2015-01-01"))
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEarthSunDist("2015-07-07"))
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEarthSunDist("2015-01-01"))
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEarthSunDist("2015-07-07"))
})


#-------------------------------------------------------------------------------
test_that("calcTOAIrradModel for 'Satellite' objects works as expected", {
 
  ## loop over different collections (ie P1L1, C1L1)
  path <- c(system.file("extdata", package = "satellite"), 
            system.file("testdata/LC8", package = "satellite"))
  
  lapply(1:length(path), function(i) {
    sid <- ifelse(i == 1, "LC08", "LC8")
    files <- list.files(path[i], pattern = glob2rx(paste0(sid, "*.TIF")), full.names = TRUE)
    sat <- satellite(files)  
    test <- calcTOAIrradModel(sat)
    
    expect_equal(as.character(getSatBID(test)[1]), "1")
    expect_equal(round(as.numeric(getSatESUN(test)[1]),4), round(1888.4115033, 4))
    expect_equal(as.character(getSatBID(test)[2]), "2")
    expect_equal(round(as.numeric(getSatESUN(test)[2]),4), round(1974.8429354, 4))
    expect_equal(as.character(getSatBID(test)[3]), "3")
    expect_equal(round(as.numeric(getSatESUN(test)[3]),4), round(1851.7520559, 4))
    expect_equal(as.character(getSatBID(test)[11]), "11")
    expect_equal(round(as.numeric(getSatESUN(test)[11]),4), round(0.1068904, 4))
  })
})
