context("Solar irradiation (ESun)")

test_that("calcTOAIrradianceModel works as expected", {
  calcTOAIrradianceModel(lut$L8_RSR, model = "MNewKur")
  calcTOAIrradianceModel(lut$L7_RSR, model = "MNewKur")
  calcTOAIrradianceModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                     date = "2015-01-01")
})

test_that("calcTOAIrradianceTable works as expected", {
  calcTOAIrradianceTable(sensor = "Landsat 5")
  calcTOAIrradianceTable(sensor = "Landsat 7")
  calcTOAIrradianceTable(sensor = "Landsat 7", normalize = FALSE, "2015-01-01")
})
