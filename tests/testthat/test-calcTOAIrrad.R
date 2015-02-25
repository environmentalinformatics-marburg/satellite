context("Solar irradiation (ESun)")

test_that("calcTOAIrradModel works as expected", {
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                     date = "2015-01-01")
})

test_that("calcTOAIrradRadTable works as expected", {
  calcTOAIrradRadTable(sensor = "Landsat 5")
  calcTOAIrradRadTable(sensor = "Landsat 7")
  calcTOAIrradRadTable(sensor = "Landsat 7", normalize = FALSE, "2015-01-01")
})
