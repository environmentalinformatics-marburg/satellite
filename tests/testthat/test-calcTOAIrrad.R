context("Solar irradiation (ESun)")

test_that("calcTOAIrradModel works as expected", {
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEartSunDist("2015-01-01"))
})

test_that("calcTOAIrradRadTable works as expected", {
  calcTOAIrradRadTable(sid = "LE5")
  calcTOAIrradRadTable(sid = "LE7")
  calcTOAIrradRadTable(sid = "LE7", normalize = FALSE, 
                       esd = calcEartSunDist("2015-01-01"))
})
