context("Solar irradiation (ESun)")

test_that("toaIrradianceModel works as expected", {
  toaIrradianceModel(lut$l8_rsr, model = "MNewKur")
  toaIrradianceModel(lut$l7_rsr, model = "MNewKur")
  toaIrradianceModel(lut$l7_rsr, model = "MNewKur", normalize = FALSE, 
                     date = "2015-01-01")
})

test_that("toaIrradianceTable works as expected", {
  toaIrradianceTable(sensor = "Landsat 5")
  toaIrradianceTable(sensor = "Landsat 7")
  toaIrradianceTable(sensor = "Landsat 7", normalize = FALSE, "2015-01-01")
})
