context("toaIrradianceModel")

test_that("toaIrradianceModel works as expected", {
  toaIrradianceModel(sensor = "Landsat 7", model = "MNewKur")
  toaIrradianceModel(sensor = "Landsat 8", model = "MNewKur")
})
