context("extend,Satellite-method")

path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
sat <- satellite(files)

## geographic extent of georg-gassmann-stadium (utm 32-n)
ext_ggs <- raster::extent(482606.4, 482781.4, 5627239, 5627489)
sat_ggs <- extend(sat, ext_ggs)

test_that("target object inherits larger spatial extent", {
  
  ## keep extended layers only, ie drop initial layers (default)
  expect_true(abs(raster::xmin(sat_ggs@layers[[1]]) - raster::xmin(ext_ggs)) <= 30)
  expect_true(abs(raster::ymin(sat_ggs@layers[[1]]) - raster::ymin(ext_ggs)) <= 30)

  ## append extended layers to initial 'Satellite' object
  sat_sbs <- extend(sat, ext_ggs, subset = FALSE)
  
  id1 <- length(sat@layers) + 1L
  expect_true(abs(raster::xmin(sat_sbs@layers[[id1]]) - raster::xmin(ext_ggs)) <= 30)
  expect_true(abs(raster::ymin(sat_sbs@layers[[id1]]) - raster::ymin(ext_ggs)) <= 30)

})

test_that("fill values for new cells are assigned correctly", {
  val_ggs <- raster::extract(sat_ggs@layers[[1]], ext_ggs)
  expect_true(all(is.na(val_ggs)))
  
  sat_ggs2 <- extend(sat, ext_ggs, value = -999)
  val_ggs2 <- raster::extract(sat_ggs2@layers[[1]], ext_ggs)
  expect_equal(unique(val_ggs2), -999)
})