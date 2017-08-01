# devtools::test(".", "satellite")
context("satellite")


### Collection 1 Level-1 (C1L1) -----

## landsat 4, 5
test_that("satellite works as expected for Landsat 4 and 5 files (C1L1)", {
  path <- system.file("testdata/LT05", package = "satellite")
  files <- list.files(path, pattern = "^LT05.*.TIF$", full.names = TRUE)
  
  sat <- satellite(files)
})

## landsat 7
test_that("satellite works as expected for Landsat 7 files (C1L1)", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = "^LE07.*.TIF$", full.names = TRUE)
  
  sat <- satellite(files)
})

## landsat 8
test_that("satellite works as expected for Landsat 8 files (C1L1)", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = "^LC08.*.TIF$", full.names = TRUE)
  
  sat <- satellite(files)
})

## other input methods
test_that("satellite works as expected for 'Raster*' input (C1L1)", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = "^LC08.*.TIF$", full.names = TRUE)
  l8 <- raster::stack(files[-grep("_B8.TIF$", files)])
  sat <- satellite(l8)
})

test_that("satellite works as expected for 'list' input (C1L1)", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = "^LC08.*.TIF$", full.names = TRUE)
  files <- sortFilesLandsat(files)
  l8 <- lapply(files, raster)
  
  mtd <- compMetaLandsat(files)
  sat <- satellite(l8, meta = mtd)
})


### Pre-Collection Level-1 (P1L1) -----

## landsat 7
test_that("satellite works as expected for Landsat 7 files (P1L1)", {
  path <- system.file("testdata/LE7", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.TIF"), 
                      full.names = TRUE)
  
  sat <- satellite(files)

  expect_equal(basename(attr(getSatDataLayers(sat)[[1]], "file")@name), "LE71950252001211EDC00_B1.TIF")
  expect_equal(basename(attr(getSatDataLayers(sat)[[2]], "file")@name), "LE71950252001211EDC00_B2.TIF")
  expect_equal(basename(attr(getSatDataLayers(sat)[[6]], "file")@name), "LE71950252001211EDC00_B6_VCID_1.TIF")
  expect_equal(basename(attr(getSatDataLayers(sat)[[9]], "file")@name), "LE71950252001211EDC00_B8.TIF")
  
  expect_equal(as.character(getSatMeta(sat)$BID[[1]]), "1")
  expect_equal(as.character(getSatMeta(sat)$BID[[2]]), "2")
  expect_equal(as.character(getSatMeta(sat)$BID[[6]]), "6_VCID_1")
  expect_equal(as.character(getSatMeta(sat)$BID[[9]]), "8")
  
  expect_equal(as.character(getSatMeta(sat)$BCDE[[1]]), "B001n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[2]]), "B002n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[6]]), "B0061")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[9]]), "B008n")
})


## landsat 8 
test_that("satellite works as expected for Landsat 8 files (P1L1)", {
  path <- system.file("testdata/LC8", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.TIF"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  expect_equal(names(getSatDataLayers(sat)[[1]]), "B001n")
  expect_equal(names(getSatDataLayers(sat)[[2]]), "B002n")
  expect_equal(names(getSatDataLayers(sat)[[8]]), "B008n")
  expect_equal(names(getSatDataLayers(sat)[[10]]), "B010n")
  expect_equal(names(getSatDataLayers(sat)[[12]]), "B0QAn")
  
  expect_equal(as.character(getSatMeta(sat)$BID[[1]]), "1")
  expect_equal(as.character(getSatMeta(sat)$BID[[2]]), "2")
  expect_equal(as.character(getSatMeta(sat)$BID[[6]]), "6")
  expect_equal(as.character(getSatMeta(sat)$BID[[12]]), "QA")
  
  expect_equal(as.character(getSatMeta(sat)$BCDE[[1]]), "B001n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[2]]), "B002n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[6]]), "B006n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[12]]), "B0QAn")
})

## other input methods
test_that("satellite works as expected for stacks (P1L1)", {
  sat <- satellite(l8)
})

test_that("satellite works as expected for lists (P1L1)", {
  sat <- satellite(raster::unstack(l8))
})
