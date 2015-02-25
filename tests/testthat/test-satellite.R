context("satellite")

test_that("satellite works as expected for Landsat 7  files", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  

  expect_equal(names(getSatLayers(sat)[[1]]), "LE71950252001211EDC00_B1")
  expect_equal(names(getSatLayers(sat)[[2]]), "LE71950252001211EDC00_B2")
  expect_equal(names(getSatLayers(sat)[[6]]), "LE71950252001211EDC00_B6_VCID_1")
  expect_equal(names(getSatLayers(sat)[[9]]), "LE71950252001211EDC00_B8")
  
  expect_equal(as.character(getSatMeta(sat)$BIDS[[1]]), "1")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[2]]), "2")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[6]]), "6_VCID_1")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[9]]), "8")
  
  expect_equal(as.character(getSatMeta(sat)$BCDE[[1]]), "001n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[2]]), "002n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[6]]), "0061")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[9]]), "008n")
})

test_that("satellite works as expected for Landsat 8 files", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  expect_equal(names(getSatLayers(sat)[[1]]), "LC81950252013188LGN00_B1")
  expect_equal(names(getSatLayers(sat)[[2]]), "LC81950252013188LGN00_B2")
  expect_equal(names(getSatLayers(sat)[[8]]), "LC81950252013188LGN00_B8")
  expect_equal(names(getSatLayers(sat)[[10]]), "LC81950252013188LGN00_B10")
  expect_equal(names(getSatLayers(sat)[[12]]), "LC81950252013188LGN00_BQA")
  
  expect_equal(as.character(getSatMeta(sat)$BIDS[[1]]), "1")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[2]]), "2")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[6]]), "6")
  expect_equal(as.character(getSatMeta(sat)$BIDS[[12]]), "QA")
  
  expect_equal(as.character(getSatMeta(sat)$BCDE[[1]]), "001n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[2]]), "002n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[6]]), "006n")
  expect_equal(as.character(getSatMeta(sat)$BCDE[[12]]), "0QAn")
})


test_that("satellite works as expected for stacks", {
  sat <- satellite(l8)
})