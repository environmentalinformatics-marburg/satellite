# devtools::test(".", "calcDODN")
context("calcDODN")

test_that("calcDODN works as expected", {
  ## precollection 
  path <- system.file("testdata/LC8", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  sat <- satellite(files)

  t1 <- calcDODN(getSatDataLayer(sat, bcde = "B002n"))
  t2 <- calcDODN(getSatDataLayer(sat, bcde = "B004n"))
  expect_equal(t1, 8763)
  expect_equal(t2, 6677)
  
  t1.1 <- calcDODN(sat, "B002n"); expect_equal(t1, t1.1)
  t2.1 <- calcDODN(sat, "B004n"); expect_equal(t2, t2.1)
  
  ## collection 1
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
  sat <- satellite(files)
  
  t1.2 <- calcDODN(sat, bcde = "B002n")
  t2.2 <- calcDODN(sat, bcde = "B004n")
})
