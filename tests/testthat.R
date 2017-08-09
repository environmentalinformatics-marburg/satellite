library(testthat)
library(satellite)


### functions to create test data sets -----

### Utility functions to quickly create test data sets for various sensors
### Florian Detsch, last modified on 2017-07-24

tst_obj <- function(type = c("LC08", "LC8", "LE07", "LE7", "LT05")) {
  if (type[1] == "LC08")
    tst_obj_lc08()
  else if (type[1] == "LC8")
    tst_obj_lc8()
  else if (type[1] == "LE07")
    tst_obj_le07()
  else if (type[1] == "LE7")
    tst_obj_le7()
  else if (type[1] == "LT05")
    tst_obj_lt05()
  else 
    stop("Sensor test data not available, yet.\n")
}


### collection 1 level-1 -----

## landsat 8
tst_obj_lc08 <- function() {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
  satellite(files)
}

## landsat 7
tst_obj_le07 <- function() {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LE07*.TIF"), full.names = TRUE)
  satellite(files)
}

## landsat 5
tst_obj_lt05 <- function() {
  path <- system.file("testdata/LT05", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LT05*.TIF"), full.names = TRUE)
  satellite(files)
}


### precollection level-1 -----

## landsat 8
tst_obj_lc8 <- function() {
  path <- system.file("testdata/LC8", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  satellite(files)
}

## landsat 7
tst_obj_le7 <- function() {
  path <- system.file("testdata/LE7", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LE7*.TIF"), full.names = TRUE)
  satellite(files)
}


### perform tests -----

test_check("satellite")
