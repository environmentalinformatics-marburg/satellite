setwd("D:/active/moc/am-remote-sensing/examples/data/landsat/l8_2013-07-07")

files <- list.files(".", pattern = glob2rx("*.TIF"), full.names = TRUE)

act_file <- files[1]

# Test metaFilePathLandsat()
for(i in seq(length(files))){
  print(metaFilePathLandsat(files[i]))
}

# Test landsatCoefficients()
test <- (metaFilePathLandsat(files[3]))
landsatCoefficients(test[1])

