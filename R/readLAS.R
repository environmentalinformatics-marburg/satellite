# library(rgdal)
# library(sp)
# 
# new_las_phb <- function(){
#   las_phb <- data.frame(
#     ITEM = c("File Signature ('LASF')",
#              "File Source ID",
#              "Global Encoding",
#              "Project ID - GUID data 1",
#              "Project ID - GUID data 2",
#              "Project ID - GUID data 3",
#              "Project ID - GUID data 4",
#              "Version Major",
#              "Version Minor",
#              "System Identifier",
#              "Generating Software",
#              "File Creation Day of Year",
#              "File Creation Year",
#              "Header Size",
#              "Offset to point data",
#              "Number of Variable Length Records",
#              "Point Data Record Format",
#              "Point Data Record Length",
#              "Legacy Number of point records",
#              "Legacy Number of points by return",
#              "X scale factor",
#              "Y scale factor",
#              "Z scale factor",
#              "X offset",
#              "Y offset",
#              "Z offset",
#              "Max X",
#              "Min X",
#              "Max Y",
#              "Min Y",
#              "Max Z",
#              "Min Z",
#              "Start of Waveform Data Packet Record",
#              "Start of first Extended Variable Length Record",
#              "Number of Extended Variable Length Records",
#              "Number of point records",
#              "Number of points by return"),
#     FORMAT = c("char[4]",
#                "unsigned short",
#                "unsigned short",
#                "unsigned long",
#                "unsigned short",
#                "unsigned short",
#                "unsigned char[8]",
#                "unsigned char",
#                "unsigned char",
#                "char[32]",
#                "char[32]",
#                "unsigned short",
#                "unsigned short",
#                "unsigned short",
#                "unsigned long",
#                "unsigned long",
#                "unsigned char",
#                "unsigned short",
#                "unsigned long",
#                "unsigned long [5]",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "double",
#                "unsigned long long",
#                "unsigned long long",
#                "unsigned long",
#                "unsigned long long",
#                "unsigned long long [15]"),
#     SIZE = c("4 bytes",
#              "2 bytes",
#              "2 bytes",
#              "4 bytes",
#              "2 byte",
#              "2 byte",
#              "8 bytes",
#              "1 byte",
#              "1 byte",
#              "32 bytes",
#              "32 bytes",
#              "2 bytes",
#              "2 bytes",
#              "2 bytes",
#              "4 bytes",
#              "4 bytes",
#              "1 byte",
#              "2 bytes",
#              "4 bytes",
#              "20 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "8 bytes",
#              "4 bytes",
#              "8 bytes", 
#              "120 bytes"),
#     REQUIRED = c("* ",
#                  "* ",
#                  "* ",
#                  "",
#                  "",
#                  "",
#                  "",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ",
#                  "* ")
#   )
#   
#   las_phb$WHAT <- ""
#   las_phb$WHAT[grep("unsigned", las_phb$FORMAT)] <- "integer"
#   las_phb$WHAT[grep("char", las_phb$FORMAT)] <- "raw"
#   las_phb$WHAT[grep("double", las_phb$FORMAT)] <- "numeric"
#   
#   las_phb$SIGNED <- TRUE
#   las_phb$SIGNED[grep("unsigned short", las_phb$FORMAT)] <- FALSE
#   las_phb$SIGNED[grep("unsigned long", las_phb$FORMAT)] <- FALSE
#   
#   las_phb$N <- as.numeric(gsub("[[:alpha:][:punct:]]", "", las_phb$FORMAT))
#   las_phb$N[las_phb$WHAT == "character"] <- 1
#   #las_phb$N[las_phb$WHAT == "raw"] <- 1
#   las_phb$N[is.na(las_phb$N)] <- 1
#   
#   las_phb$BYTES <- as.numeric(gsub("[[:alpha:]]", "", las_phb$SIZE))
#   
#   las_phb$READSIZE <- las_phb$BYTES / las_phb$N
#   
#   las_phb$WHAT[grep("unsigned char", las_phb$FORMAT)] <- "integer"
#   
#   
#   #   las_phb$N[las_phb$WHAT == "raw"] <- las_phb$READSIZE[las_phb$WHAT == "raw"]
#   #   las_phb$READSIZE[las_phb$WHAT == "raw"] <- 1
#   
#   return(las_phb)
# }
# 
# 
# # Read LAS file
# inpath_lidar <- "D:/active/moc/am-remote-sensing/examples/data/lidar/"
# las <- file(paste0(inpath_lidar, "U4805626.las"), "rb")
# 
# seek(las)
# 
# las_phb <- new_las_phb()
# # las_phb <- las_phb[-c(33:37),]
#     
# header <- lapply(seq(nrow(las_phb)), function(x){
#   if(las_phb$WHAT[x] == "raw"){
#     bytes <- readBin(las, las_phb$WHAT[x], n = las_phb$N[x], 
#                      size = las_phb$READSIZE[x], signed = las_phb$SIGNED[x], 
#                      endian = "little")
#     entry <- readBin(bytes, "character", size = las_phb$BYTES[x], 
#                      endian = "little")
#   } else {
#     entry <- readBin(las, las_phb$WHAT[x], n = las_phb$N[x], 
#                      size = las_phb$READSIZE[x], signed = las_phb$SIGNED[x], 
#                      endian = "little")
#   }
#   return(entry)
# })
# close(las)
# names(header) <- las_phb$ITEM
# if(header$`Version Minor` < 4){
#   header <- header[1:32]
# }
# header
# 
# 
# numberPointRecords <- header[["Legacy Number of point records"]]
# offsetToPointData <- header[["Offset to point data"]]
# pointDataRecordLength <-header[["Point Data Record Length"]]
# xyzScaleOffset <- cbind(unlist(header[c("X scale factor", "Y scale factor", "Z scale factor")]),
#                         unlist(header[c("X offset", "Y offset", "Z offset")]))
# 
# 
# # Read data
# las <- file(paste0(inpath_lidar, "U4805626.las"), "rb")
# junk <- readBin(las, "raw", n = offsetToPointData, size = 1)
# 
# skip <- 0
# nrows <- NULL
# 
# ## deal with rows to skip and max rows to be read
# if (skip > 0) {
#   junk <- readBin(con, "raw", size = 1, n = pointDataRecordLength * skip)
#   numberPointRecords <- numberPointRecords - skip
# }
# if (!is.null(nrows)) {
#   if (numberPointRecords > nrows) numberPointRecords <- nrows
# }
# 
# if (numberPointRecords < 1) stop("no records left to read")
# 
# allbytes <- matrix(readBin(las, "raw", n = pointDataRecordLength * numberPointRecords, size = 1, endian = "little"),
#                    ncol= pointDataRecordLength, nrow = numberPointRecords, byrow = TRUE)
# close(las)
# 
# gpstime <- NULL
# if(ncol(allbytes) == 28){
#   gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")
# } 
# 
# 
# mm <- matrix(readBin(t(allbytes[,1:(3*4)]), "integer", size = 4, n = 3 * numberPointRecords, endian = "little"), ncol = 3, byrow = TRUE)
# mm[,1] <- mm[ ,1] * xyzScaleOffset[1,1] + xyzScaleOffset[1, 2]
# mm[,2] <- mm[ ,2] * xyzScaleOffset[2,1] + xyzScaleOffset[2, 2]
# mm[,3] <- mm[ ,3] * xyzScaleOffset[3,1] + xyzScaleOffset[3, 2]
# 
# intensity <- readBin(t(allbytes[, 13:14]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
# classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
# 
# mmtest <- data.frame(X = mm[,1],
#                      Y = mm[,2],
#                      Z = mm[,3],
#                      INT = intensity,
#                      CLASS = classification)
# 
# rm(allbytes, gpstime, mm, intensity, classification)
# 
# mmtest13 <- mmtest[mmtest$INT == 13,]
# mmtest13_sp <- SpatialPoints(mmtest13)
# coordinates(mmtest13) <- ~X + Y
# mmtest13@proj4string <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +north")
# spplot(mmtest13)
# 
# 
# mmtest02 <- mmtest[mmtest$INT == 2,]
# mmtest13_sp <- SpatialPoints(mmtest13)
# coordinates(mmtest13) <- ~X + Y
# mmtest13@proj4string <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +north")
# spplot(mmtest13)
# 
# 
# sort( sapply(ls(),function(x){object.size(get(x))}))    
# 
# #http://staff.acecrc.org.au/~mdsumner/las/readLAS.R
