#' Get filename, bands and metadata file for Landsat 7 and 8 standard 1B/T format
#'
#' @description
#' The function compiles the sensor, band, filename and metadata filename information
#' for standard level 1B/T Landsat files.
#'
#' @param files Path and filename(s) of one or more Landsat band files or, 
#' alternatively, one or more Landsat metadata files. 
#'
#' @return \code{data.frame} containing filepaths, band numbers and metadata 
#' filepaths. 
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
#' 
#' compFilePathLandsat(files)  
#' 
#' sortFilesLandsat(files)
#' sortFilesLandsat(files, id = TRUE) # indices
#' 
#' @export compFilePathLandsat
#' @name compFilePathLandsat
#' @rdname compFilePathLandsat
#' @aliases compFilePathLandsat
compFilePathLandsat <- function(files){
  if((length(files) == 1 & grepl("MTL", files[1])) == FALSE){
    info <- lapply(files, function(x){
      layer <- tools::file_path_sans_ext(basename(x))
      pos <- gregexpr(pattern ='_B', layer)[[1]][1]
      band_ids <- substr(basename(x), pos + 2, 
                         nchar(layer))
      meta <- paste0(dirname(x), "/", substr(basename(x), 1, pos), "MTL.txt")
      sid <- substr(basename(x), 1, 3)
      
      ## alternative sensor pattern
      if (sid %in% c("LC0", "LE0", "LT0")) {
        pttrn <- substr(basename(x), 1, 4)
        rid <- apply(lut$SENSOR_ID_PATTERN, 1, FUN = function(x) pttrn %in% x)
        sid <- lut$SENSOR_ID_PATTERN$SID[rid]
      }
      
      sensor <- lutInfoSensorFromSID(sid)
      band_code <- lutInfoBCDEFromBID(sid = sid, bid = band_ids)
      data.frame(SID = sid, 
                 SENSOR = sensor,
                 BID = band_ids,
                 BCDE = band_code,
                 LAYER = layer,
                 FILE = x,
                 CALIB = "SC",
                 METAFILE = meta,
                 stringsAsFactors = FALSE,
                 row.names = NULL)
    })
    result <- (do.call("rbind", info))
    return(result)
  } else {
    info <- lapply(files, function(x){
      sid <- substr(basename(x), 1, 3)
      
      ## alternative sensor pattern
      if (sid %in% c("LC0", "LE0", "LT0")) {
        pttrn <- substr(basename(x), 1, 4)
        rid <- apply(lut$SENSOR_ID_PATTERN, 1, FUN = function(x) pttrn %in% x)
        sid <- lut$SENSOR_ID_PATTERN$SID[rid]
      }
      
      sensor <- lutInfoSensorFromSID(sid)
      band_code <- lutInfoBCDEFromBID(sid = sid)
      
      ids = names(lut$BANDS) == sid
      bid = sapply(band_code, function(i) {
        tmp = grep(i, lut[[lut$BANDS[ids]]][, "BCDE"])
        lut[[lut$BANDS[ids]]][tmp, "BID"]
      })
      
      data.frame(BCDE = as.character(band_code),SID = sid, 
                 SENSOR = sensor,
                 BID = bid,
                 LAYER = NA,
                 FILE = NA,
                 CALIB = NA,
                 METAFILE = x,
                 stringsAsFactors = FALSE,
                 row.names = NULL)
    })
    result <- (do.call("rbind", info))
    rownames(result) <- NULL
    return(result)
  }
}


# Sort Landsat band files -----
#' @describeIn compFilePathLandsat Sort Landsat band files in ascending order.
#' @param id \code{logical}, defaults to \code{FALSE}. Determines whether to 
#' return sorted band files (ie default) or sorting order.
#' @return If \code{id = FALSE} (default), sorted band files as 
#' \code{character}, else the corresponding sorting order as \code{integer}.
#' @export sortFilesLandsat
sortFilesLandsat <- function(files, id = FALSE) {
  cfp <- compFilePathLandsat(files)
  bid <- suppressWarnings(as.integer(cfp$BID))
  
  if (id) {
    order(bid)
  } else {
    files[order(bid)]
  }
}
