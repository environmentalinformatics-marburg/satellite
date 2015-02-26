#' Get filename, bands and meta file for Landsat 7 and 8 standard 1B/T format.
#'
#' @description
#' The function compiles the sensor, band, filename and metafilename information
#' for standard level 1B/T Landsat filenames.
#'
#' @param files path and filename of one or more landsat band file
#'
#' @return Data frame containing filepathes, band numbers and metafilepathes
#'
#' @export compFilePathLandsat
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' compFilePathLandsat(files)  
#' 
compFilePathLandsat <- function(files){
  info <- lapply(files, function(x){
    layer <- tools::file_path_sans_ext(basename(x))
    pos <- gregexpr(pattern ='_B', layer)[[1]][1]
    band_ids <- substr(basename(x), pos + 2, 
                       nchar(layer))
    meta <- paste0(dirname(x), "/", substr(basename(x), 1, pos), "MTL.txt")
    sid <- substr(basename(x), 1, 3)
    
    sensor <- lutInfoSensorFromSID(sid)
    band_code <- lutInfoBCDEFromBID(band_ids, sid)

    data.frame(SID = sid, 
               SENSOR = sensor,
               BID = band_ids,
               BCDE = band_code,
               LAYER = layer,
               FILE = x,
               METAFILE = meta,
               stringsAsFactors = FALSE)
  })
  result <- (do.call("rbind", info))
  rownames(result) <- NULL
  return(result)
}