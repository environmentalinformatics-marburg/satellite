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
#' @export compileFilePathLandsat
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' compileFilePathLandsat(files)  
#' 
compileFilePathLandsat <- function(files){
  info <- lapply(files, function(x){
    pos <- gregexpr(pattern ='_B', tools::file_path_sans_ext(basename(x)))[[1]][1]
    band_ids <- substr(basename(x), pos + 2, 
                       nchar(tools::file_path_sans_ext(basename(x))))
    meta <- paste0(dirname(x), "/", substr(basename(x), 1, pos), "MTL.txt")
    sid <- substr(basename(x), 1, 3)
    
    sensor <- lutInfoSensorFromSID(sid)
    band_code <- lutInfoBCDEFromBIDS(band_ids, sid)

    data.frame(SID = sid, 
               SENSOR = sensor,
               BIDS = band_ids,
               BCDE = band_code,
               FILE = x,
               METAFILE = meta)
  })
  result <- (do.call("rbind", info))
  rownames(result) <- NULL
  return(result)
}