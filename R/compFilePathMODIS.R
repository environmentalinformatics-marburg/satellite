
#' Get filename, bands and metadata file for Global Land Survey standard 1B/T format
#'
#' @description
#' The function compiles the sensor, band, filename and metadata filename information
#' for standard level 1B/T Global Land Survey (GLS) files.
#'
#' @param files Path and filename(s) of one or more GLS band files or, 
#' alternatively, one or more GLS metadata files. 
#'
#' @return \code{data.frame} containing filepaths, band numbers and metadata 
#' filepaths. 
#'
#' @export compFilePathMODIS
#'
#' @examples
#' \dontrun{
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' compFilePathMODIS(files)  
#' }
#' 
compFilePathMODIS <- function(files){
  if((length(files) == 1 & grepl("MTL", files[1])) == FALSE){
    info <- lapply(files, function(x){
      layer <- tools::file_path_sans_ext(basename(x))
      pos <- gregexpr(pattern ='_B', layer)[[1]][1]
      if(pos < 0){
        pos <- gregexpr(pattern ='_DEM', layer)[[1]][1]
        band_ids <- substr(basename(x), pos + 1, pos + 3)
      } else {
        band_ids <- substr(basename(x), pos + 2, pos + 2)
      }
      meta <- paste0(dirname(x), "/", substr(basename(x), 1, pos), "MTL.txt")
      sid <- substr(basename(x), 1, 2)
      
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
      
      name <- strsplit(basename(x), ".tif")[1]
      bid <- as.character(as.numeric(substr(name, nchar(name)-2, nchar(name))))
      sensor <- lutInfoSensorFromSID(sid)
      # band_code <- lutInfoBCDEFromBID(sid = sid)
      band_code <- lutInfoBCDEFromBID(sid = sid, bid = bid)
      data.frame(BCDE = as.character(band_code),
                 SID = sid, 
                 SENSOR = sensor,
                 BID = NA,
                 LAYER = NA,
                 FILE = x,
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