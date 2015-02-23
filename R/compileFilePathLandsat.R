#' Get filename, bands and meta file for Landsat 7 and 8 standard 1B/T format.
#'
#' @description
#' The function builds the metadata filename  based on standard level 1B/T 
#' Landsat filenames.
#'
#' @param files path and filename of one or more landsat band file
#'
#' @return Data frame containing filepathes, band numbers and metafilepathes
#'
#' @export compileFilePathLandsat
#'
#' @examples
#' compileFilePathLandsat(files = c(inst/extdata/LC81950252013188LGN00_B1.tif,
#' inst/extdata/LC81950252013188LGN00_B10.tif))

compileFilePathLandsat <- function(files){
  info <- lapply(files, function(x){
    pos <- gregexpr(pattern ='_B', 
                    sub("(.+)[.][^.]+$", "\\1", 
                        basename(x)))[[1]][1]
    band <- substr(basename(x), pos + 2, 
                   nchar(sub("(.+)[.][^.]+$", "\\1", basename(x))))
    meta <- paste0(dirname(x), "/", 
                   substr(basename(x), 1, pos), 
                   "MTL.txt")
    sid <- substr(basename(x), 1, 3)
    sensor <- lut$sensors[which(names(lut$sensors) == sid)]
    data.frame(SENSOR = sensor,
               BAND = band,
               FILE = x,
               METAFILE = meta)
  })
  return(do.call("rbind", info))
}