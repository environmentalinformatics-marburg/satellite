#' Get metadata filename from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function builds the metadata filename  based on standard level 1B/T 
#' Landsat filenames.
#'
#' @param filepath path and filename to the landsat band file (not the metadata)
#'
#' @return List containing metadata filepath [[1]] and band number [[2]]
#'
#' @export metaFilePathLandsat
#'
#' @examples
#' landsat8_filepath <- "LC81950252013188LGN00_B2.TIF"
#' metaFilePathLandsat(filepath = landsat8_filepath)

metaFilePathLandsat <- function(filepath){
  pos <- gregexpr(pattern ='_B', 
                  sub("(.+)[.][^.]+$", "\\1", 
                      basename(filepath)))[[1]][1]
  band <- substr(basename(filepath), pos + 2, 
                 nchar(sub("(.+)[.][^.]+$", "\\1", basename(filepath))))
  meta.filepath <- paste0(dirname(filepath), "/", 
                          substr(basename(filepath), 1, pos), 
                          "MTL.txt")
  result <- list(meta.filepath, as.numeric(band))
  attr(result, "Info") <- c("MetaFile", "Bandnumber")
  return(result)
}