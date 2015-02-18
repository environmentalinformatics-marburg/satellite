#' Get metadata filename from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function builds the metadata filename  based on standard level 1B/T 
#' Landsat filenames.
#'
#' @param filepath path and filename to the landsat band file (not the metadata)
#'
#' @return Vector containing band as character [1] and metadata filepath [2]
#'
#' @export metaFilePathLandsat
#'
#' @examples
#' not run:
#' metaFilePathLandsat(filepath = "Name_of_Landsat_Band")

metaFilePathLandsat <- function(filepath){
  pos <- gregexpr(pattern ='_B', 
                  sub("(.+)[.][^.]+$", "\\1", 
                      basename(filepath)))[[1]][1]
  band <- substr(basename(filepath), pos + 2, 
                 nchar(sub("(.+)[.][^.]+$", "\\1", basename(filepath))))
  meta.filepath <- paste0(dirname(filepath), "/", 
                          substr(basename(filepath), 1, pos), 
                          "MTL.txt")
  result <- c(meta.filepath, band)
  attr(result, "Info") <- c("MetaFile", "Band")
  return(result)
}