#' Get metadata filename from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function builds the metadata filename  based on standard level 1B/T 
#' Landsat filenames.
#'
#' @param filepath path and filename to the landsat band file (not the metadata)
#'
#' @return vector containing band as character [1] and metadata filepath [2]
#'
#' @export getInfoFromLevel1Name
#'
#' @examples
#' not run:
#' getInfoFromLevel1Name(filepath = "Name_of_Landsat_Band")

getInfoFromLevel1Name <- function(filepath){
  pos <- gregexpr(pattern ='_B', 
                  sub("(.+)[.][^.]+$", "\\1", 
                      basename(filepath)))[[1]][1]
  band <- substr(basename(filepath), pos + 2, 
                 nchar(sub("(.+)[.][^.]+$", "\\1", basename(filepath))))
  meta.filepath <- paste0(dirname(filepath), "/", 
                          substr(basename(filepath), 1, pos), 
                          "MTL.txt")
  result <- c(band, meta.filepath)
  attr(result, "Info") <- c("Band", "MetaFile")
  return(result)
}