if ( !isGeneric('plot') ) {
  setGeneric('plot', function(x, y, ...)
    standardGeneric('plot'))
}

#' Plot a 'Satellite' object
#' 
#' @description
#' This is the standard plotting routine for the 'Satellite' class. Layers are 
#' drawn either from the start (default; limited to a maximum of 16 sub-plots 
#' similar to \code{\link{raster::plot}}) or according to the speficied band 
#' codes.
#' 
#' @param x A 'Satellite' object, usually returned by \code{\link{satellite}}. 
#' @param bcde Band codes to be visualized. If not supplied, the initial (up to)
#' 16 layers are being visualized.
#' @param ... Further arguments passed on to \code{\link{plot}}.
#' 
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' sat_ps <- panSharp(sat)
#' 
#' plot(sat_ps, bcde = c("B001n", "B002n", "B003n"))
#' 
#' @export
#' @name plot
#' @rdname plot
#' @aliases plot,Satellite,ANY-method

# set methods -------------------------------------------------------------

setMethod('plot', signature(x = 'Satellite',
                            y = 'ANY'), 
          function(x,
                   bcde = NULL, 
                   ...) {
            
            ## visualize the initial (up to) 16 layers if 'bcde' is not specified
            if (is.null(bcde)) {
              ls_lyr <- getSatDataLayers(x)
              fc_bcde <- getSatBCDE(x)
              
              # par settings
              int_lyr_len <- length(ls_lyr)
              int_nrow <- int_ncol <- 1L
              if (int_lyr_len == 2) {
                int_ncol <- 2L
              } else if (int_lyr_len > 2 & int_lyr_len <= 4) {
                int_nrow <- int_ncol <- 2L
              } else if (int_lyr_len > 4 & int_lyr_len <= 6) {
                int_nrow <- 2L; int_ncol <- 3L
              } else if (int_lyr_len > 6 & int_lyr_len <= 9) {
                int_nrow <- int_ncol <- 3L
              } else if (int_lyr_len > 9 & int_lyr_len <= 12) {
                int_nrow <- 3L; int_ncol <- 4L
              } else if (int_lyr_len > 12) {
                int_nrow <- int_ncol <- 4L
                
                if (int_lyr_len > 16)
                  ls_lyr <- ls_lyr[1:16]
              }
              
              # visualize
              par(mfrow = c(int_nrow, int_ncol))
              for (i in 1:int_lyr_len) {
                plot(ls_lyr[[i]], ...)
                title(fc_bcde[i], line = 0)
              }
              
            ## visualize the specified bands only  
            } else {
              ls_lyr <- getSatDataLayer(x, bcde = bcde)
            }
        
            ## reset par settings
            par(mfrow = c(1, 1))
          })    